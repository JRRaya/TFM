# ============================================================
# TIDYMODELS PIPELINE
# Modelos:
#   - GLM Logístico
#   - Elastic Net (glmnet)
#   - GAM (mgcv)
#   - Random Forest (ranger)
#   - XGBoost
# ============================================================

rm(list = ls(all.names = TRUE))
gc()

pacman::p_load(
  tidymodels,
  workflowsets,
  finetune,
  doFuture,
  future,
  DALEXtra,
  DALEX,
  vip,
  bonsai,
  mgcv,
  dplyr,
  ggplot2,
  here,
  ranger,
  xgboost,
  glmnet
)

# ------------------------------------------------------------
# CONFIGURACIÓN
# ------------------------------------------------------------

SEED <- 123

set.seed(SEED)

doFuture::registerDoFuture()
future::plan(multisession, workers = parallel::detectCores() - 1)

# ------------------------------------------------------------
# CARGA DE DATOS
# ------------------------------------------------------------

dataset <- readRDS(
  here::here("data/modelos/dataset.rds")
)

target <- "target"

dataset[[target]] <- as.factor(dataset[[target]])

# ------------------------------------------------------------
# RESAMPLING
# ------------------------------------------------------------

folds <- vfold_cv(
  dataset,
  v = 10,
  repeats = 20,
  strata = all_of(target)
)

# ------------------------------------------------------------
# RECIPE
# ------------------------------------------------------------

rec <- recipe(
  as.formula(paste(target, "~ .")),
  data = dataset
)

# ------------------------------------------------------------
# MÉTRICAS EMA
# ------------------------------------------------------------

metricas <- metric_set(
  roc_auc,
  pr_auc,
  accuracy,
  bal_accuracy,
  sens,
  spec,
  f_meas,
  mn_log_loss,
  brier_class
)

# ------------------------------------------------------------
# MODELOS
# ------------------------------------------------------------

# 1. GLM
glm_spec <-
  logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

# 2. Elastic Net
enet_spec <-
  logistic_reg(
    penalty = tune(),
    mixture = tune()
  ) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

# 3. GAM
gam_spec <-
  gen_additive_mod(
    adjust_deg_free = tune(),
    select_features = TRUE
  ) %>%
  set_engine(
    "mgcv",
    method = "REML"
  ) %>%
  set_mode("classification")

# 4. Random Forest
rf_spec <-
  rand_forest(
    trees = 2000,
    mtry = tune(),
    min_n = tune()
  ) %>%
  set_engine(
    "ranger",
    importance = "permutation"
  ) %>%
  set_mode("classification")

# 5. XGBoost
xgb_spec <-
  boost_tree(
    trees = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),
    min_n = tune(),
    sample_size = tune(),
    mtry = tune()
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# ------------------------------------------------------------
# WORKFLOWS
# ------------------------------------------------------------

glm_wf  <- workflow() %>% add_recipe(rec) %>% add_model(glm_spec)
enet_wf <- workflow() %>% add_recipe(rec) %>% add_model(enet_spec)
gam_wf  <- workflow() %>% add_recipe(rec) %>% add_model(gam_spec)
rf_wf   <- workflow() %>% add_recipe(rec) %>% add_model(rf_spec)
xgb_wf  <- workflow() %>% add_recipe(rec) %>% add_model(xgb_spec)

# ------------------------------------------------------------
# GRIDS
# ------------------------------------------------------------

enet_grid <- grid_regular(
  penalty(range = c(-5, 1)),
  mixture(),
  levels = c(40, 11)
)

gam_grid <- tibble(
  adjust_deg_free = c(
    0.5,
    1,
    2,
    4,
    8
  )
)

rf_grid <- grid_latin_hypercube(
  finalize(
    mtry(),
    dataset[, setdiff(names(dataset), target)]
  ),
  min_n(),
  size = 50
)

xgb_grid <- grid_latin_hypercube(
  trees(range = c(500L, 3000L)),
  tree_depth(range = c(2L, 10L)),
  learn_rate(range = c(-4, -1)),
  loss_reduction(),
  min_n(),
  sample_prop(),
  finalize(
    mtry(),
    dataset[, setdiff(names(dataset), target)]
  ),
  size = 100
)

# ------------------------------------------------------------
# CONTROL
# ------------------------------------------------------------

ctrl <- control_race(
  save_pred = TRUE,
  save_workflow = TRUE,
  parallel_over = "everything",
  verbose_elim = TRUE
)

# ------------------------------------------------------------
# ENTRENAMIENTO
# ------------------------------------------------------------

glm_fit <- fit_resamples(
  glm_wf,
  resamples = folds,
  metrics = metricas,
  control = control_resamples(save_pred = TRUE)
)

enet_fit <- tune_race_anova(
  enet_wf,
  resamples = folds,
  grid = enet_grid,
  metrics = metricas,
  control = ctrl
)

gam_fit <- tune_race_anova(
  gam_wf,
  resamples = folds,
  grid = gam_grid,
  metrics = metricas,
  control = ctrl
)

rf_fit <- tune_race_anova(
  rf_wf,
  resamples = folds,
  grid = rf_grid,
  metrics = metricas,
  control = ctrl
)

xgb_fit <- tune_race_anova(
  xgb_wf,
  resamples = folds,
  grid = xgb_grid,
  metrics = metricas,
  control = ctrl
)

# ------------------------------------------------------------
# RESULTADOS
# ------------------------------------------------------------

glm_metrics  <- collect_metrics(glm_fit)
enet_metrics <- collect_metrics(enet_fit)
gam_metrics  <- collect_metrics(gam_fit)
rf_metrics   <- collect_metrics(rf_fit)
xgb_metrics  <- collect_metrics(xgb_fit)

resultados <- bind_rows(
  mutate(glm_metrics, modelo = "GLM"),
  mutate(enet_metrics, modelo = "ElasticNet"),
  mutate(gam_metrics, modelo = "GAM"),
  mutate(rf_metrics, modelo = "RF"),
  mutate(xgb_metrics, modelo = "XGB")
)

write.csv(
  resultados,
  here::here("resultados_modelos.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# SELECCIÓN FINAL
# ------------------------------------------------------------

best_rf <- select_best(rf_fit, metric = "roc_auc")
best_xgb <- select_best(xgb_fit, metric = "roc_auc")
best_enet <- select_best(enet_fit, metric = "roc_auc")
best_gam <- select_best(gam_fit, metric = "roc_auc")

# ------------------------------------------------------------
# AJUSTE FINAL DEL GANADOR
# ------------------------------------------------------------

final_xgb_wf <- finalize_workflow(
  xgb_wf,
  best_xgb
)

final_xgb_fit <- fit(
  final_xgb_wf,
  dataset
)

saveRDS(
  final_xgb_fit,
  here::here("modelo_final_xgb.rds")
)

# ------------------------------------------------------------
# EXPLICABILIDAD EMA / DALEX
# ------------------------------------------------------------

X <- dataset[, setdiff(names(dataset), target)]
y <- dataset[[target]]

explainer <- explain_tidymodels(
  final_xgb_fit,
  data = X,
  y = y,
  label = "XGBoost"
)

vip <- model_parts(
  explainer,
  loss_function = loss_one_minus_auc,
  B = 50
)

saveRDS(
  vip,
  here::here("vip_xgb.rds")
)

plot(vip)

# ============================================================
# FIN
# ============================================================
