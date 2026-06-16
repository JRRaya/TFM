# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
gc(verbose = FALSE, full = TRUE)
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(caret, glmnet, doFuture, future, future.apply, dplyr, here, ggplot2, gtsummary, arm, gt)

# 1.3. Configuración de reproducibilidad paralela
RNGkind("L'Ecuyer-CMRG")
set.seed(123)

# 1.4. Configurar paralelismo
doFuture::registerDoFuture()
future::plan(multisession, workers = 4L)

# 2. Función GLM
glm_model <- function(dataset, ecuation, ruta, nombre) {
  # 1. Definir la ecuación del modelo
  formula <- as.formula(ecuation)

  # 2. GLM
  # 2.1. Definir tuning
  tune <- base::expand.grid(
    alpha = seq(0, 1, by = 0.1),
    lambda = 10^seq(-5, 0.5, length = 250)
  )

  # 2.2. Definir semilla
  semillas <- c(
    lapply(1:(10 * 20), function(x) sample.int(10000, nrow(tune) + 1, replace = TRUE)),
    list(sample.int(10000, 1))
  )
      
  # 2.3. Entrenar modelo
  glm <- caret::train(
    formula,
    data = dataset,
    tuneGrid = tune,
    method = "glmnet",
    family = "binomial",
    metric = "ROC",
    trControl = caret::trainControl(
      method = "repeatedcv",
      number = 10,
      repeats = 20,
      classProbs = TRUE,
      summaryFunction = twoClassSummary,
      sampling = "down",
      seeds = semillas,
      savePredictions   = "final",
      selectionFunction = "oneSE",
      allowParallel = TRUE
    )
  )

  # 2.4. Tabla resumen
  tabla <- coef(glm$finalModel, s = glm$bestTune$lambda) %>%
    as.matrix() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Variable") %>%
    dplyr::mutate(Coeficiente = as.numeric(.[[2]])) %>%
    dplyr::select(Variable, Coeficiente) %>% 
    dplyr::mutate(
      OR        = exp(Coeficiente),
      Shrinkage = dplyr::if_else(Coeficiente == 0, "Eliminada (L1)", "Retenida")
    ) %>%
    dplyr::filter(Variable != "(Intercept)") %>%
    dplyr::arrange(dplyr::desc(abs(Coeficiente))) %>%
    dplyr::cross_join(
      glm$results %>%
        dplyr::filter(
          alpha  == glm$bestTune$alpha,
          lambda == glm$bestTune$lambda
        ) %>%
        dplyr::select(Accuracy, Kappa, AccuracySD, KappaSD) %>%
        dplyr::slice(1)
    ) %>%
    gt::gt() %>%
    gt::tab_header(
      title    = paste0("Modelo GLM logístico – ", nombre),
      subtitle = paste0(
        "alpha = ",  round(glm$bestTune$alpha, 2),
        " | lambda = ", round(glm$bestTune$lambda, 5)
      )
    ) %>%
    gt::fmt_number(
      columns  = c(Coeficiente, OR, Accuracy, Kappa, AccuracySD, KappaSD),
      decimals = 4
    ) %>%
    gt::cols_label(
      Variable    = "Variable",
      Coeficiente = "Coef. (log-odds)",
      OR          = "Odds Ratio",
      Shrinkage   = "Estado",
      Accuracy    = "Accuracy (CV)",
      AccuracySD  = "±SD Accuracy",
      Kappa       = "Kappa (CV)",
      KappaSD     = "±SD Kappa"
    ) %>%
    gt::tab_spanner(
      label   = "Coeficientes del modelo",
      columns = c(Variable, Coeficiente, OR, Shrinkage)
    ) %>%
    gt::tab_spanner(
      label   = "Métricas de validación cruzada",
      columns = c(Accuracy, AccuracySD, Kappa, KappaSD)
    ) %>%
    gt::tab_style(
      style     = gt::cell_fill(color = "#fff3cd"),
      locations = gt::cells_body(rows = Shrinkage == "Eliminada (L1)")
    ) %>%
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) %>%
    gt::opt_row_striping() %>%
    gt::opt_table_font(font = gt::google_font("IBM Plex Sans"))

  # 2.5. Residuos
  pred_prob <- predict(glm, newdata = dataset, type = "prob")[, 2]
  y_num     <- as.numeric(dataset[[all.vars(formula)[1]]]) - 1
  residuos  <- y_num - pred_prob

  png(
    filename = here::here(paste0(ruta, nombre, "_binnedplot.png")),
    width = 800, height = 600, res = 120
  )
  arm::binnedplot(
    x    = pred_prob,
    y    = residuos,
    xlab = "Probabilidad predicha",
    ylab = "Residuo promedio",
    main = paste0("Binned residual plot – ", nombre)
  )
  dev.off()

  # 3. Guardado
  # 3.1. Modelo
  base::saveRDS(
    object = glm,
    file = here::here(paste0(ruta, nombre, "_glm.RDS"))
  )

  # 3.2. Tabla
  gt::gtsave(
    tabla,
    filename = here::here(paste0(ruta, nombre, "_tabla_resumen.html"))
  )
}

# 3. Definir datasets
datasets <- list(
  list(
    dataset  = readRDS(here::here("data/modelos/normalizacion/df_2008_2012.rds")),
    ecuation = "diferencia_2008_2012_rast ~ .",
    ruta     = "data/modelos/glm/2008_2012/",
    nombre   = "diferencia_2008_2012"
  ),
  list(
    dataset  = readRDS(here::here("data/modelos/normalizacion/df_2008_2023.rds")),
    ecuation = "diferencia_2008_2023_rast ~ .",
    ruta     = "data/modelos/glm/2008_2023/",
    nombre   = "diferencia_2008_2023"
  ),
  list(
    dataset  = readRDS(here::here("data/modelos/normalizacion/df_2015_2023.RDS")),
    ecuation = "diferencia_2015_2023_rast ~ .",
    ruta     = "data/modelos/glm/2015_2023/",
    nombre   = "diferencia_2015_2023"
  ),
  list(
    dataset  = readRDS(here::here("data/modelos/normalizacion/df_2020_2023.RDS")),
    ecuation = "diferencia_2020_2023_rast ~ .",
    ruta     = "data/modelos/glm/2020_2023/",
    nombre   = "diferencia_2020_2023"
  )
)

# 4. Crear carpetas de salida
sapply(
  datasets, 
  function(m) {
    dir.create(here::here(m$ruta), recursive = TRUE, showWarnings = FALSE)
  }
)

# 5. Ejecutar modelos
future.apply::future_lapply(datasets, function(m) {
  glm_model(
    dataset = m$dataset,
    ecuation = m$ecuation,
    ruta     = m$ruta,
    nombre   = m$nombre
  )
}, future.seed = TRUE)

# 6. Cerrar paralelismo
future::plan(sequential)