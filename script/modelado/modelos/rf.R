# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
gc(verbose = FALSE, full = TRUE)
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(caret, ranger, DALEX, doFuture, future, future.apply, dplyr, tidyr, here, ggplot2, scales, patchwork, yardstick)

# 2. Función RF
rf_model <- function(dataset, ecuation, ruta, nombre, seed = 123) {

  # 2.1. Configuración previa
  # 2.1.1. Configurar reproducibilidad bajo paralelismo
  set.seed(seed, kind = "L'Ecuyer-CMRG")

  # 2.1.2. Configurar paralelismo
  doFuture::registerDoFuture()
  future::plan(multisession, workers = 15L)

  # 2.1.3. Definir comportamiento del paralelismo al terminar el entrenamiento
  on.exit(future::plan(sequential), add = TRUE)

  # 2.1.4. Definir la ecuación del modelo
  formula  <- as.formula(ecuation)
  resp_var <- all.vars(formula)[1]

  # 2.2. RF
  # 2.2.1. Definir grid de hiperparámetros
  tune <- expand.grid(
    mtry          = 2:max(2, ncol(dataset) - 1),
    splitrule     = c("gini", "extratrees"),
    min.node.size = 1:15
  )

  # 2.2.2. Definir semillas a partir del grid
  semillas <- c(
    lapply(
      1:200,
      function(x) sample.int(1000, nrow(tune))
    ),
    list(sample.int(1000, 1))
  )

  # 2.2.3. Entrenar modelo
  rf <- caret::train(
    formula,
    data             = dataset,
    method           = "ranger",
    metric           = "ROC",
    num.trees        = 2000,
    importance       = "permutation",
    local.importance = TRUE,
    num.threads      = 1,
    tuneGrid         = tune,
    trControl        = caret::trainControl(
      method            = "repeatedcv",
      number            = 10,
      repeats           = 20,
      classProbs        = TRUE,
      summaryFunction   = twoClassSummary,
      seeds             = semillas,
      savePredictions   = "final",
      selectionFunction = "oneSE",
      allowParallel     = TRUE,
      verboseIter       = TRUE
    )
  )

  # 2.3. Guardado
  # 2.3.1. Modelo
  base::saveRDS(
    object = rf,
    file   = here::here(paste0(ruta, nombre, "_rf.rds"))
  )

  # 2.3.2. Configuraciones estéticas compartidas
  tema_base <- ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle    = ggplot2::element_text(color = "grey40", size = 9),
      axis.title       = ggplot2::element_text(size = 10),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "grey92"),
      plot.margin      = ggplot2::margin(12, 16, 12, 12)
    )

  # 2.3.3. Importancia de variables – ranger (interno)
  imp_df <- rf$finalModel$variable.importance.local |>
    as.data.frame() |>
    tidyr::pivot_longer(
      cols      = everything(),
      names_to  = "Variable",
      values_to = "imp"
    ) |>
    dplyr::summarise(
      Mean = mean(imp),
      SD   = sd(imp),
      .by  = Variable
    ) |>
    dplyr::arrange(Mean) |>
    dplyr::mutate(
      Variable = factor(Variable, levels = Variable),
      ymin     = Mean - SD,
      ymax     = Mean + SD
    )

  ggplot2::ggsave(
    filename  = here::here(paste0(ruta, nombre, "_rf_var_imp_ranger.pdf")),
    plot      = ggplot2::ggplot(imp_df, ggplot2::aes(x = Variable, y = Mean)) +
      ggplot2::geom_col(width = 0.65, fill = "grey50") +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = ymin, ymax = ymax),
        width     = 0.30,
        linewidth = 0.5,
        color     = "grey20"
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          y     = ymax,
          label = formatC(Mean, format = "g", digits = 3)
        ),
        hjust = -1.1,
        size  = 2.5,
        color = "grey30"
      ) +
      ggplot2::coord_flip(clip = "off") +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0.05, 0.30))
      ) +
      ggplot2::labs(
        title    = nombre,
        subtitle = "Permutation importance (ranger) · media ± SD por observación",
        x        = NULL,
        y        = "Importancia (reducción media de exactitud)"
      ) +
      tema_base,
    width     = 8,
    height    = max(4, nrow(imp_df) * 0.30),
    units     = "in",
    limitsize = FALSE
  )

  # 2.3.4. Importancia de variables – DALEX (agnóstica)
  explainer  <- DALEX::explain(
    model            = rf,
    data             = dataset[, setdiff(names(dataset), resp_var)],
    y                = as.numeric(dataset[[resp_var]]) - 1L,
    predict_function = function(model, newdata) {
      predict(model, newdata = newdata, type = "prob")[, 2L]
    },
    label   = nombre,
    verbose = FALSE
  )

  # 2.3.5. Cálculo de importancia por permutación (B = 50, loss = 1 - AUC)
  set.seed(seed)
  vip_ema <- DALEX::model_parts(
    explainer     = explainer,
    loss_function = DALEX::loss_one_minus_auc,
    B             = 50,
    type          = "difference"
  )

  base::saveRDS(
    object = vip_ema,
    file   = here::here(paste0(ruta, nombre, "_vip_ema.rds"))
  )

  ggplot2::ggsave(
    filename  = here::here(paste0(ruta, nombre, "_rf_var_imp_ema.pdf")),
    plot      = plot(vip_ema) +
      ggplot2::ggtitle(
        label    = nombre,
        subtitle = "Permutation variable importance · 1-AUC · 50 permutaciones (DALEX)"
      ) +
      tema_base +
      ggplot2::theme(legend.position = "bottom"),
    width     = 8,
    height    = max(4, length(unique(vip_ema$variable)) * 0.32),
    units     = "in",
    limitsize = FALSE
  )

  # 2.3.6. Tuning de hiperparámetros
  ggplot2::ggsave(
    filename = here::here(paste0(ruta, nombre, "_rf_tuning.pdf")),
    plot     = rf$results |>
      dplyr::mutate(
        min.node.size = factor(min.node.size),
        splitrule     = factor(splitrule)
      ) |>
      ggplot2::ggplot(ggplot2::aes(
        x        = mtry,
        y        = ROC,
        color    = min.node.size,
        group    = interaction(min.node.size, splitrule),
        linetype = splitrule
      )) +
      ggplot2::geom_line(linewidth = 0.7, alpha = 0.8) +
      ggplot2::geom_point(size = 1.8) +
      ggplot2::geom_point(
        data = rf$results |>
          dplyr::mutate(
            min.node.size = factor(min.node.size),
            splitrule     = factor(splitrule)
          ) |>
          dplyr::slice(which.max(ROC)),
        ggplot2::aes(x = mtry, y = ROC),
        shape = 21, size = 4, fill = "white", color = "#a50026", stroke = 1.5
      ) +
      ggplot2::scale_color_viridis_d(option = "turbo", name = "min.node.size") +
      ggplot2::scale_linetype_manual(
        values = c("gini" = "solid", "extratrees" = "dashed"),
        name   = "splitrule"
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_number(accuracy = 0.001)
      ) +
      ggplot2::labs(
        title    = nombre,
        subtitle = "Selección de hiperparámetros · gini y extratrees",
        x        = "mtry",
        y        = "AUC-ROC (CV)"
      ) +
      ggplot2::guides(
        color    = ggplot2::guide_legend(nrow = 2),
        linetype = ggplot2::guide_legend(nrow = 1)
      ) +
      tema_base +
      ggplot2::theme(legend.position = "bottom"),
    width  = 9,
    height = 5,
    units  = "in"
  )

  # 2.3.7. Partial-dependence profiles (PDP) – DALEX
  # 2.3.7.1. Calcular perfiles para todas las variables predictoras
  set.seed(seed)
  pdp <- DALEX::model_profile(
    explainer  = explainer,
    variables  = setdiff(names(dataset), resp_var),
    type       = "partial",
    N          = 500
  )

  base::saveRDS(
    object = pdp,
    file   = here::here(paste0(ruta, nombre, "_pdp.rds"))
  )

  # 2.3.7.2. Extraer tabla de perfiles y generar un gráfico por variable
  pdp_df   <- as.data.frame(pdp$agr_profiles)
  vars_pdp <- unique(pdp_df[["_vname_"]])

  invisible(lapply(vars_pdp, function(var) {
    df_var <- pdp_df[pdp_df[["_vname_"]] == var, ]

    ggplot2::ggsave(
      filename  = here::here(paste0(ruta, nombre, "_pdp_", var, ".pdf")),
      plot      = ggplot2::ggplot(
        df_var,
        ggplot2::aes(x = `_x_`, y = `_yhat_`)
      ) +
        ggplot2::geom_line(linewidth = 0.9, color = "grey30") +
        ggplot2::geom_rug(
          sides     = "b",
          linewidth = 0.25,
          color     = "grey60",
          alpha     = 0.4
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::label_number(accuracy = 0.001),
          limits = c(min(df_var$`_yhat_`) - 0.05, max(df_var$`_yhat_`) + 0.05) # Ajuste dinámico óptimo
        ) +
        ggplot2::labs(
          title    = nombre,
          subtitle = paste0("Partial-dependence profile · ", var),
          x        = var,
          y        = "Probabilidad predicha (media)"
        ) +
        tema_base,
      width     = 6,
      height    = 4,
      units     = "in",
      limitsize = FALSE
    )
  }))

  # 2.3.7.3. Panel resumen con todas las variables en una sola página
  pdp_panel <- ggplot2::ggplot(
    pdp_df,
    ggplot2::aes(x = `_x_`, y = `_yhat_`)
  ) +
    ggplot2::geom_line(linewidth = 0.7, color = "grey30") +
    ggplot2::geom_rug(
      sides     = "b",
      linewidth = 0.20,
      color     = "grey60",
      alpha     = 0.35
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(accuracy = 0.01),
      limits = c(0, 1)
    ) +
    ggplot2::facet_wrap(
      ~ `_vname_`,
      scales = "free_x",
      ncol   = 3
    ) +
    ggplot2::labs(
      title    = nombre,
      subtitle = "Partial-dependence profiles · todas las variables",
      x        = NULL,
      y        = "Probabilidad predicha (media)"
    ) +
    tema_base +
    ggplot2::theme(
      strip.text       = ggplot2::element_text(face = "bold", size = 8),
      strip.background = ggplot2::element_rect(fill = "grey96", color = NA)
    )

  ggplot2::ggsave(
    filename  = here::here(paste0(ruta, nombre, "_pdp_panel.pdf")),
    plot      = pdp_panel,
    width     = 12,
    height    = max(4, ceiling(length(vars_pdp) / 3) * 3.5),
    units     = "in",
    limitsize = FALSE
  )

  # 2.3.8. Rendimiento del modelo – clasificación binaria
  # 2.3.8.1. Extraer predicciones finales del CV y construir tabla unificada
  preds_cv <- rf$pred |>
    dplyr::rename(
      obs  = obs,
      pred = pred
    ) |>
    dplyr::mutate(
      obs_num = as.integer(obs) - 1L
    )

  # 2.3.8.2. Rendimiento global via DALEX (medidas escalares)
  set.seed(seed)
  perf_ema <- DALEX::model_performance(explainer)

  base::saveRDS(
    object = perf_ema,
    file   = here::here(paste0(ruta, nombre, "_perf_ema.rds"))
  )

  # 2.3.8.3. Curva ROC construida desde las predicciones del CV (yardstick)
  pos_cls <- levels(preds_cv$obs)[2]

  roc_df <- yardstick::roc_curve(
    data      = preds_cv,
    truth     = obs,
    !!pos_cls,
    event_level = "second"
  )

  auc_val <- yardstick::roc_auc(
    data        = preds_cv,
    truth       = obs,
    !!pos_cls,
    event_level = "second"
  )$.estimate

  ggplot2::ggsave(
    filename = here::here(paste0(ruta, nombre, "_rf_roc.pdf")),
    plot     = ggplot2::ggplot(
      roc_df,
      ggplot2::aes(x = 1 - specificity, y = sensitivity)
    ) +
      ggplot2::geom_abline(
        slope     = 1,
        intercept = 0,
        linetype  = "dashed",
        color     = "grey70",
        linewidth = 0.6
      ) +
      ggplot2::geom_line(
        linewidth = 1.0,
        color     = "grey20"
      ) +
      ggplot2::annotate(
        "text",
        x     = 0.75,
        y     = 0.15,
        label = paste0("AUC = ", round(auc_val, 4)),
        size  = 3.5,
        color = "grey30"
      ) +
      ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
      ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
      ggplot2::labs(
        title    = nombre,
        subtitle = "Curva ROC · predicciones finales CV repeatedcv (yardstick)",
        x        = "Tasa de falsos positivos (1 - Especificidad)",
        y        = "Tasa de verdaderos positivos (Sensibilidad)"
      ) +
      tema_base,
    width  = 6,
    height = 5,
    units  = "in"
  )

  # 2.3.8.4. Gráfico de distribución de residuos (lift de probabilidades)
  res_df <- data.frame(
    obs_num  = preds_cv$obs_num,
    prob_pos = preds_cv[[levels(preds_cv$obs)[2]]]
  ) |>
    dplyr::mutate(
      clase = factor(
        ifelse(obs_num == 1L, "Positivo", "Negativo"),
        levels = c("Positivo", "Negativo")
      )
    )

  ggplot2::ggsave(
    filename = here::here(paste0(ruta, nombre, "_rf_prob_dist.pdf")),
    plot     = ggplot2::ggplot(
      res_df,
      ggplot2::aes(x = prob_pos, fill = clase, color = clase)
    ) +
      ggplot2::geom_density(
        alpha     = 0.35,
        linewidth = 0.6
      ) +
      ggplot2::scale_fill_manual(
        values = c("Positivo" = "#2c7bb6", "Negativo" = "#d7191c"),
        name   = NULL
      ) +
      ggplot2::scale_color_manual(
        values = c("Positivo" = "#2c7bb6", "Negativo" = "#d7191c"),
        name   = NULL
      ) +
      ggplot2::scale_x_continuous(
        labels = scales::label_percent(accuracy = 1),
        limits = c(0, 1)
      ) +
      ggplot2::labs(
        title    = nombre,
        subtitle = "Distribución de probabilidad predicha por clase · CV repeatedcv",
        x        = "Probabilidad predicha (clase positiva)",
        y        = "Densidad"
      ) +
      tema_base +
      ggplot2::theme(legend.position = "bottom"),
    width  = 7,
    height = 4.5,
    units  = "in"
  )

  # 2.3.8.5. Matriz de confusión – umbral 0.5
  conf_df <- preds_cv |>
    dplyr::count(obs, pred, name = "n") |>
    dplyr::mutate(
      obs  = factor(obs,  levels = rev(levels(preds_cv$obs))),
      pred = factor(pred, levels =     levels(preds_cv$obs))
    )

  ggplot2::ggsave(
    filename = here::here(paste0(ruta, nombre, "_rf_conf_mat.pdf")),
    plot     = ggplot2::ggplot(
      conf_df,
      ggplot2::aes(x = pred, y = obs, fill = n)
    ) +
      ggplot2::geom_tile(color = "white", linewidth = 0.8) +
      ggplot2::geom_text(
        ggplot2::aes(label = scales::label_comma()(n)),
        size  = 4,
        color = "white",
        fontface = "bold"
      ) +
      ggplot2::scale_fill_gradient(
        low  = "grey80",
        high = "grey20",
        name = "Observaciones"
      ) +
      ggplot2::labs(
        title    = nombre,
        subtitle = "Matriz de confusión · umbral 0.5 · CV repeatedcv (predicciones finales)",
        x        = "Predicción",
        y        = "Observación real"
      ) +
      tema_base +
      ggplot2::theme(
        axis.text  = ggplot2::element_text(size = 11),
        legend.position = "right"
      ),
    width  = 5,
    height = 4,
    units  = "in"
  )

  # 2.3.8.6. Métricas de rendimiento escalares – tabla visual
  niveles <- levels(preds_cv$obs)

  metricas_df <- data.frame(
    Métrica = c("AUC-ROC", "Accuracy", "Kappa", "Sensibilidad", "Especificidad", "F1"),
    Valor   = c(
      round(auc_val,                                                         4),
      round(mean(preds_cv$obs == preds_cv$pred),                            4),
      round(
        {
          cm  <- table(preds_cv$pred, preds_cv$obs)
          po  <- sum(diag(cm)) / sum(cm)
          pe  <- sum(rowSums(cm) * colSums(cm)) / sum(cm)^2
          (po - pe) / (1 - pe)
        },
        4
      ),
      round(
        sum(preds_cv$pred == pos_cls & preds_cv$obs == pos_cls) /
          max(sum(preds_cv$obs == pos_cls), 1),
        4
      ),
      round(
        {
          neg_cls <- niveles[1]
          sum(preds_cv$pred == neg_cls & preds_cv$obs == neg_cls) /
            max(sum(preds_cv$obs == neg_cls), 1)
        },
        4
      ),
      round(
        {
          tp  <- sum(preds_cv$pred == pos_cls & preds_cv$obs == pos_cls)
          fp  <- sum(preds_cv$pred == pos_cls & preds_cv$obs != pos_cls)
          fn  <- sum(preds_cv$pred != pos_cls & preds_cv$obs == pos_cls)
          2 * tp / max(2 * tp + fp + fn, 1)
        },
        4
      )
    )
  )

  ggplot2::ggsave(
    filename = here::here(paste0(ruta, nombre, "_rf_metricas.pdf")),
    plot     = ggplot2::ggplot(
      metricas_df,
      ggplot2::aes(
        x    = Valor,
        y    = stats::reorder(Métrica, Valor),
        fill = Valor
      )
    ) +
      ggplot2::geom_col(width = 0.60) +
      ggplot2::geom_text(
        ggplot2::aes(label = scales::label_number(accuracy = 0.0001)(Valor)),
        hjust = -1.1,
        size  = 3.2,
        color = "grey30"
      ) +
      ggplot2::scale_x_continuous(
        limits = c(0, 1.15),
        labels = scales::label_number(accuracy = 0.01)
      ) +
      ggplot2::scale_fill_gradient(
        low  = "grey75",
        high = "grey25",
        guide = "none"
      ) +
      ggplot2::labs(
        title    = nombre,
        subtitle = "Métricas de rendimiento · clasificación binaria · CV repeatedcv",
        x        = "Valor",
        y        = NULL
      ) +
      tema_base,
    width  = 7,
    height = 4,
    units  = "in"
  )
}

# 3. Definir datasets
datasets <- list(
  list(
    dataset  = readRDS(here::here("data/modelos/eda/vif/2008_2012/df_f_be_2008_2012.rds")),
    ecuation = "diferencia_2008_2012_rast ~ .",
    ruta     = "data/modelos/rf/2008_2012/",
    nombre   = "rf_f_be_2008_2012"
  ),
  list(
    dataset  = readRDS(here::here("data/modelos/eda/vif/2008_2023/df_f_be_2008_2023.rds")),
    ecuation = "diferencia_2008_2023_rast ~ .",
    ruta     = "data/modelos/rf/2008_2023/",
    nombre   = "rf_f_be_2008_2023"
  ),
  list(
    dataset  = readRDS(here::here("data/modelos/eda/vif/2015_2023/df_f_be_2015_2023.rds")),
    ecuation = "diferencia_2015_2023_rast ~ .",
    ruta     = "data/modelos/rf/2015_2023/",
    nombre   = "rf_f_be_2015_2023"
  ),
  list(
    dataset  = readRDS(here::here("data/modelos/eda/vif/2020_2023/df_f_be_2020_2023.rds")),
    ecuation = "diferencia_2020_2023_rast ~ .",
    ruta     = "data/modelos/rf/2020_2023/",
    nombre   = "rf_f_be_2020_2023"
  )
)

# 4. Crear carpetas de salida
sapply(
  datasets,
  function(m) dir.create(here::here(m$ruta), recursive = TRUE, showWarnings = FALSE)
)

# 5. Ejecutar modelos
lapply(datasets, function(m) {
  rf_model(
    dataset  = m$dataset,
    ecuation = m$ecuation,
    ruta     = m$ruta,
    nombre   = m$nombre
  )
})