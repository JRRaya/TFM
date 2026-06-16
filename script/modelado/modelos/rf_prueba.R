# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
gc(verbose = FALSE, full = TRUE)
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(caret, ranger, doFuture, future, future.apply, dplyr, here, ggplot2, scales)

# 1.3. Configuración de reproducibilidad paralela
RNGkind("L'Ecuyer-CMRG")
set.seed(123)

# 1.4. Configurar paralelismo: un worker por dataset
doFuture::registerDoFuture()
future::plan(multisession, workers = 4L)

# 2. Función RF
rf_model <- function(dataset, ecuation, ruta, nombre) {
  # 1. Definir la ecuación del modelo
  formula <- as.formula(ecuation)

  # 2. RF
  # 2.1. Definir grid de hiperparámetros
  tune <- expand.grid(
    mtry          = 2:max(2, ncol(dataset) - 1),
    splitrule     = c("gini", "extratrees"),
    min.node.size = 1:15
  )

  # 2.2. Definir semillas a partir del grid
  semillas <- c(
    lapply(
      1:200,
      function(x) sample.int(1000, nrow(tune))
    ),
    list(sample.int(1000, 1))
  )

  # 2.3. Entrenar modelo
  rf <- caret::train(
    formula,
    data        = dataset,
    method      = "ranger",
    metric      = "ROC",
    num.trees   = 2000,
    importance  = "permutation",
    num.threads = 1,
    tuneGrid    = tune,
    trControl   = caret::trainControl(
      method            = "repeatedcv",
      number            = 10,
      repeats           = 20,
      classProbs        = TRUE,
      summaryFunction   = twoClassSummary,
      sampling          = "down",
      seeds             = semillas,
      savePredictions   = "final",
      selectionFunction = "oneSE",
      allowParallel     = TRUE,
      verboseIter = TRUE
    )
  )

  # 3. Guardado
  # 3.1. Modelo
  base::saveRDS(
    object = rf,
    file   = here::here(paste0(ruta, nombre, "_rf.RDS"))
  )

  # 3.2. Configuraciones estéticas compartidas
  colores   <- c("Bajo" = "#74add1", "Medio" = "#f46d43", "Alto" = "#a50026")
  tema_base <- ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle    = ggplot2::element_text(color = "grey40", size = 9),
      axis.title       = ggplot2::element_text(size = 10),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "grey92"),
      plot.margin      = ggplot2::margin(12, 16, 12, 12)
    )

  # 3.3. Importancia de variables
  ggplot2::ggsave(
    filename  = here::here(paste0(ruta, nombre, "_rf_var_imp.pdf")),
    plot      = caret::varImp(rf, scale = TRUE)$importance %>%
      as.data.frame() %>%
      dplyr::mutate(
        Variable = rownames(.),
        grupo    = factor(
          dplyr::case_when(
            Overall >= 66 ~ "Alto",
            Overall >= 33 ~ "Medio",
            TRUE          ~ "Bajo"
          ),
          levels = c("Bajo", "Medio", "Alto")
        )
      ) %>%
      dplyr::arrange(Overall) %>%
      dplyr::mutate(Variable = factor(Variable, levels = Variable)) %>%
      ggplot2::ggplot(ggplot2::aes(x = Variable, y = Overall, fill = grupo)) +
      ggplot2::geom_col(width = 0.7) +
      ggplot2::geom_text(
        ggplot2::aes(label = round(Overall, 1)),
        hjust = -0.2, size = 2.8, color = "grey30"
      ) +
      ggplot2::coord_flip(clip = "off") +
      ggplot2::scale_fill_manual(values = colores, name = "Importancia") +
      ggplot2::scale_y_continuous(
        limits = c(0, 115),
        breaks = seq(0, 100, 25),
        labels = function(x) paste0(x, "%")
      ) +
      ggplot2::labs(
        title    = nombre,
        subtitle = "Permutation Importance (escalada 0–100)",
        x        = NULL,
        y        = "Importancia relativa (%)"
      ) +
      tema_base +
      ggplot2::theme(legend.position = "bottom"),
    width     = 8,
    height    = max(4, nrow(caret::varImp(rf, scale = TRUE)$importance) * 0.28),
    units     = "in",
    limitsize = FALSE
  )

  # 3.4. Tuning de hiperparámetros
  ggplot2::ggsave(
    filename = here::here(paste0(ruta, nombre, "_rf_tuning.pdf")),
    plot     = rf$results %>%
      dplyr::mutate(
        min.node.size = factor(min.node.size),
        splitrule     = factor(splitrule)
      ) %>%
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
        data = rf$results %>%
          dplyr::mutate(
            min.node.size = factor(min.node.size),
            splitrule     = factor(splitrule)
          ) %>%
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
}

# 3. Definir datasets
datasets <- list(
  list(
    dataset  = readRDS(here::here("data/modelos/eda/vif/2008_2012/df_f_be_2008_2012.rds")),
    ecuation = "diferencia_2008_2012_rast ~ .",
    ruta     = "data/modelos/rf/be_2008_2012/",
    nombre   = "rf_f_be_2008_2012"
  ),
  list(
    dataset  = readRDS(here::here("data/modelos/eda/vif/2008_2023/df_f_be_2008_2023.rds")),
    ecuation = "diferencia_2008_2023_rast ~ .",
    ruta     = "data/modelos/rf/be_2008_2023/",
    nombre   = "rf_f_be_2008_2023"
  ),
  list(
    dataset  = readRDS(here::here("data/modelos/eda/vif/2015_2023/df_f_be_2015_2023.rds")),
    ecuation = "diferencia_2015_2023_rast ~ .",
    ruta     = "data/modelos/rf/be_2015_2023/",
    nombre   = "rf_f_be_2015_2023"
  ),
  list(
    dataset  = readRDS(here::here("data/modelos/eda/vif/2020_2023/df_f_be_2020_2023.rds")),
    ecuation = "diferencia_2020_2023_rast ~ .",
    ruta     = "data/modelos/rf/be_2020_2023/",
    nombre   = "rf_f_be_2020_2023"
  )
)

# 4. Crear carpetas de salida
sapply(
  datasets,
  function(m) dir.create(here::here(m$ruta), recursive = TRUE, showWarnings = FALSE)
)

# 5. Ejecutar modelos en paralelo: un worker por dataset
furrr::future_walk(
  datasets,
  ~ rf_model(
      dataset  = .x$dataset,
      ecuation = .x$ecuation,
      ruta     = .x$ruta,
      nombre   = .x$nombre
    ),
  .options = furrr::furrr_options(seed = TRUE)
)

# 6. Cerrar paralelismo
future::plan(sequential)