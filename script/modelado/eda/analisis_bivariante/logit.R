# 1. Limpieza y paquetes
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
base::gc(full = TRUE)

pacman::p_load(dplyr, here, ggpubr, stats, DescTools, furrr, fs, stringr)

# 2. Paralelismo
future::plan(future::multisession, workers = 10L)

# 3. Funciones auxiliares
# 3.1. Generar modelos y gráficos para una variable predictora
procesar_variable <- function(i, input_df, predicha, dir_graficos, dir_modelos) {
  # 1. Cargar dataset
  df_var <- input_df[, c(predicha, i), drop = FALSE] |>
    dplyr::filter(
      is.finite(.data[[i]]) & is.finite(.data[[predicha]]) # Asegurar que ambas variables tienen valores no finitos
    )

  # 2. Control de calidad
  if (sum(df_var[[predicha]] == 1L) < 10L || sum(df_var[[predicha]] == 0L) < 10L) return(NULL)

  # 3. GLM logístico
  m <- glm(
    stats::as.formula(
      paste(predicha, "~", i)
    ),
    data = df_var, 
    family = stats::binomial(link = "logit")
  )

  # 4. Estadísticos
  estadisticos <- summary(m)$coefficients |>
    as.data.frame() |>
    dplyr::mutate(
      variable = i, 
      nagelkerke = DescTools::PseudoR2(
        m, 
        which = "Nagelkerke"
      )
    )

  # 5. Curva de respuesta logística
  curva <- ggpubr::ggscatter(
    df_var,
    x     = i,
    y     = predicha,
    alpha = 0.25,
    color = "#8C2D04",
    size  = 1.2,
    title    = paste("Curva de respuesta:", i),
    subtitle = paste("Pseudo-R² Nagelkerke:", round(DescTools::PseudoR2(m, which = "Nagelkerke"), 3)),
    xlab = i,
    ylab = "Probabilidad de invasión"
  ) +
  ggplot2::geom_smooth(
    method      = "glm",
    formula     = y ~ x,
    method.args = list(family = binomial),
    color       = "#2166AC",
    fill        = "#2166AC",
    se          = TRUE
  )

  # 6. Guardado
  write.csv(
    estadisticos,
    file.path(
      dir_modelos, 
      paste0("stats_", i, ".csv")
    ), 
    row.names = TRUE
  )

  ggplot2::ggsave(
    file.path(dir_graficos, paste0("curva_", i, ".png")),
    curva, 
    width = 8, 
    height = 5, 
    dpi = 600 
  )

  ggplot2::ggsave(
    file.path(dir_graficos, "preview", paste0("curva_", i, ".png")),
    curva, 
    width = 7, 
    height = 3.5, 
    dpi = 150
  )

  invisible(NULL)
}

# 3.2. Función de iteración paralela sobre cada variable explicativa
logit <- function(input_df, ruta_salida) {
  # 1. Detectar variable respuesta y variables predictoras del dataset
  predicha <- names(input_df)[grep("^(diferencia|presencia)_", names(input_df))][1]
  predictoras <- setdiff(names(input_df), predicha)

  # 2. Asegurar binariedad (0/1) variable respuesta
  input_df[[predicha]] <- if (is.factor(input_df[[predicha]])) {
    as.integer(input_df[[predicha]]) - 1L
  } else {
    as.integer(input_df[[predicha]])
  }

  # 3. Crear directorios de salida
  purrr::walk(
    file.path(ruta_salida, c("graficos", "graficos/preview", "modelos")),
    dir.create, 
    recursive = TRUE, 
    showWarnings = FALSE
  )

  # 4. Procesar paralelamento los modelos/gráficos
  furrr::future_walk(
    .x = predictoras,
    .f = function(i) procesar_variable(
      i, 
      input_df, 
      predicha,
      dir_graficos = file.path(ruta_salida, "graficos"),
      dir_modelos = file.path(ruta_salida, "modelos")
    ),
    .options = furrr::furrr_options(seed = TRUE)
  )
}

# 5. Descubrimiento de datasets
rds <- fs::dir_ls(
  here::here("data/modelado/data_cleaning/balanceo"), 
  regexp = "df_b_.*\\.rds$", 
  recurse = TRUE
)

# 6. Ejecución
purrr::walk(rds, function(ruta_rds) {
  logit(
    input_df = readRDS(ruta_rds),
    ruta_salida = here::here(
      "data/modelado/eda/analisis_bivariante/logit", 
      stringr::str_extract(basename(ruta_rds), "\\d{4}_\\d{4}")
    )
  )
})

# 7. Cerrar paralelismo
future::plan(future::sequential)