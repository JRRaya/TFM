# 1. Limpieza y paquetes
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
base::gc(full = TRUE)

pacman::p_load(dplyr, here, ggpubr, furrr, fs, stringr)

# 2. Paralelismo
future::plan(future::multisession, workers = 10L)

# 3. Funciones auxiliares
# 3.1. Detectar si una variable es categórica
es_categorica <- function(x) {
  is.factor(x) || is.character(x) || length(unique(na.omit(x))) <= 4L
}

# 3.2. Detectar si conviene escala logarítmica
usar_log <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0 || any(x < 0)) return(FALSE)
  x_pos <- x[x > 0]
  if (length(x_pos) < 10L) return(FALSE)
  # Criterio 1: rango amplio entre positivos
  rango_amplio <- (max(x_pos) / min(x_pos)) > 50
  # Criterio 2: distribución muy sesgada (percentil 95 >> mediana)
  sesgo_alto   <- (quantile(x_pos, 0.95) / median(x_pos)) > 5
  rango_amplio || sesgo_alto
}

# 3.3. Aplicar escala log a un ggplot ya construido
aplicar_log <- function(p, log_x = FALSE, log_y = FALSE) {
  if (log_x) p <- p + ggplot2::scale_x_log10()
  if (log_y) p <- p + ggplot2::scale_y_log10()
  p
}

# 3.3. Generar gráfico de violin/balloon para una variable predictora
procesar_variable <- function(i, input_df, predicha, dir_graficos) {
  # 1. Subconjunto y limpieza
  df_var <- input_df[, c(predicha, i), drop = FALSE] |>
    dplyr::filter(is.finite(.data[[predicha]])) |>
    dplyr::mutate(clase = factor(.data[[predicha]]))

  # 2. Control de calidad
  if (sum(df_var[[predicha]] == 1L) < 10L || sum(df_var[[predicha]] == 0L) < 10L) return(NULL)

  # 3. Gráfico según tipo de variable
  if (es_categorica(input_df[[i]])) {
    df_var <- dplyr::mutate(df_var, across(all_of(i), factor))

    # Tabla de frecuencias con nombres fijos para ggballoonplot
    df_balloon <- df_var |>
      dplyr::count(clase, .data[[i]]) |>
      dplyr::rename(categoria = 2)

    p <- ggpubr::ggballoonplot(
      df_balloon,
      x       = "categoria",
      y       = "clase",
      size    = "n",
      fill    = "n",
      ggtheme = ggpubr::theme_pubr(),
      title   = paste("Frecuencia por clase:", i),
      xlab    = i,
      ylab    = predicha
    )
  } else {
    escala_log <- usar_log(df_var[[i]])

    p <- ggpubr::ggviolin(
      df_var,
      x          = "clase",
      y          = i,
      color      = "clase",
      fill       = "clase",
      palette    = c("#2166AC", "#8C2D04"),
      add        = c("boxplot", "mean_point"),
      add.params = list(fill = "white", width = 0.15),
      title      = paste("Distribución:", i),
      xlab       = predicha,
      ylab       = if (escala_log) paste0(i, " (log\u2081\u2080)") else i
    )
    p <- aplicar_log(p, log_y = escala_log)
  }

  # 4. Guardado
  ggplot2::ggsave(
    file.path(dir_graficos, paste0("boxplot_", i, ".png")),
    p,
    width  = 8,
    height = 5,
    dpi    = 600
  )

  ggplot2::ggsave(
    file.path(dir_graficos, "preview", paste0("boxplot_", i, ".png")),
    p,
    width  = 7,
    height = 3.5,
    dpi    = 150
  )

  invisible(NULL)
}

# 3.4. Función de iteración paralela sobre cada variable predictora
boxplot <- function(input_df, ruta_salida) {
  # 1. Detectar variable respuesta y predictoras
  predicha    <- names(input_df)[grep("^(diferencia|presencia)_", names(input_df))][1]
  predictoras <- setdiff(names(input_df), predicha)

  # 2. Crear directorios de salida
  purrr::walk(
    file.path(ruta_salida, c("", "preview")),
    dir.create,
    recursive    = TRUE,
    showWarnings = FALSE
  )

  # 3. Procesar en paralelo
  furrr::future_walk(
    .x = predictoras,
    .f = function(i) procesar_variable(
      i,
      input_df,
      predicha,
      dir_graficos = ruta_salida
    ),
    .options = furrr::furrr_options(seed = TRUE)
  )
}

# 4. Descubrimiento de datasets
rds <- fs::dir_ls(
  here::here("data/modelado/data_cleaning/balanceo"),
  regexp  = "df_b_.*\\.rds$",
  recurse = TRUE
)

# 5. Ejecución
purrr::walk(rds, function(ruta_rds) {
  boxplot(
    input_df    = readRDS(ruta_rds),
    ruta_salida = here::here(
      "data/modelado/eda/analisis_univariante/boxplot",
      stringr::str_extract(basename(ruta_rds), "\\d{4}_\\d{4}")
    )
  )
})

# 6. Cerrar paralelismo
future::plan(future::sequential)