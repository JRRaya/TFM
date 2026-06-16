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

# 3.4. Generar gráfico para un par de variables
procesar_par <- function(par, input_df, predicha, dir_graficos) {
  var_x <- par[1]
  var_y <- par[2]

  # 1. Reordenar par mixto: siempre (categórica, numérica)
  if (es_categorica(input_df[[var_y]]) && !es_categorica(input_df[[var_x]])) {
    var_x <- par[2]
    var_y <- par[1]
  }

  # 2. Subconjunto, limpieza y construcción de clase
  df_clases <- input_df[, c(predicha, var_x, var_y), drop = FALSE] |>
    dplyr::filter(is.finite(.data[[predicha]])) |>
    dplyr::mutate(clase = factor(
      dplyr::if_else(.data[[predicha]] == 1L, "Presencia", "Ausencia"),
      levels = c("Ausencia", "Presencia")
    ))

  if (nrow(df_clases) < 10L) return(NULL)

  # 3. Añadir panel "Ambas" duplicando todas las filas con clase combinada
  df_par <- dplyr::bind_rows(
    df_clases,
    dplyr::mutate(df_clases, clase = factor("Ambas"))
  ) %>%
    dplyr::mutate(clase = factor(clase, levels = c("Ausencia", "Presencia", "Ambas")))

  paleta <- c("Ausencia" = "#2166AC", "Presencia" = "#8C2D04", "Ambas" = "#4D9D5A")

  # 4. Gráfico según combinación de tipos
  cat_x <- es_categorica(input_df[[var_x]])
  cat_y <- es_categorica(input_df[[var_y]])

  p <- if (!cat_x && !cat_y) {
    # Ambas numéricas: scatterplot con regresión lineal, ecuación y r de Spearman
    log_x <- usar_log(df_par[[var_x]])
    log_y <- usar_log(df_par[[var_y]])
    p <- ggpubr::ggscatter(
      df_par,
      x          = var_x,
      y          = var_y,
      color      = "clase",
      palette    = paleta,
      size       = 0.8,
      alpha      = 0.25,
      add        = "reg.line",
      add.params = list(linewidth = 0.8),
      conf.int   = TRUE,
      facet.by   = "clase",
      title      = paste(var_x, "\u2014", var_y),
      xlab       = if (log_x) paste0(var_x, " (log\u2081\u2080)") else var_x,
      ylab       = if (log_y) paste0(var_y, " (log\u2081\u2080)") else var_y
    ) +
      ggpubr::stat_regline_equation(
        size        = 2.8,
        label.x.npc = "left",
        label.y.npc = 0.97
      ) +
      ggpubr::stat_regline_equation(
        ggplot2::aes(label = after_stat(rr.label)),
        size        = 2.8,
        label.x.npc = "left",
        label.y.npc = 0.91
      ) +
      ggpubr::stat_cor(
        method        = "spearman",
        cor.coef.name = "rho",
        size          = 2.8,
        label.x.npc   = "left",
        label.y.npc   = 0.85
      )
    aplicar_log(p, log_x, log_y)
  } else if (cat_x && !cat_y) {
    # Categórica + numérica: violin + boxplot
    log_y <- usar_log(df_par[[var_y]])
    p <- ggpubr::ggviolin(
      dplyr::mutate(df_par, across(all_of(var_x), factor)),
      x          = var_x,
      y          = var_y,
      color      = "clase",
      fill       = "clase",
      palette    = paleta,
      add        = c("boxplot", "mean_point"),
      add.params = list(fill = "white", width = 0.15),
      facet.by   = "clase",
      title      = paste(var_x, "\u2014", var_y),
      xlab       = var_x,
      ylab       = if (log_y) paste0(var_y, " (log\u2081\u2080)") else var_y
    )
    aplicar_log(p, log_y = log_y)

  } else {
    # Ambas categóricas: balloon plot de frecuencias
    df_balloon <- df_par %>%
      dplyr::mutate(across(all_of(c(var_x, var_y)), factor)) %>%
      dplyr::count(clase, .data[[var_x]], .data[[var_y]])

    ggpubr::ggballoonplot(
      df_balloon,
      x        = var_x,
      y        = var_y,
      size     = "n",
      fill     = "n",
      facet.by = "clase",
      ggtheme  = ggpubr::theme_pubr(),
      title    = paste(var_x, "\u2014", var_y),
      xlab     = var_x,
      ylab     = var_y
    )
  }

  # 5. Guardado
  nombre_archivo <- paste0(var_x, "__", var_y, ".png")

  ggplot2::ggsave(
    file.path(dir_graficos, nombre_archivo), p, width = 12, height = 5, dpi = 600
  )

  ggplot2::ggsave(
    file.path(dir_graficos, "preview", nombre_archivo), p, width = 10.5, height = 3.5, dpi = 150
  )

  invisible(NULL)
}

# 3.5. Función de iteración paralela sobre todos los pares
scatterplot <- function(input_df, ruta_salida) {
  # 1. Detectar variable respuesta y predictoras
  predicha    <- names(input_df)[grep("^(diferencia|presencia)_", names(input_df))][1]
  predictoras <- setdiff(names(input_df), predicha)

  # 2. Convertir variable respuesta a 0/1
  input_df[[predicha]] <- if (is.factor(input_df[[predicha]])) {
    as.integer(input_df[[predicha]]) - 1L
  } else {
    as.integer(input_df[[predicha]])
  }

  # 3. Crear directorios de salida
  purrr::walk(
    file.path(ruta_salida, c("", "preview")),
    dir.create,
    recursive    = TRUE,
    showWarnings = FALSE
  )

  # 4. Generar todos los pares sin repetición y procesar en paralelo
  pares <- utils::combn(predictoras, 2, simplify = FALSE)

  furrr::future_walk(
    .x = pares,
    .f = function(par) procesar_par(
      par,
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
  scatterplot(
    input_df    = readRDS(ruta_rds),
    ruta_salida = here::here(
      "data/modelado/eda/analisis_bivariante/scatterplot",
      stringr::str_extract(basename(ruta_rds), "\\d{4}_\\d{4}")
    )
  )
})

# 6. Cerrar paralelismo
future::plan(future::sequential)