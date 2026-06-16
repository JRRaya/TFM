# 1. Limpieza y paquetes
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
base::gc(full = TRUE)

pacman::p_load(dplyr, here, ggpubr, furrr, fs, stringr, jsonlite, moments)

# 2. Paralelismo
future::plan(future::multisession, workers = 10L)

# 3. Funciones auxiliares
# 3.1. Detectar si una variable es categórica
es_categorica <- function(x) {
  is.factor(x) || is.character(x) || length(unique(na.omit(x))) <= 4L
}

# 3.2. Generar gráfico de densidad/barras para una variable predictora
procesar_variable_png <- function(i, input_df, predicha, dir_graficos) {
  # 1. Subconjunto y limpieza
  df_var <- input_df[, c(predicha, i), drop = FALSE] |>
    dplyr::filter(is.finite(.data[[predicha]])) |>
    dplyr::mutate(clase = factor(.data[[predicha]]))

  # 2. Control de calidad
  if (sum(df_var[[predicha]] == 1L) < 10L || sum(df_var[[predicha]] == 0L) < 10L) return(NULL)

  # 3. Gráfico según tipo de variable
  if (es_categorica(input_df[[i]])) {
    df_agg <- df_var |>
      dplyr::mutate(across(all_of(i), factor)) |>
      dplyr::count(clase, .data[[i]]) |>
      dplyr::rename(categoria = all_of(i))

    p <- ggpubr::ggbarplot(
      df_agg,
      x        = "categoria",
      y        = "n",
      fill     = "clase",
      color    = "clase",
      palette  = c("#2166AC", "#8C2D04"),
      position = position_dodge(0.7),
      title    = paste("Distribución:", i),
      xlab     = i,
      ylab     = "Conteo"
    )
  } else {
    p <- ggpubr::ggdensity(
      df_var,
      x       = i,
      color   = "clase",
      fill    = "clase",
      rug     = TRUE,
      palette = c("#2166AC", "#8C2D04"),
      add     = "mean",
      title   = paste("Densidad:", i),
      xlab    = i,
      ylab    = "Densidad"
    )
  }

  # 4. Guardado
  ggplot2::ggsave(
    file.path(dir_graficos, paste0("densidad_", i, ".png")),
    p,
    width  = 8,
    height = 5,
    dpi    = 600
  )

  invisible(NULL)
}

# 3.3. Función de iteración paralela sobre cada variable predictora (PNG)
densidad <- function(input_df, ruta_salida) {
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
    .f = function(i) procesar_variable_png(
      i,
      input_df,
      predicha,
      dir_graficos = ruta_salida
    ),
    .options = furrr::furrr_options(seed = TRUE)
  )
}

# 3.4. Estadísticos descriptivos de un vector numérico (para tooltips JSON)
stats_vec <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(NULL)
  q <- quantile(x, probs = c(0.05, 0.25, 0.75, 0.95))
  list(
    n         = length(x),
    media     = round(mean(x),              4L),
    mediana   = round(median(x),            4L),
    sd        = round(sd(x),               4L),
    iqr       = round(IQR(x),              4L),
    min       = round(min(x),              4L),
    p05       = round(q[["5%"]],            4L),
    p25       = round(q[["25%"]],           4L),
    p75       = round(q[["75%"]],           4L),
    p95       = round(q[["95%"]],           4L),
    max       = round(max(x),              4L),
    asimetria = round(moments::skewness(x), 4L),
    curtosis  = round(moments::kurtosis(x), 4L)
  )
}

# 3.5. Kernel density estimate (para gráfico de densidad JSON)
kde_vec <- function(x, n_puntos = 512L) {
  x <- x[is.finite(x)]
  if (length(x) < 10L) return(NULL)
  d <- density(x, n = n_puntos)
  list(x = round(d$x, 6L), y = round(d$y, 8L))
}

# 3.6. Frecuencias para variables categóricas (JSON)
frecuencias_cat <- function(x) {
  x_c <- as.character(x[!is.na(x)])
  tb  <- sort(table(x_c), decreasing = TRUE)
  list(
    categorias = names(tb),
    conteos    = as.integer(tb)
  )
}

# 3.7. Procesar una variable predictora y devolver su estructura JSON
procesar_variable_json <- function(var, df, predicha) {
  x_all <- df[[var]]
  clase <- if (is.factor(df[[predicha]])) as.integer(df[[predicha]]) - 1L else as.integer(df[[predicha]])

  if (sum(clase == 1L, na.rm = TRUE) < 10L || sum(clase == 0L, na.rm = TRUE) < 10L) return(NULL)

  if (es_categorica(x_all)) {
    # ── Variable categórica: frecuencias por clase ──────────────────────────
    list(
      variable   = var,
      categorica = TRUE,
      global     = frecuencias_cat(x_all),
      clase_0    = frecuencias_cat(x_all[clase == 0L]),
      clase_1    = frecuencias_cat(x_all[clase == 1L])
    )
  } else {
    # ── Variable numérica: KDE + estadísticos por clase ──────────────────────
    x_num <- as.numeric(x_all)
    x0    <- x_num[clase == 0L]
    x1    <- x_num[clase == 1L]

    list(
      variable   = var,
      categorica = FALSE,
      log_x      = FALSE,
      global  = list(kde = kde_vec(x_num), stats = stats_vec(x_num)),
      clase_0 = list(kde = kde_vec(x0),    stats = stats_vec(x0)),
      clase_1 = list(kde = kde_vec(x1),    stats = stats_vec(x1))
    )
  }
}

# 3.8. Procesar un RDS completo (un periodo) y devolver su estructura JSON
procesar_periodo_json <- function(ruta) {
  periodo     <- stringr::str_extract(basename(ruta), "\\d{4}_\\d{4}")
  df          <- readRDS(ruta)
  predicha    <- names(df)[grep("^(diferencia|presencia)_", names(df))][1]
  predictoras <- setdiff(names(df), predicha)

  vars_data <- furrr::future_map(
    predictoras,
    ~ procesar_variable_json(.x, df, predicha),
    .options = furrr::furrr_options(seed = TRUE)
  ) |>
    purrr::compact()

  list(periodo = periodo, variables = vars_data)
}

# 4. Descubrimiento de datasets
rds <- fs::dir_ls(
  here::here("data/modelado/data_cleaning/balanceo"),
  regexp  = "df_b_.*\\.rds$",
  recurse = TRUE
)

# 5. Ejecución — PNGs de densidad
purrr::walk(rds, function(ruta_rds) {
  densidad(
    input_df    = readRDS(ruta_rds),
    ruta_salida = here::here(
      "data/modelado/eda/analisis_univariante/densidad",
      stringr::str_extract(basename(ruta_rds), "\\d{4}_\\d{4}")
    )
  )
})

# 6. Ejecución — JSON de distribución
dir_json <- here::here("data/modelado/eda/analisis_univariante/distribucion")
fs::dir_create(dir_json)

purrr::walk(rds, function(ruta) {
  res       <- procesar_periodo_json(ruta)
  ruta_json <- fs::path(dir_json, paste0("distribucion_", res$periodo, ".json"))
  jsonlite::write_json(res, path = ruta_json, auto_unbox = TRUE, null = "null")
  message("Guardado: ", ruta_json)
})

# 7. Cerrar paralelismo
future::plan(future::sequential)