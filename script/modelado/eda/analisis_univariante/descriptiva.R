# 1. Limpieza y paquetes
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
base::gc(full = TRUE)

pacman::p_load(dplyr, fs, here, stringr, purrr, moments, jsonlite)

# 2. Funciones auxiliares
# 2.1. Detectar si una variable es categórica
es_categorica <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x) || length(unique(na.omit(x))) <= 4L
}

# 2.2. Estadísticos para una variable numérica
stats_numerica <- function(x, var) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  q <- quantile(x, probs = c(0.05, 0.25, 0.75, 0.95), na.rm = TRUE)
  list(
    variable      = var,
    tipo          = "Numérica",
    n             = length(x),
    n_na          = sum(!is.finite(x)),
    media         = round(mean(x),                    4L),
    mediana       = round(median(x),                  4L),
    sd            = round(sd(x),                      4L),
    varianza      = round(var(x),                     4L),
    se            = round(sd(x) / sqrt(length(x)),    4L),
    cv            = round((sd(x) / mean(x)) * 100,    4L),
    iqr           = round(IQR(x),                     4L),
    rango         = round(max(x) - min(x),            4L),
    asimetria     = round(moments::skewness(x),       4L),
    curtosis      = round(moments::kurtosis(x),       4L),
    min           = round(min(x),                     4L),
    p05           = round(q[["5%"]],                  4L),
    p25           = round(q[["25%"]],                 4L),
    p75           = round(q[["75%"]],                 4L),
    p95           = round(q[["95%"]],                 4L),
    max           = round(max(x),                     4L),
    n_niveles     = NA,
    moda          = NA,
    frec_moda     = NA,
    frec_moda_pct = NA
  )
}

# 2.3. Estadísticos para una variable categórica
stats_categorica <- function(x, var) {
  x_clean <- na.omit(as.character(x))
  frec    <- sort(table(x_clean), decreasing = TRUE)
  list(
    variable      = var,
    tipo          = "Categórica",
    n             = length(x_clean),
    n_na          = sum(is.na(x)),
    media         = NA, mediana  = NA, sd       = NA, varianza = NA,
    se            = NA, cv       = NA, iqr      = NA, rango    = NA,
    asimetria     = NA, curtosis = NA, min      = NA, p05      = NA,
    p25           = NA, p75      = NA, p95      = NA, max      = NA,
    n_niveles     = length(frec),
    moda          = names(frec)[1],
    frec_moda     = as.integer(frec[1]),
    frec_moda_pct = round(as.numeric(frec[1]) / length(x_clean) * 100, 2L)
  )
}

# 2.4. Calcular estadísticos para un subconjunto del df
calcular_stats <- function(df, predictoras) {
  purrr::map(predictoras, function(v) {
    x <- df[[v]]
    if (es_categorica(x)) stats_categorica(x, v)
    else                   stats_numerica(x, v)
  })
}

# 2.5. Procesar un RDS completo y devolver lista con las 3 tablas
procesar_periodo <- function(ruta) {

  periodo     <- stringr::str_extract(basename(ruta), "\\d{4}_\\d{4}")
  df          <- readRDS(ruta)
  predicha    <- names(df)[grep("^(diferencia|presencia)_", names(df))][1]
  predictoras <- setdiff(names(df), predicha)

  # Clase como entero 0/1
  clase <- if (is.factor(df[[predicha]])) {
    as.integer(df[[predicha]]) - 1L
  } else {
    as.integer(df[[predicha]])
  }

  list(
    periodo = periodo,
    global  = calcular_stats(df,                       predictoras),
    clase_0 = calcular_stats(df[clase == 0L, ],        predictoras),
    clase_1 = calcular_stats(df[clase == 1L, ],        predictoras)
  )
}

# 3. Descubrimiento de datasets
rds <- fs::dir_ls(
  here::here("data/modelado/data_cleaning/balanceo"),
  regexp  = "df_b_.*\\.rds$",
  recurse = TRUE
)

# 4. Ejecución y guardado de JSON por periodo
dir_salida <- here::here("data/modelado/eda/analisis_univariante/estadistica_descriptiva")
fs::dir_create(dir_salida)

purrr::walk(rds, function(ruta) {
  res  <- procesar_periodo(ruta)
  ruta_json <- fs::path(dir_salida, paste0("descriptiva_", res$periodo, ".json"))
  jsonlite::write_json(res, path = ruta_json, auto_unbox = TRUE, null = "null")
  message("Guardado: ", ruta_json)
})