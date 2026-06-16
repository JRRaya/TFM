# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
future::plan(future::sequential)
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(terra, sf, dplyr, tidyr, purrr, furrr, future, here)

# 1.3. Configurar paralelismo
future::plan(multisession, workers = 8L)

# 2. Función de escalado
escalado <- function(ruta_input, ruta_output) {
  # 1. Lectura del dataframe
  df <- base::readRDS(ruta_input)

  # 2. Escalado y filtrado de variables categóricas
  df_e <- df %>%
    dplyr::select(where(is.numeric)) %>% # Seleccionar variables no categóricas
    base::scale() %>%
    base::as.data.frame()

  # 3. Guardado
  base::saveRDS(df_e, ruta_output)

  # 4. Limpieza
  rm(df, df_e)
  gc(full = TRUE)
}

# 3. Definir datasets
datasets <- list(
  list(
    ruta_input  = here::here("data/modelos/normalizacion/df_2008_2012.rds"),
    ruta_output = here::here("data/modelos/escalado/df_e_2008_2012.rds")
  ),
  list(
    ruta_input  = here::here("data/modelos/normalizacion/df_2008_2023.rds"),
    ruta_output = here::here("data/modelos/escalado/df_e_2008_2023.rds")
  ),
  list(
    ruta_input  = here::here("data/modelos/normalizacion/df_2015_2023.rds"),
    ruta_output = here::here("data/modelos/escalado/df_e_2015_2023.rds")
  ),
  list(
    ruta_input  = here::here("data/modelos/normalizacion/df_2020_2023.rds"),
    ruta_output = here::here("data/modelos/escalado/df_e_2020_2023.rds")
  ),
  list(
    ruta_input  = here::here("data/modelos/balanceo/df_b_2008_2012.rds"),
    ruta_output = here::here("data/modelos/escalado/df_be_2008_2012.rds")
  ),
  list(
    ruta_input  = here::here("data/modelos/balanceo/df_b_2008_2023.rds"),
    ruta_output = here::here("data/modelos/escalado/df_be_2008_2023.rds")
  ),
  list(
    ruta_input  = here::here("data/modelos/balanceo/df_b_2015_2023.rds"),
    ruta_output = here::here("data/modelos/escalado/df_be_2015_2023.rds")
  ),
  list(
    ruta_input  = here::here("data/modelos/balanceo/df_b_2020_2023.rds"),
    ruta_output = here::here("data/modelos/escalado/df_be_2020_2023.rds")
  )
)

# 4. Ejecutar en paralelo
furrr::future_walk(
  datasets,
  ~escalado(
    ruta_input  = .x$ruta_input,
    ruta_output = .x$ruta_output
  ),
  .options = furrr::furrr_options(seed = TRUE)
)

# 5. Detener paralelización
future::plan(sequential)