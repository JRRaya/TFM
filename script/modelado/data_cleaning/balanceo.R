# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
future::plan(future::sequential)
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(terra, sf, dplyr, tidyr, purrr, furrr, future, here)

# 1.3. Configurar paralelismo
future::plan(multisession, workers = 4L)

# 2. Función de balanceo
balanceo <- function(ruta_input, ruta_output) {
  # 1. Lectura del dataframe
  df <- base::readRDS(ruta_input)

  # 2. Establecer semilla 
  set.seed(123)
  
  # 3. Identificar variable respuesta
  target_var <- names(df)[grep("^diferencia_", names(df))][1]

  # 4. Calculo del grupo más pequeño
  min_size <- df %>%
    dplyr::group_by(across(all_of(target_var))) %>%
    dplyr::tally() %>%
    dplyr::pull(n) %>%
    min()

  # 5. Balanceo, factorización y guardado
  df_b <- df %>%
    dplyr::group_by(across(all_of(target_var))) %>%
    dplyr::slice_sample(n = min_size) %>% # Usamos la constante calculada
    dplyr::ungroup() 

  # 6. Guardado
  base::saveRDS(df_b, ruta_output)
  
  # 7. Limpieza
  rm(df, df_b, target_var, min_size)
  gc(full = TRUE)
}

# 3. Definir datasets
datasets <- list(
  list(
    ruta_input  = here::here("data/modelos/normalizacion/df_2008_2012.rds"),
    ruta_output = here::here("data/modelos/balanceo/df_b_2008_2012.rds")
  ),
  list(
    ruta_input  = here::here("data/modelos/normalizacion/df_2008_2023.rds"),
    ruta_output = here::here("data/modelos/balanceo/df_b_2008_2023.rds")
  ),
  list(
    ruta_input  = here::here("data/modelos/normalizacion/df_2015_2023.rds"),
    ruta_output = here::here("data/modelos/balanceo/df_b_2015_2023.rds")
  ),
  list(
    ruta_input  = here::here("data/modelos/normalizacion/df_2020_2023.rds"),
    ruta_output = here::here("data/modelos/balanceo/df_b_2020_2023.rds")
  )
)

# 4. Ejecutar en paralelo
furrr::future_walk(
  datasets,
  ~balanceo(
    ruta_input  = .x$ruta_input,
    ruta_output = .x$ruta_output
  ),
  .options = furrr::furrr_options(seed = TRUE)
)

# 5. Detener paralelización
future::plan(sequential)