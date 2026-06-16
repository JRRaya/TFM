# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(globalenv()))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(here, terra, predicts)

# 2. Función de cálculo de las variables bioclimáticas
calcular_bio <- function (patron, periodo) {
  # 1. Obtener promedios mensuales de las variables climáticas
  # 1.1. Construir los stacks de las variables con todos los meses de la serie
  prec <- terra::rast(
    list.files(
      path = here::here("data/variables/variables_bioclimaticas/precipitacion"),
      pattern = patron,
      full.names = TRUE
    )
  )

  tmax <- terra::rast(
    list.files(
      path = here::here("data/variables/variables_bioclimaticas/temperatura/temperatura_max"),
      pattern = patron,
      full.names = TRUE
    )
  )
  
  tmin <- terra::rast(
    list.files(
      path = here::here("data/variables/variables_bioclimaticas/temperatura/temperatura_min"),
      pattern = patron,
      full.names = TRUE
    )
  )

  # 1.2. crear el índice de las capas
  idx <- base::rep(
    x = 1:12, 
    length.out = terra::nlyr(prec)
  )

  # 1.3. Crear los promedios mensuales para cada una de las variables
  prec_n <- terra::tapp(prec, idx, fun = mean)

  tmax_n <- terra::tapp(tmax, idx, fun = mean)

  tmin_n <- terra::tapp(tmin, idx, fun = mean)

  # 2. Cálculo de las variables bioclimáticas
  bioclimaticas <- predicts::bcvars(
    prec = prec_n, 
    tmin = tmax_n, 
    tmax = tmin_n
  )

  # 3. Renombrar las variables
  names(bioclimaticas) <- c(
    paste0("t_mean_anual_", periodo),
    paste0("mean_diurnal_range_", periodo),
    paste0("isothermality_", periodo),
    paste0("t_seasonality_", periodo),
    paste0("t_max_warmest_month_", periodo),
    paste0("t_min_coldest_month_", periodo),
    paste0("t_anual_range_", periodo),
    paste0("t_mean_wettest_quarter_", periodo),
    paste0("t_mean_driest_quarter_", periodo),
    paste0("t_mean_warmest_quarter_", periodo),
    paste0("t_mean_coldest_quarter_", periodo),
    paste0("p_anual_", periodo),
    paste0("p_wettest_month_", periodo),
    paste0("p_driest_month_", periodo),
    paste0("p_seasonality_", periodo),
    paste0("p_wettest_quarter_", periodo),
    paste0("p_driest_quarter_", periodo),
    paste0("p_warmest_quarter_", periodo),
    paste0("p_coldest_quarter_", periodo)
  )

  # 4. Guardado
  terra::writeRaster(
    bioclimaticas, 
    here::here(paste0("data/variables/variables_bioclimaticas/bioclimaticas/bioclimaticas_", periodo,".tif")), 
    overwrite = TRUE
  )

  # 5. Liberar memoria
  rm(prec, tmax, tmin, idx, prec_n, tmax_n, tmin_n, bioclimaticas)
  gc(full = TRUE)
  terra::tmpFiles(remove = TRUE)
}

# 3. Calcular para cada periodo
calcular_bio(
  patron = "(p|tm3|tm2)_(1979|198[0-9]|199[0-9]|200[0-8])_.*\\.tif$",
  periodo = "1979_2008"
)

calcular_bio(
  patron = "(p|tm3|tm2)_(198[6-9]|199[0-9]|200[0-9]|201[0-5])_.*\\.tif$",
  periodo = "1986_2015"
)

calcular_bio(
  patron = "(p|tm3|tm2)_(199[1-9]|200[0-9]|201[0-9]|2020)_.*\\.tif$",
  periodo = "1991_2020"
)