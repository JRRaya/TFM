# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)
terra::tmpFiles(remove = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, terra, dplyr, whitebox)

# 2. Cálculo del MDE "fisurado"
whitebox::wbt_breach_depressions_least_cost(
  dem = here::here("data/variables/variables_relieve/mde/granada_mdt_10m.tif"),
  output = here::here("data/variables/variables_relieve/mde/mde_breached.tif"),
  dist = 30,
  fill = TRUE
)

# 3. Cálculo del MDE "rellenado" (de depresiones por método de Wang and Liu - 2006)
whitebox::wbt_fill_depressions_wang_and_liu(
  dem = here::here("data/variables/variables_relieve/mde/mde_breached.tif"), 
  output = here::here("data/variables/variables_relieve/mde/mde_filled.tif")
)

# 4. Cálculo de la pendiente (en grados)
whitebox::wbt_slope(
  dem = here::here("data/variables/variables_relieve/mde/mde_filled.tif"), 
  output = here::here("data/variables/variables_relieve/slope/slope_grados.tif"), 
  units = "degrees"
)

# 5. Cálculo del Área de Contribución Específica (Specific Contributing Area, SCA)
whitebox::wbt_d_inf_flow_accumulation(
  input = here::here("data/variables/variables_relieve/mde/mde_filled.tif"), 
  output = here::here("data/variables/variables_relieve/twi/sca.tif")
)

# 6. Alineación de capas
# 6.1 Carga de las capas
slope <- terra::rast(
  here::here("data/variables/variables_relieve/slope/slope_grados.tif")
)

sca <- terra::rast(
  here::here("data/variables/variables_relieve/twi/sca.tif")
)

# 6.2. Alineación condicional
if(!terra::compareGeom(sca, slope, res = TRUE, crs = FALSE, stopOnError = FALSE)) {
  sca <- terra::resample(
    sca, 
    slope, 
    method = "near"
  )

  writeRaster(
    sca,
    here::here("data/variables/variables_relieve/twi/sca.tif"),
    overwrite = TRUE
  )
}

# 6.3. Limpieza de memoria
rm(slope, sca)
gc(verbose = FALSE, full = TRUE)
terra::tmpFiles(remove = TRUE)

# 7. Cálculo del TWI
whitebox::wbt_wetness_index(
  sca = here::here("data/variables/variables_relieve/twi/sca.tif"), 
  slope = here::here("data/variables/variables_relieve/slope/slope_grados.tif"), 
  output = here::here("data/variables/variables_relieve/twi/twi.tif")
)

# 8. Cálculo del Downslope TWI
# 8.1. Cálculo del Downslope Index
whitebox::wbt_downslope_index(
  dem = here::here("data/variables/variables_relieve/mde/mde_filled.tif"),
  output = here::here("data/variables/variables_relieve/slope/downslope_index.tif"),
  out_type = "tangent"
)

# 8.2. Cargar capas ráster
downslope_index <- terra::rast(
  here::here("data/variables/variables_relieve/slope/downslope_index.tif")
)

sca <- terra::rast(
  here::here("data/variables/variables_relieve/twi/sca.tif")
)

# 8.3. Cálcular Downslope TWI
twid <- log(sca / (downslope_index + 0.000001))

# 8.4. Guardado
terra::writeRaster(
  twid,
  here::here("data/variables/variables_relieve/twi/twid.tif"),
  overwrite = TRUE
)