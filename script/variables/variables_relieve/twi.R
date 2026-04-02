# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)
terra::tmpFiles(remove = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, terra, dplyr, whitebox)

# 2. Carga de datos
roi_bbox <- sf::st_read(
  here::here("data/presencias/ailanto_bbox.shp")
) %>% 
  sf::st_transform("EPSG:25830")

# 3. Cálculo del MDE "fisurado"
whitebox::wbt_breach_depressions_least_cost(
  here::here("data/variables/variables_relieve/mde/mde_edward.tif"),
  here::here("data/variables/variables_relieve/mde/mde_breached.tif"),
  dist = 30,
  fill = TRUE
)

# 4. Cálculo del MDE "rellenado" (de depresiones por método de Wang and Liu - 2006)
whitebox::wbt_fill_depressions_wang_and_liu(
  here::here("data/variables/variables_relieve/mde/mde_breached.tif"), 
  here::here("data/variables/variables_relieve/mde/mde_filled.tif")
)

# 5. Cálculo de la pendiente (en grados)
whitebox::wbt_slope(
  dem = here::here("data/variables/variables_relieve/mde/mde_filled.tif"), 
  output = here::here("data/variables/variables_relieve/slope/slope_grados.tif"), 
  units = "degrees"
)

# 6. Cálculo del Área de Contribución Específica (Specific Contributing Area, SCA)
whitebox::wbt_d8_flow_accumulation(
  input = here::here("data/variables/variables_relieve/mde/mde_filled.tif"), 
  output = here::here("data/variables/variables_relieve/twi/sca.tif")
)

# 7. Alineación de capas
# 7.1 Carga de las capas
slope <- terra::rast(
  here::here("data/variables/variables_relieve/slope/slope_grados.tif")
)

sca <- terra::rast(
  here::here("data/variables/variables_relieve/twi/sca.tif")
)

# 7.2. Alineación condicional
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

# 7.3. Limpieza de memoria
rm(slope, sca)
gc(verbose = FALSE, full = TRUE)
terra::tmpFiles(remove = TRUE)

# 8. Cálculo del TWI
whitebox::wbt_wetness_index(
  sca = here::here("data/variables/variables_relieve/twi/sca.tif"), 
  slope = here::here("data/variables/variables_relieve/slope/slope_grados.tif"), 
  output = here::here("data/variables/variables_relieve/twi/twi.tif")
)