# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, terra, dplyr)

# 2. Carga de datos
# 2.1. Radiación total
radiacion_total <- terra::rast(
  here::here("data/variables/variables_relieve/radiacion/r_total.tif")
)

# 2.2. Radiación directa
radiacion_directa <- terra::rast(
  here::here("data/variables/variables_relieve/radiacion/r_directa.tif")
)

# 3. Asignar CRS
crs(radiacion_total) <- "EPSG:25830"

crs(radiacion_directa) <- "EPSG:25830"

# 4. Guardado
terra::writeRaster(
  radiacion_total,
  here::here("data/variables/variables_relieve/radiacion/radiacion_total.tif"),
  overwrite = TRUE
)

terra::writeRaster(
  radiacion_directa,
  here::here("data/variables/variables_relieve/radiacion/radiacion_directa.tif"),
  overwrite = TRUE
)