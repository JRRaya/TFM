# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, terra, dplyr)

# 2. Carga de datos
# 2.1. Ráster del MDE
mde <- terra::rast(
  here::here("data/variables/variables_relieve/mde/granada_mdt_10m.tif")
)

# 2.2. 'Bounding box' de nuestro ROI
roi_bbox <- sf::st_read(
  here::here("data/presencias/ailanto_bbox.shp")
) %>% 
  sf::st_transform("EPSG:25830")

# 3. Asignar CRS
terra::crs(mde) <- "EPSG:25830"

# 4. Guardado
terra::writeRaster(
  mde, 
  here::here("data/variables/variables_relieve/mde/mde.tif"),
  overwrite = TRUE
)