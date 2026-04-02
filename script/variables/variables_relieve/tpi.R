# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)
terra::tmpFiles(remove = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, terra, dplyr)

# 2. Carga de datos
mde <- terra::rast(
  here::here("data/variables/variables_relieve/mde/mde.tif")
)

# 3. Calcular Topographic Position Index (TPI)
tpi <- mde %>% 
  terra::terrain(v = "TPI", unit = "degrees")

# 4. Guardado
terra::writeRaster(
  tpi,
  here::here("data/variables/variables_relieve/tpi/tpi.tif"),
  overwrite=TRUE
)