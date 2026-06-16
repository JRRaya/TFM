# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, terra, dplyr)

# 2. Carga de datos
mde_teselas <- list.files(
  path = here::here("data/variables/variables_relieve/mde/teselas"),
  pattern = "^MDT02.*\\.tif$",
  full.names = TRUE
)

# 3. Crear mosaico
# 3.1. Generar 'SpatRasterCollection'
mde_coleccion <- terra::sprc(
  lapply(
    mde_teselas, 
    terra::rast
  )
)

# 3.2. Unir promediando en los solapes
mde <- terra::mosaic(
  mde_coleccion, 
  fun = "mean",
  filename = here::here("data/variables/variables_relieve/mde/mde.tif"),
  overwrite = TRUE,
  wopt = list(gdal = c("BIGTIFF=YES", "COMPRESS=LZW", "TILED=YES"))
)