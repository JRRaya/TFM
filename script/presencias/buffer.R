# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
gc(verbose = FALSE, full = TRUE)
terra::tmpFiles(current = TRUE, orphan = TRUE, old = TRUE, remove = TRUE)
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(terra, sf, dplyr)

# 2. Carga de datos
ailanto <- sf::st_read(
  here::here("data/presencias/ailanthus_all.shp"), 
  quiet = TRUE
)

# 3. Buffer
ailanto_buffer_20 <- ailanto %>% 
  sf::st_buffer(dist = 20)

# 4. Guardado
sf::st_write(
  ailanto_buffer_20,
  here::here("data/presencias/ailanto_buffer_20.gpkg"),
  quiet = TRUE,
  delete_dsn = TRUE
)