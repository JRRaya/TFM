# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(verbose = FALSE, full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, here)

# 2. Carga de datos
roi <- sf::st_read(
  here::here("data/presencias/ailanto_bbox.shp"),
  quiet = TRUE
) %>% 
  sf::st_transform("EPSG:4326")

# 3. Guardado
sf::st_write(roi, "data/presencias/ailanto_bbox.geojson", driver = "GeoJSON", quiet = TRUE)