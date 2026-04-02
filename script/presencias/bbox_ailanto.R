# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, dplyr, here)

# 2. Carga de datos
ailanto <- sf::st_read(
  here::here("data/presencias/ailanthus_all.shp")
)

# 3. Extraer el 'bounding box'
# 3.1. Conversión a formato 'Shapefile' y reproyección a 'EPSG:4326'
bbox <- ailanto %>% 
  sf::st_bbox() %>% # Extraer el 'bounding box'
  sf::st_as_sfc() %>% # Convertir a geometría de polígono
  sf::st_transform("EPSG:4326") # Reproyectar a CRS 'EPSG:4326'

# 3.2. Conversión a formato 'WKT'
bbox_wkt <- bbox %>% 
  sf::st_as_text()

# 4. Guardado
# 4.1. Formato 'Shapefile'
sf::st_write(
  bbox,
  here::here("data/presencias/ailanto_bbox.shp"),
  quiet = TRUE,
  delete_dsn = TRUE
)

# 4.2. Formato 'WKT'
writeLines(
  bbox_wkt, 
  here::here("data/presencias/ailanto_bbox.wkt")
)