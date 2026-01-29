# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, terra, tidyverse, here, data.table, readxl, spatstat, onpoint, patchwork)

# 2. Carga de datos
ailanto <- st_read(here("data/ailanto/ailanthus_all.shp"))

# 3. Fusión de todos los polígonos de la capa en un único objeto multipolígono
ailanto <- ailanto %>% 
  st_union()

# 4. Reproyección a EPSG:4326 para extracción de datos LiDAR del IGN
if(sf::st_crs(ailanto) != sf::st_crs("EPSG:4326")) { 
  message("CRS distinto de EPSG:4326; reproyectando a EPSG:4326")
  ailanto <- sf::st_transform(ailanto, "EPSG:4326")
}

# 5. Corrección de posibles inconsistencias
if(!(all(sf::st_is_valid(ailanto)))) {
  message("La geometría está corrupta")
  ailanto <- sf::st_make_valid(ailanto)
}

# 6. Guardado de la capa
st_write(
  ailanto,
  here("data/ailanto/ailanto_join.shp"),
  append = TRUE,
  quiet = TRUE
)