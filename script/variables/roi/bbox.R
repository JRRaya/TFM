# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, terra, stringr, dplyr, tidyr, tidyverse, trend, here, ggplot2, ggridges, viridis, patchwork, tidyterra, mapview, stars, whitebox, reticulate, rgbif, vegan, betapart, adespatial, iNEXT, SpadeR, lidR, lidRmetrics, gstat)
# devtools::install_github("ptompalski/lidRmetrics")

# 1.3. Configuración de `terra`: suprimir las barras de progreso
options(
  terra.progress = 0
)

# 2. Carga de datos
ailanto <- st_read(
  here("data/ailanto/ailanthus_all.shp")
)

# 3. Extracción del boundingbox de nuestro AOI
bbox <- st_bbox(ailanto)

# 4. Creación del objeto vectorial del bbox
# 4.1. Creación del objeto
bbox_sfc <- st_as_sfc(bbox)

# 4.2. Convertir sfc a un objeto sf
# Le asignamos un ID genérico para que tenga al menos un atributo
bbox_sf <- st_sf(ID = 1, geometry = bbox_sfc)

# 4.2. Comprobación visual
plot(bbox_sf)

# 5. Reproyección y corrección
# 5.1. Reproyección a EPSG:4326 para extracción de datos LiDAR del IGN
if(sf::st_crs(bbox_sf) != sf::st_crs("EPSG:4326")) { 
  message("CRS distinto de EPSG:4326; reproyectando a EPSG:4326")
  bbox_sf <- sf::st_transform(bbox_sf, "EPSG:4326")
}

# 5.2. Corrección de posibles inconsistencias
if(!(all(sf::st_is_valid(bbox_sf)))) {
  message("La geometría está corrupta")
  bbox_sf <- sf::st_make_valid(bbox_sf)
}

# 6. Guardado de la capa
st_write(
  bbox_sf,
  here("data/ailanto/bbox_ailanto.shp"),
  append = TRUE,
  quiet = TRUE
)