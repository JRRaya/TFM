# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
gc(verbose = FALSE, full = TRUE)
terra::tmpFiles(current = TRUE, orphan = TRUE, old = TRUE, remove = TRUE)
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(terra, sf, dplyr)

# 2. Carga de datos
# 2.1. Buffer (de 20 m) de los polígonos de presencia de ailanto de toda la serie (2008-2023)
ailanto <- sf::st_read(
  here::here("data/presencias/ailanto_buffer_20.gpkg"), 
  quiet = TRUE
) %>% 
  sf::st_union()

# 2.2. Buffer (de 20 m) de las líneas de carreteras del área de estudio
carreteras <- sf::st_read(
  here::here("data/variables/variables_geograficas/carreteras/carreteras_buffer_20.gpkg"), 
  quiet = TRUE
) %>% 
  sf::st_union()

# 3. Unión de las figuras de ambas capas
roi <- sf::st_union(
  ailanto,
  carreteras
)

# 4. Guardado
sf::st_write(
  roi,
  here::here("data/variables/roi/roi.gpkg"),
  quiet = TRUE,
  delete_dsn = TRUE
)