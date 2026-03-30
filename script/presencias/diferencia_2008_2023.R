# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, dplyr, here)

# 2. Carga de datos
# 2.1. Presencias 2008
presencias_2008 <-  sf::st_read(
  here::here("data/presencias/ailanto/presnecias_2008.gpkg")
)

# 2.2. Presencias 2023
presencias_2023 <-  sf::st_read(
  here::here("data/presencias/ailanto/presnecias_2023.gpkg")
)

# 3. Obtención de los polígonos correspondientes a la invasión en el periodo de tiempo 2008-2023
# 3.1. Aplicar diferencia
presencias_2008_2023 <- sf::st_difference(presencias_2023, presencias_2008)

# 3.2. Comprobación visual
mapview::mapview(presencias_2023) +
  mapview::mapview(presencias_2008) +
  mapview::mapview(presencias_2008_2023)

# 4. Guardado
sf::st_write(
  presencias_2008_2023,
  here("data/presencias/ailanto/presencias_2008_2023.gpkg"),
  quiet = TRUE,
  delete_dsn = TRUE
)