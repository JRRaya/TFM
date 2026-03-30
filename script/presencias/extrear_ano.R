# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(verbose = FALSE, full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, dplyr, here)

# 2. Carga de datos
ailanto <-  sf::st_read(
  here::here("data/presencias/ailanthus_all.shp")
) %>% 
  dplyr::select(Year)

# 3. Filtrado (año 2018)
ailanto_2018 <- ailanto %>% 
  dplyr::filter(Year == 2018)

# 4. Guardado
sf::st_write(
  ailanto_2018, 
  here::here("data/presencias/ailanto_2018.shp"), 
  quiet = TRUE, 
  delete_dsn = TRUE
)