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

# 3. Obtener poligonos por periodos
# 3.1. Periodo 2008-2013
ailanto_2008_2013 <- ailanto %>% 
  dplyr::filter(Year <= 2013)

# 3.2. Periodo 2008-2013
ailanto_2014_2023 <- ailanto %>% 
  dplyr::filter(Year >= 2014) 

# 4. Guardado
# 4.1. Periodo 2008-2013
sf::st_write(
  ailanto_2008_2013, 
  here::here("data/presencias/ailanto_2008_2013.gpkg"), 
  quiet = TRUE, 
  delete_dsn = TRUE
)

# 4.2. Periodo 2008-2013
sf::st_write(
  ailanto_2014_2023, 
  here::here("data/presencias/ailanto_2014_2023.gpkg"), 
  quiet = TRUE, 
  delete_dsn = TRUE
)