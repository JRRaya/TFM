# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, terra, dplyr, here)

# 2. Carga de datos
ailanto <- sf::st_read(here::here("data/presencias/ailanto/ailanthus_all.shp"))

# 3. Calcular estadisticos por año
resumen <- ailanto %>% 
  as.data.frame() %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(
    n_filas = n()
  ) %>% 
  dplyr::ungroup() 

print(resumen)