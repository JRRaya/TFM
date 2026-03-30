# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(terra, sf, dplyr, here)

# 2. Carga de datos
# 2.1. Presencias
# 2.1.1. Periodo 2008-2013
ailanto_2008_2013 <-  sf::st_read(
  here::here("data/presencias/ailanto_2008_2013.gpkg")
)

# 2.1.2. Periodo 2014-2023
ailanto_2014_2023 <-  sf::st_read(
  here::here("data/presencias/ailanto_2014_2023.gpkg")
)

# 2.1.3. Periodo completo
ailanto_2008_2023 <-  sf::st_read(
  here::here("data/presencias/ailanthus_all.shp")
)

# 2.2. Plantilla ráster
plantilla <- terra::rast(
  here::here("data/variables/variables_bioticas/estructural/basic/basic_lidar1.tif"), 
  lyrs = 1
)

# 3. Rasterizar 
# 3.1. Periodo 2008-2013
ailanto_2008_2013_rast <- ailanto_2008_2013 %>% 
  terra::rasterize(
    plantilla,
    field = 1,
    background = 0
  )

# 3.2. Periodo 2014-2023
ailanto_2014_2023_rast <- ailanto_2014_2023 %>% 
  terra::rasterize(
    plantilla,
    field = 1,
    background = 0
  )

# 3.3. Serie completa
ailanto_2008_2023_rast <- ailanto_2008_2023 %>% 
  terra::rasterize(
    plantilla,
    field = 1,
    background = 0
  )

# 4. Excluir presencias del primer periodo
# Las áreas ya invadidas no entran dentro del análisis de diferencias en el hábitat
ailanto_2014_2023_rast <- ailanto_2014_2023_rast %>% 
  terra::mask(
    ailanto_2008_2013,
    inverse = TRUE
  )

# 5. Guardado
# 5.1. Periodo 2008-2013
terra::writeRaster(
  ailanto_2008_2013_rast,
  here::here("data/presencias/ailanto_2008_2013_rast.tif"), 
  overwrite = TRUE
)

# 5.2. Periodo 2014-2023
terra::writeRaster(
  ailanto_2014_2023_rast,
  here::here("data/presencias/ailanto_2014_2023_rast.tif"), 
  overwrite = TRUE
)

# 5.3. Serie completa
terra::writeRaster(
  ailanto_2008_2023_rast,
  here::here("data/presencias/ailanto_2008_2023_rast.tif"), 
  overwrite = TRUE
)
