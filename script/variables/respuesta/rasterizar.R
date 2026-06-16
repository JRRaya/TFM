# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(terra, sf, dplyr, here)

# 2. Carga de datos
# 2.1. Presencias
# 2.1.1. Presencias 2008
presencias_2008 <- sf::st_read(
  here::here("data/presencias/presencias_2008.gpkg")
)

# 2.1.2. Presencias 2008
presencias_2015 <- sf::st_read(
  here::here("data/presencias/presencias_2015.gpkg")
)

# 2.1.3. Presencias 2008-2012
presencias_2008_2012 <- sf::st_read(
  here::here("data/presencias/presencias_2008_2012.gpkg")
)

# 2.1.4. Presencias 2008-2015
presencias_2008_2015 <- sf::st_read(
  here::here("data/presencias/presencias_2008_2015.gpkg")
)

# 2.1.5. Presencias 2008-2018
presencias_2008_2018 <- sf::st_read(
  here::here("data/presencias/presencias_2008_2018.gpkg")
)

# 2.1.6. Presencias 2008-2020
presencias_2008_2020 <- sf::st_read(
  here::here("data/presencias/presencias_2008_2020.gpkg")
)

# 2.2. Diferencias
# 2.2.1. Diferencia 2008-2023
diferencia_2008_2023 <- sf::st_read(
  here::here("data/presencias/diferencia_2008_2023.gpkg")
) 

# 2.2.2. Diferencia 2008-2012
diferencia_2008_2012 <- sf::st_read(
  here::here("data/presencias/diferencia_2008_2012.gpkg")
)

# 2.2.3. Diferencia 2014-2023
diferencia_2015_2023 <- sf::st_read(
  here::here("data/presencias/diferencia_2015_2023.gpkg")
)

# 2.2.4. Diferencia 2018-2023
diferencia_2018_2023 <- sf::st_read(
  here::here("data/presencias/diferencia_2018_2023.gpkg")
)

# 2.2.5. Diferencia 2020-2023
diferencia_2020_2023 <- sf::st_read(
  here::here("data/presencias/diferencia_2020_2023.gpkg")
)

# 2.3. Plantilla ráster
plantilla <- terra::rast(
  here::here("data/variables/variables_relieve/radiacion/radiacion_directa.tif")
)

# 3. Rasterizar
# 3.1. Periodo 2008
presencias_2008_rast <- presencias_2008 %>% 
  terra::rasterize(
    plantilla,
    field = 1,
    background = 0,
    filename = here::here("data/presencias/presencias_2008_rast.tif"), 
    overwrite = TRUE,
    wopt = list(gdal = c("COMPRESS=LZW", "BIGTIFF=YES"), datatype = "INT1U")
  )

# 3.2. Periodo 2015
presencias_2015_rast <- presencias_2015 %>% 
  terra::rasterize(
    plantilla,
    field = 1,
    background = 0,
    filename = here::here("data/presencias/presencias_2015_rast.tif"), 
    overwrite = TRUE,
    wopt = list(gdal = c("COMPRESS=LZW", "BIGTIFF=YES"), datatype = "INT1U")
  )

# 3.3. Periodo 2008-2023
diferencia_2008_2023_rast <- diferencia_2008_2023 %>% 
  terra::rasterize(
    plantilla,
    field = 1,
    background = 0,
    filename = here::here("data/presencias/diferencia_2008_2023_rast.tif"), 
    overwrite = TRUE,
    wopt = list(gdal = c("COMPRESS=LZW", "BIGTIFF=YES"), datatype = "INT1U")
  )

# 3.4. Periodo 2008-2012
diferencia_2008_2012_rast <- diferencia_2008_2012 %>% 
  terra::rasterize(
    plantilla,
    field = 1,
    background = 0,
    filename = here::here("data/presencias/diferencia_2008_2012_rast.tif"), 
    overwrite = TRUE,
    wopt = list(gdal = c("COMPRESS=LZW", "BIGTIFF=YES"), datatype = "INT1U")
  )

# 3.5. Periodo 2015-2023
diferencia_2015_2023_rast <- diferencia_2015_2023 %>% 
  terra::rasterize(
    plantilla,
    field = 1,
    background = 0
  ) %>% 
  terra::mask(
    presencias_2008_2015,
    inverse = TRUE,
    filename = here::here("data/presencias/diferencia_2015_2023_rast.tif"), 
    overwrite = TRUE,
    wopt = list(gdal = c("COMPRESS=LZW", "BIGTIFF=YES"), datatype = "INT1U")
  )

# 3.6. Periodo 2018-2023
diferencia_2018_2023_rast <- diferencia_2018_2023 %>% 
  terra::rasterize(
    plantilla,
    field = 1,
    background = 0
  ) %>% 
  terra::mask(
    presencias_2008_2018,
    inverse = TRUE,
    filename = here::here("data/presencias/diferencia_2018_2023_rast.tif"), 
    overwrite = TRUE,
    wopt = list(gdal = c("COMPRESS=LZW", "BIGTIFF=YES"), datatype = "INT1U")
  )

# 3.7. Periodo 2020-2023
diferencia_2020_2023_rast <- diferencia_2020_2023 %>% 
  terra::rasterize(
    plantilla,
    field = 1,
    background = 0
  ) %>% 
  terra::mask(
    presencias_2008_2020,
    inverse = TRUE,
    filename = here::here("data/presencias/diferencia_2020_2023_rast.tif"), 
    overwrite = TRUE,
    wopt = list(gdal = c("COMPRESS=LZW", "BIGTIFF=YES"), datatype = "INT1U")
  )