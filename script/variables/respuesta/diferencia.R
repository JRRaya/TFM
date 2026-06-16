# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, dplyr, here)

# 2. Carga de datos
# 2.1. Presencias 2008-2023
presencias_2008_2023 <-  sf::st_read(
  here::here("data/variables/respuesta/ailanthus_all.shp")
) %>% 
  dplyr::filter(
    Sp_dens_m2 != 0
  )

# 2.2. Presencias 2008
presencias_2008 <-  presencias_2008_2023 %>% 
  dplyr::filter(Year == 2008) %>% 
  sf::st_union() 

# 2.3. Presencias 2008
presencias_2015 <-  presencias_2008_2023 %>% 
  dplyr::filter(Year <= 2015) %>% 
  sf::st_union() 

# 2.4. Presencias 2008-2012
presencias_2008_2012 <- presencias_2008_2023 %>% 
  dplyr::filter(Year <= 2012) %>% 
  sf::st_union()

# 2.5. Presencias 2008-2014
presencias_2008_2015 <- presencias_2008_2023 %>% 
  dplyr::filter(Year <= 2015) %>% 
  sf::st_union()

# 2.6. Presencias 2008-2018
presencias_2008_2018 <- presencias_2008_2023 %>% 
  dplyr::filter(Year <= 2018) %>% 
  sf::st_union()

# 2.7. Presencias 2008-2020
presencias_2008_2020 <- presencias_2008_2023 %>% 
  dplyr::filter(Year <= 2020) %>% 
  sf::st_union()

# 3. Aplicar diferencia
# Obtención de los polígonos correspondientes a la invasión en el periodo de tiempo 2008-2023
# 3.1. Diferencia 2008-2023 
diferencia_2008_2023 <- presencias_2008_2023 %>% 
  sf::st_union() %>% 
  sf::st_difference(presencias_2008) %>% 
  sf::st_buffer(dist = -1)

diferencia_2008_2023 <- diferencia_2008_2023[!sf::st_is_empty(diferencia_2008_2023)] # Filtrar posibles geometrías vacías

# 3.2. Diferencia 2008-2012
diferencia_2008_2012 <- presencias_2008_2012 %>% 
  sf::st_union() %>% 
  sf::st_difference(presencias_2008) %>% 
  sf::st_buffer(dist = -1)

diferencia_2008_2012 <- diferencia_2008_2012[!sf::st_is_empty(diferencia_2008_2012)]

# 3.3. Diferencia 2015-2023
diferencia_2015_2023 <- presencias_2008_2023 %>% 
  sf::st_union() %>% 
  sf::st_difference(presencias_2008_2015) %>% 
  sf::st_buffer(dist = -1)

diferencia_2015_2023 <- diferencia_2015_2023[!sf::st_is_empty(diferencia_2015_2023)]

# 3.4. Diferencia 2018-2023
diferencia_2018_2023 <- presencias_2008_2023 %>% 
  sf::st_union() %>% 
  sf::st_difference(presencias_2008_2018) %>% 
  sf::st_buffer(dist = -1)

diferencia_2018_2023 <- diferencia_2018_2023[!sf::st_is_empty(diferencia_2018_2023)]

# 3.5. Diferencia 2020-2023
diferencia_2020_2023 <- presencias_2008_2023 %>% 
  sf::st_union() %>% 
  sf::st_difference(presencias_2008_2020) %>% 
  sf::st_buffer(dist = -1)

diferencia_2020_2023 <- diferencia_2020_2023[!sf::st_is_empty(diferencia_2020_2023)]

# 4. Guardado
# 4.1. Diferencias
sf::st_write(
  diferencia_2008_2023,
  here::here("data/variables/respuesta/diferencia_2008_2023.gpkg"),
  quiet = TRUE,
  delete_dsn = TRUE
)

sf::st_write(
  diferencia_2008_2012,
  here::here("data/variables/respuesta/diferencia_2008_2012.gpkg"),
  quiet = TRUE,
  delete_dsn = TRUE
)

sf::st_write(
  diferencia_2015_2023,
  here::here("data/variables/respuesta/diferencia_2015_2023.gpkg"),
  quiet = TRUE,
  delete_dsn = TRUE
)

sf::st_write(
  diferencia_2018_2023,
  here::here("data/variables/respuesta/diferencia_2018_2023.gpkg"),
  quiet = TRUE,
  delete_dsn = TRUE
)

sf::st_write(
  diferencia_2020_2023,
  here::here("data/variables/respuesta/diferencia_2020_2023.gpkg"),
  quiet = TRUE,
  delete_dsn = TRUE
)

# 4.2. Presencias
sf::st_write(
  presencias_2008,
  here::here("data/variables/respuesta/presencias_2008.gpkg"),
  quiet = TRUE,
  delete_dsn = TRUE
)

sf::st_write(
  presencias_2015,
  here::here("data/variables/respuesta/presencias_2015.gpkg"),
  quiet = TRUE,
  delete_dsn = TRUE
)

sf::st_write(
  presencias_2008_2012,
  here::here("data/variables/respuesta/presencias_2008_2012.gpkg"),
  quiet = TRUE,
  delete_dsn = TRUE
)

sf::st_write(
  presencias_2008_2015,
  here::here("data/variables/respuesta/presencias_2008_2015.gpkg"),
  quiet = TRUE,
  delete_dsn = TRUE
)

sf::st_write(
  presencias_2008_2018,
  here::here("data/variables/respuesta/presencias_2008_2018.gpkg"),
  quiet = TRUE,
  delete_dsn = TRUE
)

sf::st_write(
  presencias_2008_2020,
  here::here("data/variables/respuesta/presencias_2008_2020.gpkg"),
  quiet = TRUE,
  delete_dsn = TRUE
)

sf::st_write(
  presencias_2008_2023,
  here::here("data/variables/respuesta/presencias_2008_2023.gpkg"),
  quiet = TRUE,
  delete_dsn = TRUE
)