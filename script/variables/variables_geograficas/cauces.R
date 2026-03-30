# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, terra, dplyr, here)

# 2. Carga de datos
# 2.1. Capa de carreteras (de Andalucía)
carreteras <- sf::st_read(
    here::here("data/variables/variables_geograficas/cauces/cauces.gpkg"),
    layer = "T03_01_Rio" 
  ) %>% 
  sf::st_transform(crs = "EPSG:25830")


# 2.2. Capa del bounding box de nuestro Área de Estudio
roi_bbox <- sf::st_read(here::here("data/presencias/ailanto/bbox_ailanto.shp")) %>% 
  sf::st_transform(crs = "EPSG:25830")

# 3. Buffer iterativo
# 3.1. Creamo el vector de distancias
distancias <- c(5, 10, 15, 20, 25, 50, 100, 200, 500, 1000)

# 3.2. Creamos la función de cálculo y guardado iterativo de los buffers
buffers <- function (carretera, distancia, ruta) {
  # Crear lista de buffers
  lista_buffers <- list()

  # Bucle de cálculo iterativo  
  for (i in distancia) {
    # Realizamos el buffer
    carretera_buffer <- carretera$Shape %>% 
      sf::st_buffer(dist = i) %>% # Aplicamos el buffer 
      sf::st_union() # Fusión de todos los polígonos (multipolígono)

    # Recortar a la extensión de nuestro ROI
    carretera_buffer <- carretera_buffer %>% 
      sf::st_intersection(roi_bbox)

    # Agregamos a la lista
    lista_buffers[[(i / 5)]] <- carretera_buffer

    # Guardado
    sf::st_write(
      carretera_buffer,
      here::here(paste0(ruta, i, ".gpkg")),
      quiet = TRUE,
      delete_dsn = TRUE
    )

    # Limpieza
    rm(carretera_buffer)
    gc(verbose = FALSE, full = TRUE)
  }

  # Salida de la función
  return(lista_buffers)
}

# 3.3. Calculamos los buffers
lista <- buffers(
  carretera = carreteras,
  distancia = distancias,
  ruta = "data/variables/variables_geograficas/cauces/cauces_buffer_"
)

# 4. Guardado
sf::st_write(
  carreteras,
  here::here("data/variables/variables_geograficas/cauces/cauces.gpkg"), 
  quiet = TRUE, 
  delete_dsn = TRUE
)