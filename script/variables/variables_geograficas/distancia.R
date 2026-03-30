# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))

pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

gc(verbose = FALSE, full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, terra, dplyr, here)

# 2. Carga de datos
# 2.1. Capas vectoriales de los buffers
buffers <- list.files(
  path = here::here("data/variables/variables_geograficas"),
  pattern = "_buffer_.*\\.gpkg$",
  full.names = TRUE,
  recursive = TRUE
)

# 2.2. Plantilla raster
plantilla <- terra::rast(
  here::here("data/variables/variables_bioticas/basic/basic_lidar1.tif"), 
  lyrs = 1
)

# 3. Ráster de distancia
# 3.1. Creamos la función de cálculo del ráster de distancias
raster_distancia <- function(lista, template) {
  # Lista de resultados
  lista_resultados <- list()

  # Bucle de rasterización y cálculo del ráster de distancia
  for (i in lista) {
    # Cargamos la capa vectorial a procesar
    vect <- sf::st_read(here::here(i), quiet = TRUE)

    # Extraemos el nombre
    nombre <- tools::file_path_sans_ext(basename(i))

    # Extraemos las rutas de entrada y salida
    ruta <- dirname(i)
    ruta_raster <- file.path(ruta, paste0(nombre, ".tif"))
    ruta_distancia <- file.path(ruta, paste0(gsub("buffer", "distancia", nombre), ".tif"))

    # Rasterización
    vect_rast <- vect %>% 
      rasterize(
        template,
        field = 1,
        background = NA
      ) %>% 
      writeRaster(here::here(ruta_raster), overwrite = TRUE)

    # Creamos el ráster de distancia
    vect_distancia <- vect_rast %>% 
      distance() %>% 
      writeRaster(here::here(ruta_distancia), overwrite = TRUE)

    # Guardamos en la lista
    lista_resultados[[nombre]] <- c(
      raster = vect_rast,
      distancia = vect_distancia
    )

    # Limpiamos el entorno
    rm(vect, vect_rast)
    gc(full = TRUE)
    terra::tmpFiles(remove = TRUE)
  }

  # Definimos la salida de la función
  return(lista_resultados)
}

# 3.2. Aplicamos la función
resultados <- raster_distancia(
  lista = buffers, 
  template = plantilla
)