# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))

pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

gc(verbose = FALSE, full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, terra, dplyr, here) 

# 2. Carga de datos
# 2.1. Lista de capas vectoriales
# 2.1.1. Listar rutas de las capas vectoriales
vectoriales <- list.files(
  path = here::here("data/presencias"),
  pattern = "^presencias_.*\\.gpkg$",
  full.names = TRUE,
  recursive = TRUE
)

# 2.1.2. Aplanar lista
lista_vectoriales <- base::unlist(vectoriales)

# 2.2. Plantilla raster
plantilla <- terra::rast(
  here::here("data/variables/variables_relieve/radiacion/radiacion_directa.tif")
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
    ruta_raster <- file.path("data/variables/variables_bioticas/distancia_focos", paste0(nombre, ".tif"))
    ruta_distancia <- file.path("data/variables/variables_bioticas/distancia_focos", paste0("distancia_", nombre, ".tif"))

    # Rasterización
    vect_rast <- vect %>% 
      terra::rasterize(
        y = template,
        field = 1,
        background = NA
      ) %>%
      terra::writeRaster(
        filename = here::here(ruta_raster), 
        overwrite = TRUE,
        wopt = list(gdal = c("COMPRESS=LZW", "BIGTIFF=YES"), datatype = "INT1U")
      )

    # Creamos el ráster de distancia
    vect_distancia <- vect_rast %>% 
      terra::distance() %>%
      terra::writeRaster(
        filename = here::here(ruta_distancia), 
        overwrite = TRUE,
        NAflag = -9999,
        wopt = list(gdal = c("COMPRESS=LZW", "BIGTIFF=YES"), datatype = "FLT4S")
      )

    # Guardamos en la lista
    lista_resultados[[nombre]] <- list(
      raster = vect_rast,
      distancia = vect_distancia
    )

    # Limpiamos el entorno
    rm(vect, vect_rast, vect_distancia)
    gc(full = TRUE)
    terra::tmpFiles(remove = TRUE)
  }

  # Definimos la salida de la función
  return(lista_resultados)
}

# 3.2. Aplicamos la función
resultados <- raster_distancia(
  lista = lista_vectoriales, 
  template = plantilla
)