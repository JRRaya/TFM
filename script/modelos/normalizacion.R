# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)
terra::tmpFiles(remove = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(terra, sf, dplyr, here)

# 2. Crear datasets de variables a normalizar
# 2.1. Presencias: Rásteres binarios (0, 1) de píxeles invadidos (o no) en un periodo dado
presencias_2008_2013 <- here::here("data/presencias/ailanto_2008_2013_rast.tif")

presencias_2014_2023 <- here::here("data/presencias/ailanto_2014_2023_rast.tif")

presencias_2008_2023 <- here::here("data/presencias/ailanto_2008_2023_rast.tif")

# 2.2. Variables predictoras
# 2.2.1. Variables bióticas
# 2.2.1.1. Variables funcionales (índices de vegetación)
variables_funcionales_2008_2013 <- list(
  here::here("data/variables/variables_bioticas/funcional/rgb/orto_2007/orto_2007_vi.tif")
)

variables_funcionales_2014_2023 <- list(
    list.files(
    path = here::here("data/variables/variables_bioticas/funcional/sentinel"),
    pattern = "\\.tif$",
    full.names = TRUE,
    recursive = TRUE
  ),
  here::here("data/variables/variables_bioticas/funcional/rgb/orto_2007/orto_2013_vi.tif")
)
variables_funcionales_2008_2023 <- list(
  here::here("data/variables/variables_bioticas/funcional/rgb/orto_2007/orto_2007_vi.tif")
)

# 2.2.1.2. Variables estructurales (métricas derivadas de datos LiDAR)
variables_estructurales <- list.files(
  path = here::here("data/variables/variables_bioticas/estructural"),
  pattern = "_mean\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

# 2.3. Variables geográficas
variables_geograficas <- list.files(
  path = here::here("data/variables/variables_geograficas"),
  pattern = "(cauces|nucleos)_distancia_20\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

# 2.4. Variables del relieve
variables_relieve <- list(
  c(
    "data/variables/variables_relieve/radiacion/radiacion_total.tif",
    "data/variables/variables_relieve/radiacion/radiacion_directa.tif",
    "data/variables/variables_relieve/mde/mde.tif",
    "data/variables/variables_relieve/slope/slope_grados.tif",
    "data/variables/variables_relieve/twi/sca.tif",
    "data/variables/variables_relieve/twi/twi.tif",
    "data/variables/variables_relieve/tpi/tpi.tif",
    "data/variables/variables_relieve/tri/tri.tif",
    "data/variables/variables_relieve/aspect/aspect.tif",
    "data/variables/variables_relieve/aspect/aspect_clas.tif"
  )
)

# 2.5. Datasets de variables a normalizar
lista_2008_2013 <- unlist( # Aplanar lista
    list(
    presencias_2008_2013, 
    variables_funcionales_2008_2013, 
    variables_geograficas, 
    variables_relieve
  )
)

lista_2014_2023 <- unlist(
  list(
    presencias_2014_2023, 
    variables_funcionales_2014_2023, 
    variables_estructurales, 
    variables_geograficas, 
    variables_relieve
  )
)

lista_2008_2023 <- unlist( 
    list(
    presencias_2008_2023, 
    variables_funcionales_2008_2023, 
    variables_geograficas, 
    variables_relieve
  )
)

# 3. Creamos la función de normalización de las varibales
normalizacion <- function(lista, ruta) {
  # Cargar 1º elemento de la lista (plantilla)
  plantilla <- lista[[1]] %>% 
    terra::rast() %>% 
    terra::project("EPSG:25830", method = "bilinear")

  # Cargar el ROI
  roi <- sf::st_read(here::here("data/variables/roi/roi.gpkg"), quiet = TRUE) %>% 
    sf::st_transform(crs = "EPSG:25830")

  # Creamos la lista donde almacenar los rásteres normalizados
  lista_n <- vector("list", length(lista))

  # Iterar sobre cada elemento de la lista
  for(i in seq_along(lista)) {
    # Cargar el ráster
    rast <- lista[[i]] %>% 
      terra::rast()

    # Reproyecctar, remuestrear y recortar
    lista_n[[i]] <- rast %>% 
      terra::project("EPSG:25830", method = "near") %>% 
      terra::resample(plantilla) %>% 
      terra::crop(roi) %>% 
      terra::mask(roi)

    # Asignación dinámica de nombres en una sola línea
    nom <- tools::file_path_sans_ext(basename(lista[[i]]))
    names(lista_n[[i]]) <- if(terra::nlyr(rast) == 1) nom else paste0(nom, "_", names(rast))

    # Liberar memoria
    rm(rast, nom)
    gc(full = TRUE)
  }

  # Rasterización de la lista
  lista_n_rast <- lista_n %>% 
    terra::rast()

  # Guardado en disco
  terra::writeRaster(lista_n_rast, ruta, overwrite = TRUE)

  # Limpiar entorno
  rm(plantilla, roi)
  gc(full = TRUE)

  # Salida de la función
  return(lista_n)
}

# 4. Crear stacks de variables normalizadas
variables_2008_2013 <- normalizacion(
  lista = lista_2008_2013,
  ruta = here::here("data/variables/variables_2008_2013.tif")
)

variables_2014_2023 <- normalizacion(
  lista = lista_2014_2023,
  ruta = here::here("data/variables/variables_2014_2023.tif")
)

variables_2008_2023 <- normalizacion(
  lista = lista_2008_2023,
  ruta = here::here("data/variables/variables_2008_2023.tif")
)