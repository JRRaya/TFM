# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(terra, dplyr, here)

# 2. Listamos las carpetas en las que están alojados las capas
lista_carpetas <- list.files(
  path = here::here("data/variables/variables_bioticas/vi"),
  full.names = FALSE
)

# 3. Promediado de las variables
for (i in lista_carpetas) {
  lista_archivos <- list.files(
    path = here::here("data/variables/variables_bioticas/vi", i),
    pattern = "201[89]|202[0-3]",
    full.names = TRUE,
    recursive = TRUE
  )

  # Comprobamos el número de carpetas listadas
  if (length(lista_archivos) < 6) { next }

  # Creamos la plantilla para el resto de rásteres
  plantilla <- lista_archivos[[1]] %>% 
    terra::rast() %>% 
    terra::project("EPSG:25830", method = "bilinear")

  # Cargamos el stack de capas (una por año)
  stack <- terra::rast(lapply(lista_archivos, function(x) {
    terra::resample(terra::rast(x), plantilla, method = "bilinear")
  }))

  # Media 
  mean <- stack %>%
    terra::project(
      "EPSG:25830", 
      method = "bilinear"
    ) %>% 
    terra::app(
      fun = mean, 
      na.rm = TRUE,
      filename  = file.path("data/variables/variables_bioticas/vi", i, paste0(i, "_mean.tif")),
      overwrite = TRUE
    )
  
  # Mediana
  median <- stack %>%
    terra::project(
      "EPSG:25830", 
      method = "bilinear"
    ) %>% 
    terra::app(
      fun = median, 
      na.rm = TRUE, 
      filename  = file.path("data/variables/variables_bioticas/vi", i, paste0(i, "_median.tif")),
      overwrite = TRUE
    )  

  # Limpieza
  rm(lista_archivos, mean, median)
  gc(full = TRUE)
  terra::tmpFiles(remove = TRUE)
}