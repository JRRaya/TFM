# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(terra, dplyr, here)

# 2. Listamos las carpetas en las que están alojados las capas
lista_carpetas <- list.files(
  path = here::here("data/variables/variables_bioticas"),
  full.names = FALSE
)

# 3. Promediado de las variables
for (i in lista_carpetas) {
  # Listamos las capas a promediar
  lista_archivos <- list.files(
    path = file.path("data/variables/variables_bioticas", i),
    pattern = "_lidar[12]\\.tif$",
    full.names = TRUE,
    recursive = TRUE
  )

  # Comprobamos el número de capas listadas
  if (length(lista_archivos) < 2) { next }

  # Media 
  lista_archivos %>%
    lapply(function(x) {
      r <- terra::rast(x)
      if (!terra::same.crs(r, terra::crs(terra::rast(lista_archivos[1])))) terra::project(r, terra::crs(terra::rast(lista_archivos[1])))
      else r
    }) %>%
    terra::rast() %>%
    terra::tapp(
      fun = mean,
      index = rep(seq(1, terra::nlyr(terra::rast(lista_archivos[1]))), 2)
    ) %>%
    (\(r) { terra::set.names(r, terra::names(terra::rast(lista_archivos[1]))); r })() %>%
    terra::writeRaster(
      filename = file.path("data/variables/variables_bioticas", i, paste0(i, "_mean.tif")),
      overwrite = TRUE
    )
  
  # Mediana
  lista_archivos %>%
    lapply(function(x) {
      r <- terra::rast(x)
      if (!terra::same.crs(r, terra::crs(terra::rast(lista_archivos[1])))) terra::project(r, terra::crs(terra::rast(lista_archivos[1])))
      else r
    }) %>%
    terra::rast() %>%
    terra::tapp(
      fun = mean,
      index = rep(seq(1, terra::nlyr(terra::rast(lista_archivos[1]))), 2)
    ) %>%
    (\(r) { terra::set.names(r, terra::names(terra::rast(lista_archivos[1]))); r })() %>%
    terra::writeRaster(
      filename = file.path("data/variables/variables_bioticas", i, paste0(i, "_median.tif")),
      overwrite = TRUE
    )

  # Limpieza
  rm(lista_archivos)
  gc(full = TRUE)
  terra::tmpFiles(remove = TRUE)
}