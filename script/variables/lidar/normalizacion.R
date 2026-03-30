# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, terra, stringr, dplyr, tidyr, tidyverse, trend, here, ggplot2, ggridges, viridis, patchwork, tidyterra, mapview, stars, whitebox, reticulate, rgbif, vegan, betapart, adespatial, iNEXT, SpadeR, lidR, lidRmetrics, gstat)
# devtools::install_github("ptompalski/lidRmetrics")

# 1.3. Configuración de `terra`: suprimir las barras de progreso
options(
  terra.progress = 0
)

# 1.4. Configuración de GDAL
Sys.setenv(GTIFF_SRS_SOURCE = "EPSG")
options(rgdal_show_exportToProj4_warnings = "none")

# 1.5. Forzar un solo hilo para evitar colisiones de archivos temporales
lidR::set_lidr_threads(1)

# 2. Normalización de los puntos de las teselas LiDAR
# 2.1. Especificar vuelos LiDAR
conjuntos_lidar <- c("lidar1", "lidar2", "lidar3")

# 2.2. Normalización iterativa de los datos LiDAR de cada uno de los vuelos
for (conjunto in conjuntos_lidar) {
  # Carga de teselas originales
  rutas_teselas <- list.files(
      path = file.path("data/variables/lidar", conjunto),
      pattern = "\\.laz$",
      full.names = TRUE
    )
  
  teselas <- lidR::readLAScatalog(
    folder = rutas_teselas, 
    select = "xyzcrn", 
    filter = "-drop_z_below 0"
  )
  
  errores <- las_check(teselas, print = TRUE)
  
  # Normalización de los puntos de las teselas LiDAR
  lidR::opt_output_files(teselas) <- {
    dir.create(here::here("data/lidar", conjunto, "norm"), recursive = TRUE, showWarnings = FALSE)
    file.path(here::here("data/lidar", conjunto, "norm"), "{ORIGINALFILENAME}_norm")
  }
  
  terra::(teselas) <- TRUE

  teselas_n <- lidR::normalize_height(teselas, tin())
  
  # Comprobación
  rutas_teselas_n <- list.files(
    path = file.path("data/lidar", conjunto, "norm"),
    pattern = "\\.laz$",
    full.names = TRUE
  )
  
  teselas_n <- lidR::readLAScatalog(
    folder = rutas_teselas_n, 
    select = "xyzcrn", 
    filter = "-drop_z_below 0"
  )
  
  errores_tin_n <- las_check(teselas_n, print = TRUE)
  
  # Limpieza de la memoria
  rm(teselas, teselas_n, rutas_teselas, rutas_teselas_n, errores, errores_tin_n)
  gc(full = TRUE)
  terra::tmpFiles(remove = TRUE)
}