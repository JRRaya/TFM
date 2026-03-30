# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
# devtools::install_github("ptompalski/lidRmetrics")
pacman::p_load(terra, lidR, lidRmetrics)

# 1.3. Configuración de `terra`: suprimir las barras de progreso
options(
  terra.progress = 0
)

# 1.4. Configuración de GDAL
Sys.setenv(GTIFF_SRS_SOURCE = "EPSG")
options(rgdal_show_exportToProj4_warnings = "none")

# 1.5. Forzar un solo hilo para evitar colisiones de archivos temporales
lidR::set_lidr_threads(1)

# 2. Especificar vuelos LiDAR
conjuntos_lidar <- c("lidar1", "lidar2", "lidar3")

# 3. Cálculo de los interval de la altura a nivel de píxel para cada uno de los conjuntos de datos LiDAR
for (conjunto in conjuntos_lidar) {
  # Carga de teselas 
  teselas_n <- lidR::readLAScatalog(
    folder = list.files(
      path = file.path("data/variables/lidar", conjunto, "norm"),
      pattern = "\\.laz$",
      full.names = TRUE
    ), 
    filter = "-drop_z_below 0"
  )

  # Cálculo de las métricas de dosel
  interval <- lidR::pixel_metrics(
    teselas_n,
    func = ~lidRmetrics::metrics_interval(
      z = Z, 
      zintervals = c(0, 0.5, 1.5, 2.5, 4, 10, 20)
      zmin = 0.5,
      right = FALSE
    ),
    res = 10
  ) %>% 
    terra::crop(roi) %>% 
    terra::mask(roi)

  # Guardado
  terra::writeRaster(
    interval, 
    here::here(paste0("data/variables/variables_bioticas/interval/interval_", conjunto, ".tif"))
  )
  
  # Limpieza de la memoria
  rm(interval, teselas_n)
  gc(full = TRUE)
  terra::tmpFiles(remove = TRUE)
}