# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
# devtools::install_github("ptompalski/lidRmetrics")
pacman::p_load(terra, lidR, lidRmetrics, Rdimtools)

# 1.3. Configurar paralelización
# future::plan(multisession, workers = 8L) 
cl <- parallel::makeCluster(10L)
doParallel::registerDoParallel(cl)
lidR::set_lidr_threads(1L)

# 2. Calcular métricas LiDAR
# 2.1. Crear función de cálculo y guardado
metricas <- function (lista_metricas, lista_vuelos) {
  # 2.1.1. Bucle de procesado por métrica
  for (metrica in metricas) {

    # 2.1.2. Bucle de procesado por vuelo
    for (vuelo in lista_vuelos) { 
      # 2.1.2.1. Lectura del catálogo 
      ctg <- lidR::readLAScatalog(
        folder = list.files(
          path = file.path("data/variables/lidar", vuelo, "norm"),
          pattern = "\\.laz$",
          full.names = TRUE
        ), 
        filter = "-drop_z_below 0"
      )

      # 2.1.2.2. Cálculo de las métricas de dosel
      rast <- lidR::pixel_metrics(
        ctg,
        func = ~metrica,
        res = 10
      ) 

      # 2.1.2.3. Guardado
      terra::writeRaster(
        rast, 
        here::here(paste0("data/variables/variables_bioticas/", metrica, "/", metrica, "_", vuelo, ".tif")),
        overwrite = TRUE
      )
      
      # 2.1.2.4. Limpieza de la memoria
      rm(rast, ctg)
      gc(full = TRUE)
      terra::tmpFiles(remove = TRUE)
    }
  }
}

# 2.2. Lista de vuelos a procesar
conjuntos <- c("lidar1", "lidar2", "lidar3")

# 2.3. Lista de métricas a calcular
metricas <- list(
  # n (total number of returns), zmin, zmax, zmean, zvar, zsd, zcv, zskew, zkurt
  basic = lidRmetrics::metrics_basic(z = Z, zmin = 0.25),
  canopydensity = lidRmetrics::metrics_canopydensity(z = Z, interval_count = 5, zmin = 0.25),
  dispersion = lidRmetrics::metrics_dispersion(z = Z, dz = 5, zmin = 0.25, zmax = 30),
  echo = lidRmetrics::metrics_echo(ReturnNumber = ReturnNumber, NumberOfReturns = NumberOfReturns, z = Z, zmin = 0.25),
  echo2 = lidRmetrics::metrics_echo2(ReturnNumber = ReturnNumber, KeepReturns = c(1, 2, 3, 4, 5), z = Z, zmin = 0.25),
  fd = function(X,Y,Z) {
    M = cbind(X,Y,Z)
    Rdimtools::est.boxcount(M)$estdim
  },
  HOME = lidRmetrics::metrics_HOME(z = Z, i = i, zmin = 0.25),
  interval = lidRmetrics::metrics_interval(z = Z, zintervals = c(0, 0.5, 1.5, 2.5, 4, 10, 20), zmin = 0.25, right = FALSE),
  kde = lidRmetrics::metrics_kde(z = Z, bw = 1, zmin = 0.25, npeaks = 3),
  lad = lidRmetrics::metrics_lad(z = Z, zmin = 0.25, dz = 5, k = 0.5, z0 = 1),
  Lmoments = lidRmetrics::metrics_Lmoments(z = Z, zmin = 0.25),
  percabove = lidRmetrics::metrics_percabove(z = Z, threshold = c(2, 5, 10, 20), zmin = 0.25),
  percentiles = lidRmetrics::metrics_percentiles(z = Z, zmin = 0.25),
  rumple = lidRmetrics::metrics_rumple(x = X, y = Y, z = Z, pixel_size = 10, zmin = 0.25),
  texture = lidRmetrics::metrics_texture(x = X, y = Y, z = Z, pixel_size = 10, zmin = 0.25, chm_algorithm = lidR::kriging(), k = 50),
  voxels = lidRmetrics::metrics_voxels(x = X, y = Y, z = Z, vox_size = 5, zmin = 0.25)
)

# 3. Limpiar bandas vacías
here::here("data/variables/variables_bioticas/estructural/echo2/echo2_lidar1.tif") %>%
  terra::rast() %>% 
  terra::writeRaster(here::here("data/variables/variables_bioticas/estructural/echo2/echo2_lidar1_temp.tif"), overwrite = TRUE, gdal = c("BIGTIFF=YES", "COMPRESS=LZW", "TILED=YES"))

here::here("data/variables/variables_bioticas/estructural/echo2/echo2_lidar1_temp.tif") %>%
  terra::rast() %>% 
  terra::subset(c("n_return_1", "n_return_2", "n_return_3", "n_return_4")) %>%
  terra::writeRaster(here::here("data/variables/variables_bioticas/estructural/echo2/echo2_lidar1.tif"), overwrite = TRUE, gdal = c("BIGTIFF=YES", "COMPRESS=LZW", "TILED=YES"))

here::here("data/variables/variables_bioticas/estructural/echo2/echo2_lidar2.tif") %>%
  terra::rast() %>% 
  terra::writeRaster(here::here("data/variables/variables_bioticas/estructural/echo2/echo2_lidar2_temp.tif"), overwrite = TRUE, gdal = c("BIGTIFF=YES", "COMPRESS=LZW", "TILED=YES"))

here::here("data/variables/variables_bioticas/estructural/echo2/echo2_lidar2_temp.tif") %>%
  terra::rast() %>% 
  terra::subset(c("n_return_1", "n_return_2", "n_return_3", "n_return_4")) %>%
  terra::writeRaster(here::here("data/variables/variables_bioticas/estructural/echo2/echo2_lidar2.tif"), overwrite = TRUE, gdal = c("BIGTIFF=YES", "COMPRESS=LZW", "TILED=YES"))

# 3. Cerrar clusterización
parallel::stopCluster(cl)