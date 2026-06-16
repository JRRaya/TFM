# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(lidR, rlas, future, here, terra, gstat, automap)

# 1.3. Configurar paralelización
# future::plan(multisession, workers = 8L) 
cl <- parallel::makeCluster(8L)
doParallel::registerDoParallel(cl)
lidR::set_lidr_threads(1L)

# 2. Normalización
# 2.1. Crear funicón de normalización
normalizacion <- function(input, output) {
  # 1. Lectura del catálogo
  ctg <- lidR::readLAScatalog(
    folder = here::here(input),
    chunk_size = 0,
    chunk_buffer = 30
  )
  
  # 2. Configurar salida de los archivos
  lidR::opt_output_files(ctg) <- file.path(output, "{ORIGINALFILENAME}_norm")
  
  # 3. Función de normalización
  ctg_normalized <- lidR::catalog_map(
    ctg = ctg,
    FUN = function(chunk) {
      # 3.1. Normalización de la tesela
      las_normalized <- lidR::normalize_height(
        chunk, 
        algorithm = knnidw()
      )
      
      # 3.2. Salida de la funicón
      return(las_normalized)
    }
  )
  
  # 4. Generar archivos de indexación 
  lidR::opt_output_files(ctg_normalized) <- ""
  for (f in ctg_normalized$filename) {
    rlas::writelax(f, verbose = FALSE)
  }
  
  # 5. Salida de la función
  return(invisible(ctg_normalized))
}

# 4. Aplicar normalización
normalizacion(input = "lidar1", output = "lidar1/norm")
normalizacion(input = "lidar2/thin", output = "lidar2/norm")
normalizacion(input = "lidar3/thin", output = "dlidar3/norm")

# 5. Cerrar paralelización
parallel::stopCluster(cl)