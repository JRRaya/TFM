# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(lidR, rlas, future, here)

# 1.3. Configurar paralelización
# future::plan(multisession, workers = 8L) 
cl <- parallel::makeCluster(8L)
doParallel::registerDoParallel(cl)
lidR::set_lidr_threads(1L)

# 2. Diezmado homogeneo por pulso basada en rejilla
# 2.1. Función de diezmado
diezmado <- function (input, output, target, grid) {
  # 2.1.1. Lectura del catálogo
  ctg <- lidR::readLAScatalog(
    folder = here::here(input)
  )

  # 2.1.2. Configuración de 'lidR'
  lidR::opt_chunk_size(ctg) <- 0
  lidR::opt_chunk_buffer(ctg) <- 0
  lidR::opt_output_files(ctg) <- file.path(output, "{ORIGINALFILENAME}_thin")

  # 2.1.3. Aplicar la función a todo el catálogo
  ctg_thinned <- lidR::catalog_apply(
    ctg = ctg, 
    FUN = function(chunk) {      
      # 2.1.3.1. Añadir 'pulseID' a la tesela
      las_retrived <- lidR::retrieve_pulses(las)    

      # 2.1.3.2. Diezmado
      las_thinned <- lidR::decimate_points(
        las_retrived,
        algorithm = lidR::homogenize(density = target, res = grid, use_pulse = TRUE)
      )

      # 2.1.3.3. Salida de la función
      return(invisible(las_thinned))                    
    }
  )

  # 2.1.4. Generar archivos de indexación
  for (f in ctg_thinned$filename) {
    rlas::writelax(
      f, 
      verbose = FALSE
    )
  }

  # 2.1.5. Salida de la función
  return(ctg_thinned)
}

# 2.2. Aplicar diezmado
# 2.2.1. 2º vuelo
diezmado(
  input = "data/variables/lidar/lidar2",
  output = "data/variables/lidar/lidar2/diezmado",
  target = 0.5,
  grid = 5
)

# 2.2.2. 3º vuelo
diezmado(
  input = "data/variables/lidar/lidar3",
  output = "data/variables/lidar/lidar3/diezmado",
  target = 0.5,
  grid = 5
)

# 3. Cerrar clusterización
parallel::stopCluster(cl)