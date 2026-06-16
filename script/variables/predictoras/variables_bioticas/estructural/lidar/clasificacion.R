# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(lidR, future, purrr)

# 1.3. Configurar paralelización
future::plan(multisession, workers = 10L) 
lidR::set_lidr_threads(1L)

# 2. Conjuntos de datos y parámetros asociados
configuraciones <- list(
  "lidar1" = list(res = 20, angle = 20, dist = 1.5, sor_k = 5,  sor_sig = 4), 
  "lidar2" = list(res = 10, angle = 25, dist = 1.0, sor_k = 8,  sor_sig = 3), 
  "lidar3" = list(res = 5,  angle = 30, dist = 0.5, sor_k = 10, sor_sig = 3)  
)

# 3. Clasificación de puntos de suelo
# 3.1. Función de clasificación
clasificacion <- function (parametros, catalogo) {
  # 3.1.1. Lectura del catálogo
  ctg <- lidR::readLAScatalog(
    file.path("data/variables/lidar", catalogo)
  )

  # 3.1.2. Configuración del motor de 'lidR'
  lidR::opt_chunk_size(ctg)   <- 500
  lidR::opt_chunk_buffer(ctg) <- 30
  lidR::opt_output_files(ctg) <- paste0("data/variables/lidar/", catalogo, "/clas/{XLEFT}_{YBOTTOM}_clas")

  # 3.1.3. Clasificar ruido
  ctg <- lidR::classify_noise(
    ctg, 
    sor(
      k = parametros$sor_k, 
      sig = parametros$sor_sig
    )
  )

  # 3.1.4. Filtrado de puntos ruidosos
  lidR::opt_filter(ctg) <- "-drop_class 7 18"

  # 3.1.5. Clasificar puntos de suelo
  lidR::classify_ground(
    ctg, 
    ptd(
      res = parametros$res, 
      angle = parametros$angle, 
      distance = parametros$dist
    )
  )

  # 3.1.6. Liberar memoria
  rm(ctg)
  gc(full = TRUE)

  return(invisible(NULL))
}

# 3.2. Clasificación segura
purrr::iwalk(
  configuraciones, 
  ~clasificacion(.y, .x)
)

purrr::iwalk(configuraciones, \(parametros, catalogo) {
  clasificacion(parametros, catalogo)
})

# 4. Cierre de la paralelización
future::plan(sequential)