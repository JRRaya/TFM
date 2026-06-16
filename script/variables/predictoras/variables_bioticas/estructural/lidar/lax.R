# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(lidR, rlas)

# 2. Especificar conjuntos de datos
catalogos_lidar <- c("lidar1", "lidar2", "lidar3")

# 3. Bucle de generación de índices .lax
for (catalogo in catalogos_lidar) {
  # 3.1. Lectura del catálogo
  ctg <- lidR::readLAScatalog(
    file.path("data/variables/lidar", catalogo, "norm/")
  )

  # 3.2. Generar archivos de indexación
  for (f in ctg$filename) {
    rlas::writelax(
      f, 
      verbose = FALSE
    )
  }
}