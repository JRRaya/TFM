# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 2. Eliminar datos LiDAR innecesarios
# 2.1. LiDAR 1º vuelo
archivos_cir1 <- list.files(
  path = file.path("data/lidar/lidar1"),
  pattern = "CIR\\.laz$",
  full.names = TRUE
)

if(length(archivos_cir1) > 0) {
  file.remove(archivos_cir1)
}

# 2.2. LiDAR 2º vuelo
archivos_cir2 <- list.files(
  path = file.path("data/lidar/lidar2"),
  pattern = "IRC\\.laz$",
  full.names = TRUE
)

if(length(archivos_cir2) > 0) {
  file.remove(archivos_cir2)
}