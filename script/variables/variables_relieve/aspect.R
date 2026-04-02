# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)
terra::tmpFiles(remove = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, terra, dplyr)

# 2. Carga de datos
mde <- terra::rast(
  here::here("data/variables/variables_relieve/mde/mde.tif")
)

# 3. Calcular 'aspect'
aspect <- mde %>% 
  terra::terrain(v = "aspect", unit = "degrees")

# 4. Clasificación
# 4.1. Crear matriz de clasificación
m <- matrix(
  c(-1,      0,      -1,  # Plano (Flat)
    0,       22.5,    1,  # Norte
    22.5,    67.5,    2,  # Noreste
    67.5,    112.5,   3,  # Este
    112.5,   157.5,   4,  # Sureste
    157.5,   202.5,   5,  # Sur
    202.5,   247.5,   6,  # Suroeste
    247.5,   292.5,   7,  # Oeste
    292.5,   337.5,   8,  # Noroeste
    337.5,   360,     1),  # Norte (cierre) 
  ncol = 3, 
  byrow = TRUE
)

# 4.2. Aplicar clasificación
aspect_clas <- aspect %>% 
  terra::classify(m)

# 4. Guardado
# 4.1. Aspect
terra::writeRaster(
  aspect,
  here::here("data/variables/variables_relieve/aspect/aspect.tif"),
  overwrite = TRUE
)

# 4.2. Aspect clasificado
terra::writeRaster(
  aspect_clas,
  here::here("data/variables/variables_relieve/aspect/aspect_clas.tif"),
  overwrite = TRUE
)