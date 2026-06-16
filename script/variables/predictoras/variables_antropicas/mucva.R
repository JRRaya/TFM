# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
gc(verbose = FALSE, full = TRUE)
# terra::tmpFiles(current = TRUE, orphan = TRUE, old = TRUE, remove = TRUE)
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, terra, dplyr, here)

# 2. Carga de datos
# 2.1. Capa vectorial del MUCVA
mucva <- sf::st_read(
  dsn = here::here("data/variables/variables_geograficas/mucva/MUCVA25_2007.gpkg"),
  layer = "MUCVA25_07_EscalaSintetica",
  quiet = TRUE
) %>% 
  dplyr::select(U_NIVEL1) %>% 
  sf::st_transform("EPSG:25830") 

# 2.2. Capa vectorial del área de estudio
roi <- sf::st_read(
  here::here("data/presencias/ailanto_bbox.shp"),
  quiet = TRUE
) %>% 
  sf::st_transform("EPSG:25830")

# 2.3. Plantilla raster
plantilla <- terra::rast(
  here::here("data/variables/variables_relieve/mde/mde.tif")
) %>% 
  terra::project("EPSG:25830") %>% 
  terra::crop(terra::vect(roi)) %>% 
  terra::mask(terra::vect(roi))

# 3. Recorte a la extensión del árae de estudio
mucva <- mucva %>% 
  sf::st_crop(roi)

# 4. Rasterizar
mucva_rast <- mucva %>% 
  terra::rasterize(
    y = plantilla,
    field = "U_NIVEL1" 
  ) 

# 5. Guardado
terra::writeRaster(
  mucva_rast,
  here::here("data/variables/variables_geograficas/mucva/mucva_2007.tif"),
  overwrite = TRUE
)