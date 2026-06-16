# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))

pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

gc(verbose = FALSE, full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, terra, dplyr, here) 

# 2. Cargar área de estudio
aoi <- sf::st_read(
  here::here("data/presencias/ailanto_bbox.shp"),
  quiet = TRUE
)

# 3. Función de carga y recorte de las teselas 
fun_hfp <- function (input, output) {
  # 1. Generar mosaico virtual
  hfp <- terra::vrt( 
    list.files(
      path = here(input),
      pattern = "\\.tif$",
      full.names = TRUE
    )
  )

  # 2. Reproyectar AOI
  bbox <- aoi %>% 
    sf::st_transform(
      sf::st_crs(hfp)
    )

  # 3. Recortar al bounding box del AOI
  hfp_rec <- hfp %>% 
    terra::crop(bbox)

  # 4. Guardado
  terra::writeRaster(
    hfp_rec, 
    here::here(output), 
    overwrite = TRUE
  )

  # 5. Limpieza
  rm(hfp, bbox, hfp_rec)
  gc(full = TRUE)
}

# 4. Aplicar función
# fun_hfp(
#   input = "data/variables/variables_antropicas/hfp/hfp_2015",
#   output = "data/variables/variables_antropicas/hfp/hfp_2015.tif"
# )

fun_hfp(
  input = "data/variables/variables_antropicas/hfp/hfp_2018",
  output = "data/variables/variables_antropicas/hfp/hfp_2018.tif"
)

# fun_hfp(
#   input = "data/variables/variables_antropicas/hfp/hfp_2020",
#   output = "data/variables/variables_antropicas/hfp/hfp_2020.tif"
# )