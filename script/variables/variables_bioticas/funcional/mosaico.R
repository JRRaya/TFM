# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(verbose = FALSE, full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(terra, sf, dplyr, here)

# 1.3. Configuración de archivos de 'terra'
dir.create(here::here("data/tmp"), showWarnings = FALSE, recursive = TRUE)
terra::terraOptions(
  tempdir  = here::here("data/tmp"),
  memfrac  = 0.6   
)

# 2. Carga de datos
# 2.1. Ortofotografía 2007
orto_2007 <- list.files(
  path = here::here("data/variables/variables_bioticas/funcional/rgb/orto_2007"),
  pattern = "\\.jp2$",
  full.names = TRUE
)

# 2.2. Ortofotografía 2013
orto_2013 <- list.files(
  path = here::here("data/variables/variables_bioticas/funcional/rgb/orto_2013"),
  pattern = "\\.tif$",
  full.names = TRUE
)

# 2.3. Ortofotografía 2022
orto_2022 <- list.files(
  path = here::here("data/variables/variables_bioticas/funcional/rgb/orto_2022"),
  pattern = "\\.tif$",
  full.names = TRUE
)

# 3. Mosaicar
# 3.1. Creamos la lista de listas de archivos a mosaicar
lista <- list(
  orto_2007 = orto_2007,
  orto_2013 = orto_2013,
  orto_2022 = orto_2022
)

# 3.2. Función de generación del mosaico
mosaicar <- function(list) {
  # Creamos la lista donde almacenar los mosaicos
  lista_mosaicos <- list()

  # Bucle de generación de mosaicos
  for (orto in names(list)) {
    # Creamos un 'SpatRasterCollection'
    coleccion <- terra::sprc(
      lapply(
        lista[[orto]], 
        terra::rast
      )
    )

    # Unir promediando en los solapes
    mosaico <- terra::mosaic(
      coleccion, 
      fun = "mean",
      filename = here::here(paste0("data/variables/variables_bioticas/funcional/rgb/", orto, "/", orto, ".tif")),
      overwrite = TRUE,
      wopt = list(gdal = c("BIGTIFF=YES", "COMPRESS=LZW", "TILED=YES"))
    )

    # Agregar a la lista
    lista_mosaicos[[orto]] <- mosaico

    # Limpieza
    rm(orto, coleccion, mosaico)
    gc(verbose = FALSE, full = TRUE)
    unlink(list.files(here::here("data/tmp"), full.names = TRUE))
  }

  # Salida de la función
  return(lista_mosaicos)
}

# 3.3. Aplicar la función
mosaicos <- mosaicar(lista)