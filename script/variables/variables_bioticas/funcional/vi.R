# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(verbose = FALSE, full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(terra, sf, dplyr, here)

# 1.3. Configurar archivos temporales de 'terra'
dir.create("E:/terra_temp", showWarnings = FALSE) # Creamos el directorio en un disco con espacio

terra::terraOptions(
  tempdir = "E:/terra_temp", 
  memfrac = 0.5,      # Usa solo el 50% de RAM para forzar a terra a ser ordenado
  todisk = TRUE      
)

# 2. Carga de datos
# 2.1. Establecer rutas a los mosaicos
rutas <- c(
  # "data/variables/variables_bioticas/funcional/rgb/orto_2007/orto_2007.tif",
  # "data/variables/variables_bioticas/funcional/rgb/orto_2013/orto_2013.tif",
  "data/variables/variables_bioticas/funcional/rgb/orto_2022/orto_2022.tif"
)

# 2.2. Rasterizar mosaicos
rasteres <- lapply(
  rutas,
  terra::rast
)

# 2.3. Generar lista de mosaicos
mosaicos <- list(
  # orto_2007 = rasteres[[1]],
  # orto_2013 = rasteres[[2]],
  orto_2022 = rasteres[[1]]
)

# 3. Cálculo de los índices
# 3.1. Creamos la función de cálculo iterativo
vi <- function (lista_mosaicos, rgb, ms) {
  # Creamos la lista donde almacenar los resultados
  resultados <- list()

  # Bucle de cálculo de índices
  for(i in seq_along(lista_mosaicos)) {
    # Extraemos el raster y el nombre de la ortofoto
    rast <- lista_mosaicos[[i]]

    nombre <- names(lista_mosaicos)[i]

    # Averiguar número de bandas
    n_bandas <- terra::nlyr(rast)    
    
    # Extraemos las bandas RGB
    R <- rast[[1]]
    G <- rast[[2]]
    B <- rast[[3]]

    # Calcular índices en función del número de bandas
    funciones <- if (n_bandas >= 4) ms else rgb

    # En caso de que haya >= 4 bandas, se extrae la banda del NIR
    if (n_bandas >= 4) NIR <- rast[[4]]
    
    # Lista donde almacenar los índices calculados
    indices <- list()

    # Bucle de cálculo iterativo
    for (j in names(funciones)) {
      # Ejecutamos la función pasando las bandas necesarias
      f <- funciones[[j]]

      indices[[j]] <- if (n_bandas >= 4) f(R, G, B, NIR) else f(R, G, B)
    }

    # Conversión de la lista en un stack multibanda
    multibanda <- terra::rast(indices)

    # Guardado en disco
    terra::writeRaster(
      multibanda,
      here::here(paste0("data/variables/variables_bioticas/funcional/rgb/", nombre, "/", nombre, "_vi.tif")),
      overwrite = TRUE,
      wopt = list(gdal = c("BIGTIFF=YES", "COMPRESS=LZW", "TILED=YES"))
    )

    # Guardado en la lista de resultados
    resultados[[nombre]] <- multibanda
    
    # Limpieza
    rm(rast, nombre, n_bandas, R, G, B, indices, multibanda)
    if(exists("NIR")) rm(NIR)
    gc(verbose = FALSE, full = TRUE)
  }

  # Limpieza final
  terra::tmpFiles(remove = TRUE)

  # Salida de la función
  return(resultados)
}

# 3.2. Creamos la lista de índices
indices_rgb <- list(
  VARI = function(R, G, B) (G - R) / (G + R - B),
  TGI  = function(R, G, B) {
    max_val <- max(terra::global(terra::rast(list(R, G, B)), "max", na.rm = TRUE))
    return((G - (0.39 * R) - (0.61 * B)) / max_val)
  }
)

indices_ms <- list(
  VARI   = function(R, G, B, NIR) (G - R) / (G + R - B),
  TGI    = function(R, G, B, NIR) {
    max_val <- max(terra::global(terra::rast(list(R, G, B)), "max", na.rm = TRUE))
    return((G - (0.39 * R) - (0.61 * B)) / max_val)
  },
  EVI = function(R, G, B, NIR) {
    2.5 * ((NIR - R) / (NIR + 6 * R - 7.5 * B + 1))
  },
  CVI = function(R, G, B, NIR) {
    (NIR * R) / (G^2)
  },  
  NDVI   = function(R, G, B, NIR) (NIR - R) / (NIR + R),
  SAVI   = function(R, G, B, NIR) ((NIR - R) * 1.5) / (NIR + R + 0.5),
  OSAVI  = function(R, G, B, NIR) (NIR - R) / (NIR + R + 0.16),
  GNDVI  = function(R, G, B, NIR) (NIR - G) / (NIR + G),
  BNDVI  = function(R, G, B, NIR) (NIR - B) / (NIR + B),
  MCARI1 = function(R, G, B, NIR) 1.2 * (2.5 * (NIR - R) - 1.3 * (G - R))
)

# 3.3. Aplicar función
indices_vegetacion <- vi(
  lista_mosaicos = mosaicos,
  rgb = indices_rgb,
  ms = indices_ms
)