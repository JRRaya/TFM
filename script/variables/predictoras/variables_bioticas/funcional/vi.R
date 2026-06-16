# 1. Configuración y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(verbose = FALSE, full = TRUE)
terra::tmpFiles(orphan = TRUE, old = TRUE, remove = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(terra, sf, dplyr, here, future, future.apply)

# 1.3. Configurar archivos temporales de 'terra'
terra::terraOptions(
  tempdir = "E:/jose/tmp", 
  memfrac = 0.5,      
  todisk = TRUE      
)

# 2. Carga de datos
rutas_mosaicos <- list(
  orto_2008 = here::here("data/variables/variables_bioticas/funcional/rgb/orto_2008/orto_2008.tif"),
  orto_2013 = here::here("data/variables/variables_bioticas/funcional/rgb/orto_2013/orto_2013.tif"),
  orto_2016 = here::here("data/variables/variables_bioticas/funcional/rgb/orto_2016/orto_2016.tif"),
  orto_2020 = here::here("data/variables/variables_bioticas/funcional/rgb/orto_2020/orto_2020.tif")
)

# 3. Índices de vegetación
indices_rgb <- list(
  VARI = function(R, G, B) {
    denom <- G + R - B
    resultado <- terra::ifel(abs(denom) < 0.001, NA, (G - R) / denom)
    terra::clamp(resultado, lower = -1, upper = 1, values = NA)
  },
  TGI = function(R, G, B) {
    max_val <- max(terra::global(terra::rast(list(R, G, B)), "max", na.rm = TRUE))
    (G - (0.39 * R) - (0.61 * B)) / max_val
  }
)

indices_ms <- list(
  VARI = function(R, G, B, NIR) {
    denom <- G + R - B
    resultado <- terra::ifel(abs(denom) < 0.001, NA, (G - R) / denom)
    terra::clamp(resultado, lower = -1, upper = 1, values = NA)
  },
  TGI = function(R, G, B, NIR) {
    max_val <- max(terra::global(terra::rast(list(R, G, B)), "max", na.rm = TRUE))
    (G - (0.39 * R) - (0.61 * B)) / max_val
  },
  EVI = function(R, G, B, NIR) {
    denom <- NIR + 6 * R - 7.5 * B + 1
    resultado <- terra::ifel(abs(denom) < 0.001, NA, 2.5 * ((NIR - R) / denom))
    terra::clamp(resultado, lower = -1, upper = 1, values = NA)
  },
  CVI    = function(R, G, B, NIR) (NIR * R) / (G^2),
  NDWI   = function(R, G, B, NIR) (G - NIR) / (G + NIR),
  NDVI   = function(R, G, B, NIR) (NIR - R) / (NIR + R),
  SAVI   = function(R, G, B, NIR) ((NIR - R) * 1.5) / (NIR + R + 0.5),
  OSAVI  = function(R, G, B, NIR) (NIR - R) / (NIR + R + 0.16),
  GNDVI  = function(R, G, B, NIR) (NIR - G) / (NIR + G),
  BNDVI  = function(R, G, B, NIR) (NIR - B) / (NIR + B),
  MCARI1 = function(R, G, B, NIR) 1.2 * (2.5 * (NIR - R) - 1.3 * (G - R))
)

# 4. Función de cálculo de índices
calcular_vi <- function(nombre, ruta, indices_rgb, indices_ms) {
  # 1. Cargar el raster dentro del worker
  rast <- terra::rast(ruta) / 255 # Normalización (0-1)

  # 2. Extraer bandas RGB del raster
  R <- terra::subset(rast, 1)
  G <- terra::subset(rast, 2)
  B <- terra::subset(rast, 3)

  # 3. Calcular índices en función del número de bandas
  funciones <- if (terra::nlyr(rast) >= 4) indices_ms else indices_rgb

  # 4. En caso de que haya >= 4 bandas, se extrae la banda del NIR
  if (terra::nlyr(rast) >= 4) NIR <- terra::subset(rast, 4)

  # 5. Calcular índices de vegetación
  indices <- lapply(
    names(funciones), 
    function(j) {
      f <- funciones[[j]]
      resultado <- if (terra::nlyr(rast) >= 4) f(R, G, B, NIR) else f(R, G, B)
      names(resultado) <- j
      resultado
    }
  )

  # 6. Generar stack índices
  multibanda <- terra::rast(indices)

  # 7. Construir ruta de salida
  ruta_salida <- here::here(paste0("data/variables/variables_bioticas/funcional/rgb/", nombre, "/", nombre, "_vi.tif"))

  # 8. Guardar multibanda
  terra::writeRaster(
    multibanda,
    ruta_salida,
    overwrite = TRUE,
    wopt = list(gdal = c("BIGTIFF=YES", "COMPRESS=LZW", "TILED=YES"))
  )

  # 9. Salida de la función
  ruta_salida
}

# 5. Procesado en paralelo
# 5.1. Definir número de workers a emplear
future::plan(future::multisession, workers = length(rutas_mosaicos))
on.exit(future::plan(future::sequential), add = TRUE)

# 5.2. Procesado en paralelo
rutas_vi <- future.apply::future_lapply(
  X   = names(rutas_mosaicos),
  FUN = function(nombre) {
    calcular_vi(
      nombre      = nombre,
      ruta        = rutas_mosaicos[[nombre]],
      indices_rgb = indices_rgb,
      indices_ms  = indices_ms
    )
  },
  future.seed    = NULL,
  future.globals = list(
    rutas_mosaicos = rutas_mosaicos,
    calcular_vi    = calcular_vi,
    indices_rgb    = indices_rgb,
    indices_ms     = indices_ms
  )
)

# 5.3. Asignar los nombres de los mosaicos a las rutas de salida construidas
names(rutas_vi) <- names(rutas_mosaicos)