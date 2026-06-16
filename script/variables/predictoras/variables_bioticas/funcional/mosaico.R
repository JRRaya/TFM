# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(verbose = FALSE, full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(terra, sf, dplyr, here, future.apply)

# 1.3. Configuración de archivos temporales de 'terra'
# 1.3.1. Crear directorio de archivos remporales
dir.create(here::here("tmp"), showWarnings = FALSE, recursive = TRUE)

# 1.3.2. Configurar directorio y empleo de RAM
terra::terraOptions(
  tempdir = here::here("tmp"),
  memfrac = 0.5
)

# 1.4. Configuración de variables de entorno
gdal_env <- list(
  GDAL_DATA = "C:/OSGeo4W/apps/gdal/share/gdal",
  GDAL_DRIVER_PATH = "C:/OSGeo4W/apps/gdal/lib/gdalplugins",
  PATH = paste0("C:/OSGeo4W/bin;", Sys.getenv("PATH"))
)
do.call(Sys.setenv, gdal_env)

# 2. Carga de las rutas
lista <- list(
  orto_2008 = list.files(here::here("data/variables/variables_bioticas/funcional/rgb/orto_2008/teselas"), pattern = "\\.jp2$",  full.names = TRUE),
  orto_2013 = list.files(here::here("data/variables/variables_bioticas/funcional/rgb/orto_2013/teselas"), pattern = "\\.ecw$",  full.names = TRUE),
  orto_2016 = list.files(here::here("data/variables/variables_bioticas/funcional/rgb/orto_2016/teselas"), pattern = "\\.jp2$",  full.names = TRUE),
  orto_2020 = list.files(here::here("data/variables/variables_bioticas/funcional/rgb/orto_2020/teselas"), pattern = "\\.ecw$",  full.names = TRUE)
)

# 3. Conversión ECW -> TIF
# 3.1. Función de conversión de formato (ECW -> TIF)
ecw_a_tif <- function(ecw_files, gdal_env) {
  tif_files <- sub("\\.ecw$", ".tif", ecw_files)

  future::plan(future::multisession, workers = 8L)
  on.exit(future::plan(future::sequential), add = TRUE)

  future.apply::future_lapply(
    X = seq_along(ecw_files),
    FUN = function(i) {
      do.call(Sys.setenv, gdal_env)
      system2(
        command = "C:/OSGeo4W/bin/gdal_translate.exe",
        args = c(
          "-of", "GTiff",
          "-co", "COMPRESS=LZW",
          "-co", "BIGTIFF=YES",
          "-co", "TILED=YES",
          shQuote(ecw_files[i]),
          shQuote(tif_files[i])
        ),
        wait = TRUE
      )
    },
    future.seed = NULL
  )

  tif_files
}

# 3.2. Convertir archivos
lista[["orto_2013"]] <- ecw_a_tif(lista[["orto_2013"]], gdal_env)
lista[["orto_2020"]] <- ecw_a_tif(lista[["orto_2020"]], gdal_env)

# 4. Mosaicar
# 4.1. Función de mosaicar
mosaicar <- function (orto, archivos) {
  # 1. Calcular factor de agregación 
  fact <- round(10 / terra::res(terra::rast(archivos[[1]]))[1])
  
  # 2. Agregar cada tesela a la resolución objetivo
  teselas_agregadas <- lapply(
    archivos, 
    function(archivo) {
      terra::aggregate(
        terra::rast(archivo),
        fact = fact,
        fun = "mean",
        na.rm = TRUE
      )
    }
  )
  
  # 3. Mosaicar y escribir a disco
  mosaico <- terra::mosaic(
    terra::sprc(teselas_agregadas),
    fun = "mean",
    filename = here::here(paste0("data/variables/variables_bioticas/funcional/rgb/", orto, "/", orto, ".tif")),
    overwrite = TRUE,
    wopt = list(gdal = c("BIGTIFF=YES", "COMPRESS=LZW", "TILED=YES"))
  )
  
  # 4. Salida de la función
  return(here::here(paste0("rgb/", orto, "/", orto, ".tif")))
}

# 4.2. Paralelización del mosaicado
# 4.2.1. Especificar workers a emplear
future::plan(future::multisession, workers = length(lista))
on.exit(future::plan(future::sequential)) # Definir comportamiento al finalizar el proceso

# 4.2.2. Procesado en paralelo de cada año
rutas <- future.apply::future_lapply(
  X = names(lista),
  FUN = function(orto) mosaicar(orto, lista[[orto]]),
  future.seed = NULL,
  future.globals = list(
    lista = lista,
    mosaicar = mosaicar
  )
)

# 4.2.3. Asignar nombres
names(rutas) <- names(lista)