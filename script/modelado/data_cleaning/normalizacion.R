# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
future::plan(future::sequential)
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
base::gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(terra, sf, dplyr, tidyr, here, future, furrr)

# 1.3. Configuración de directorios temporales de 'terra'
terra::terraOptions(
  tempdir = here::here("E:/jose/tmp"),
  todisk  = TRUE
)

# 1.4. Configurar paralelismo
future::plan(multisession, workers = 5L)

# 2. Crear datasets de variables a normalizar
# 2.1. Presencias 
# 2.1.1. 'Bounding box' del área de estudio
roi <- sf::st_read(
  here::here("data/variables/variables_antropicas/carreteras/carreteras_buffer_25.gpkg"),
  quiet = TRUE
) %>% 
  sf::st_transform("EPSG:25830")

# 2.1.2. Rásteres binarios (0, 1) de píxeles invadidos (o no) en un periodo dado
presencias_2008 <- here::here("data/presencias/presencias_2008_rast.tif")

presencias_2015 <- here::here("data/presencias/presencias_2015_rast.tif")

diferencia_2008_2012 <- here::here("data/presencias/diferencia_2008_2012_rast.tif")

diferencia_2008_2023 <- here::here("data/presencias/diferencia_2008_2023_rast.tif")

diferencia_2015_2023 <- here::here("data/presencias/diferencia_2015_2023_rast.tif")

diferencia_2018_2023 <- here::here("data/presencias/diferencia_2018_2023_rast.tif")

diferencia_2020_2023 <- here::here("data/presencias/diferencia_2020_2023_rast.tif")

# 2.2. Variables predictoras
# 2.2.1. Variables bióticas
# 2.2.1.1. Variables funcionales (índices de vegetación)
variables_funcionales_2008_2012 <- list(
  here::here("data/variables/variables_bioticas/funcional/rgb/orto_2008/orto_2008_vi.tif")
)

variables_funcionales_2008_2023 <- list(
  here::here("data/variables/variables_bioticas/funcional/rgb/orto_2008/orto_2008_vi.tif")
)

variables_funcionales_2015_2023 <- list(
  here::here("data/variables/variables_bioticas/funcional/rgb/orto_2013/orto_2013_vi.tif")
)

variables_funcionales_2018_2023 <- list(
  here::here("data/variables/variables_bioticas/funcional/rgb/orto_2016/orto_2016_vi.tif")
)

variables_funcionales_2020_2023 <- list(
  here::here("data/variables/variables_bioticas/funcional/rgb/orto_2020/orto_2020_vi.tif")
)

# 2.2.1.2. Variables estructurales (métricas derivadas de datos LiDAR)
variables_estructurales_2015_2023 <- list.files(
  path = here::here("data/variables/variables_bioticas/estructural"),
  pattern = "_lidar1\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

variables_estructurales_2020_2023 <- list.files(
  path = here::here("data/variables/variables_bioticas/estructural"),
  pattern = "_lidar2\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

# 2.2.1.3. Distancia a focos
variables_distancia_focos_2008_2023 <- list(
  here::here("data/variables/variables_bioticas/distancia_focos/distancia_presencias_2008.tif")
)

variables_distancia_focos_2008_2012 <- list(
  here::here("data/variables/variables_bioticas/distancia_focos/distancia_presencias_2008.tif")
)

variables_distancia_focos_2015_2023 <- list(
  here::here("data/variables/variables_bioticas/distancia_focos/distancia_presencias_2008_2015.tif")
)

variables_distancia_focos_2018_2023 <- list(
  here::here("data/variables/variables_bioticas/distancia_focos/distancia_presencias_2008_2018.tif")
)

variables_distancia_focos_2020_2023 <- list(
  here::here("data/variables/variables_bioticas/distancia_focos/distancia_presencias_2008_2020.tif")
)

# 2.2.2. Variables antrópicas
variables_antropicas_2008_2023 <- list(
  list.files(
    path = here::here("data/variables/variables_antropicas"),
    pattern = "(cauces|nucleos)_distancia_5\\.tif$", 
    full.names = TRUE,
    recursive = TRUE
  ),
  here::here("data/variables/variables_antropicas/mucva/mucva_2007.tif")
)

variables_antropicas_2008_2012 <- list(
  list.files(
    path = here::here("data/variables/variables_antropicas"),
    pattern = "(cauces|nucleos)_distancia_5\\.tif$", 
    full.names = TRUE,
    recursive = TRUE
  ),
  here::here("data/variables/variables_antropicas/mucva/mucva_2007.tif")
)

variables_antropicas_2015_2023 <- list(
  list.files(
    path = here::here("data/variables/variables_antropicas"),
    pattern = "(cauces|nucleos)_distancia_5\\.tif$", 
    full.names = TRUE,
    recursive = TRUE
  ),
  here::here("data/variables/variables_antropicas/hfp/hfp_2015.tif"),
  here::here("data/variables/variables_antropicas/landcover/landcover_2012.tif")
)

variables_antropicas_2018_2023 <- list(
  list.files(
    path = here::here("data/variables/variables_antropicas"),
    pattern = "(cauces|nucleos)_distancia_5\\.tif$", 
    full.names = TRUE,
    recursive = TRUE
  ),
  here::here("data/variables/variables_antropicas/hfp/hfp_2018.tif"),
  here::here("data/variables/variables_antropicas/landcover/landcover_2018.tif")
)

variables_antropicas_2020_2023 <- list(
  list.files(
    path = here::here("data/variables/variables_antropicas"),
    pattern = "(cauces|nucleos)_distancia_5\\.tif$", 
    full.names = TRUE,
    recursive = TRUE
  ),
  here::here("data/variables/variables_antropicas/hfp/hfp_2020.tif"),
  here::here("data/variables/variables_antropicas/landcover/landcover_2018.tif")
)

# 2.2.3. Variables del relieve
variables_relieve <- list(
  c(
    here::here("data/variables/variables_relieve/radiacion/radiacion_directa.tif"),
    here::here("data/variables/variables_relieve/mde/mde.tif"),
    here::here("data/variables/variables_relieve/slope/slope_grados.tif"),
    here::here("data/variables/variables_relieve/slope/downslope_index.tif"),
    here::here("data/variables/variables_relieve/twi/sca.tif"),
    here::here("data/variables/variables_relieve/twi/twid.tif"),
    here::here("data/variables/variables_relieve/twi/twi.tif"),
    here::here("data/variables/variables_relieve/tpi/tpi.tif"),
    here::here("data/variables/variables_relieve/tri/tri.tif"),
    here::here("data/variables/variables_relieve/aspect/aspect.tif")
  )
)

# 2.2.4. Variables bioclimáticas
variables_bioclimaticas_2008_2023 <- list(
  here::here("data/variables/variables_bioclimaticas/bioclimaticas/bioclimaticas_1979_2008.tif")
)

variables_bioclimaticas_2008_2012 <- list(
  here::here("data/variables/variables_bioclimaticas/bioclimaticas/bioclimaticas_1979_2008.tif")
)

variables_bioclimaticas_2015_2023 <- list(
  here::here("data/variables/variables_bioclimaticas/bioclimaticas/bioclimaticas_1986_2015.tif")
)

variables_bioclimaticas_2020_2023 <- list(
  here::here("data/variables/variables_bioclimaticas/bioclimaticas/bioclimaticas_1991_2020.tif")
)

# 2.5. Datasets de variables a normalizar
lista_2008 <- unlist( 
  list(
    presencias_2008, 
    variables_funcionales_2008_2023, 
    variables_antropicas_2008_2023, 
    variables_relieve,
    variables_bioclimaticas_2008_2023
  )
)

lista_2015 <- unlist( 
  list(
    presencias_2015, 
    variables_funcionales_2015_2023, 
    variables_estructurales_2015_2023,
    variables_antropicas_2015_2023, 
    variables_relieve,
    variables_bioclimaticas_2015_2023
  )
)

lista_2008_2012 <- unlist( # Aplanar lista
  list(
    diferencia_2008_2012, 
    variables_funcionales_2008_2012, 
    variables_distancia_focos_2008_2012,
    variables_antropicas_2008_2012, 
    variables_relieve,
    variables_bioclimaticas_2008_2012
  )
)

lista_2008_2023 <- unlist( 
  list(
    diferencia_2008_2023, 
    variables_funcionales_2008_2023, 
    variables_distancia_focos_2008_2023,
    variables_antropicas_2008_2023, 
    variables_relieve,
    variables_bioclimaticas_2008_2023
  )
)

lista_2015_2023 <- unlist( 
  list(
    diferencia_2015_2023, 
    variables_funcionales_2015_2023, 
    variables_estructurales_2015_2023,
    variables_distancia_focos_2015_2023,
    variables_antropicas_2015_2023, 
    variables_relieve,
    variables_bioclimaticas_2015_2023
  )
)

lista_2020_2023 <- unlist( 
  list(
    diferencia_2020_2023, 
    variables_funcionales_2020_2023, 
    variables_estructurales_2020_2023,
    variables_distancia_focos_2020_2023,
    variables_antropicas_2020_2023, 
    variables_relieve,
    variables_bioclimaticas_2020_2023
  )
)

# 3. Funciones auxiliares
# 3.1. Función de detección de variables categóricas
categoricas <- function(ruta) {
  any(
    stringr::str_detect(
      basename(ruta), 
      c("mucva", "landcover", "diferencia", "presencias")
    )
  )
}

# 3.2. Función de eliminación de variables sin cobertura en presencias
depurar_lidar <- function(multibanda) {
  df_completo <- terra::as.data.frame(multibanda, na.rm = FALSE)

  # Identificar columna predicha
  predicha <- names(df_completo)[grep("^(diferencia|presencias)_", names(df_completo))][1]

  # Filas de presencia
  filas_presencia <- which(df_completo[[predicha]] == 1)

  # Si no hay presencias, devolver multibanda intacto
  if (length(filas_presencia) == 0) {
    warning("No se encontraron píxeles de presencia en el multibanda.")
    return(multibanda)
  }


  # Variables con cualquier NA en píxeles de presencia
  vars_con_na <- df_completo[filas_presencia, ] %>%
    dplyr::summarise(dplyr::across(everything(), ~ sum(is.na(.x)))) %>%
    tidyr::pivot_longer(everything()) %>%
    dplyr::filter(value > 0) %>%
    dplyr::pull(name)

  # Informar
  if (length(vars_con_na) > 0) {
    message(
      "Variables eliminadas por NAs en píxeles de presencia (n = ",
      length(vars_con_na), "): ",
      paste(vars_con_na, collapse = ", ")
    )
    multibanda <- terra::subset(
      multibanda,
      which(!names(multibanda) %in% vars_con_na)
    )
  } else {
    message("Sin variables con NAs en píxeles de presencia.")
  }

  return(multibanda)
}

# 3.3. Función de normalización
normalizacion <- function(lista, ruta_raster, ruta_df, temp, aoi) {
  # 1. Cargar plantilla raster
  plantilla <- lista[[1]] %>% 
    terra::rast() %>%
    terra::project(
      "EPSG:25830", 
      method = "near"
    )

  # 2. Crear vector de rutas finales
  rutas_tmp <- character(length(lista))

  # 3. Bucle de procesado de las variables
  for (i in seq_along(lista)) {
    cat("Procesando", i, "de", length(lista), ":", basename(lista[[i]]), "\n")

    # 3.1. Cargar raster
    r <- terra::rast(lista[[i]])

    # 3.2. Reproyectar/asignar (manualemnte) CRS
    if (grepl("LOCAL|unsupported|Engineering", terra::crs(r))) {
      terra::crs(r) <- "EPSG:25830"
    } else {
      r <- terra::project(
        x = r,
        y = "EPSG:25830", 
        method = "near",
        threads = TRUE
      )
    }

    # 3.3. Conversión a categórico si procede
    if (categoricas(lista[[i]])) {
      r <- terra::as.factor(r)
    }

    # 3.4. Normalización
    # 3.4.1. Remuestreo
    r <- terra::resample(
      x = r,
      y = plantilla,
      method = if (any(terra::is.factor(r))) "near" else "bilinear",
      threads = TRUE
    )

    # 3.4.2. Recorte y enmascarado
    r <-  terra::crop(
      x = r,
      y = aoi,
      mask = TRUE
    )

    # 3.5. Asignación de nombres
    # 3.5.1. Generar vector con el (/los) nombre(s)
    nombres <- if (terra::nlyr(r) == 1) {
      tools::file_path_sans_ext(basename(lista[[i]]))
    } else {
      terra::names(r)
    }

    # 3.5.2. Sanear nombres
    nombres <- nombres %>%
      gsub("[^A-Za-z0-9_]", "_", x = .) %>%
      gsub("^([0-9])", "X\\1", x = .) %>%
      make.unique(sep = "_")

    # 3.5.3. Asignar el (/los) nombre(s)
    terra::set.names(
      r, 
      nombres
    )

    # 3.6. Guardado del temporal
    terra::writeRaster(
      r, 
      file.path(temp, paste0("tmp_", i, ".tif")), 
      overwrite = TRUE, 
      gdal = c("BIGTIFF=YES", "COMPRESS=LZW", "TILED=YES")
    )
    
    # 3.7. Agregar a la lista de variables
    rutas_tmp[i] <- file.path(temp, paste0("tmp_", i, ".tif"))

    # 3.8. Limpieza
    rm(nombres, r)
    gc(full = TRUE)
  }

  # 4. Raster multibanda
  # 4.1. Generar raster multibanda
  multibanda <- rutas_tmp %>% 
    terra::rast()

  # 4.2. Guardar raster
  terra::writeRaster(
    multibanda,
    ruta_raster, 
    overwrite = TRUE, 
    gdal = c("BIGTIFF=YES", "COMPRESS=LZW", "TILED=YES")
  )
  
  # 5. Dataframe
  # 5.1. Depurar variables sin cobertura en píxeles de presencia
  multibanda <- depurar_lidar(multibanda)

  # 5.2. Guardado
  terra::as.data.frame(multibanda, na.rm = TRUE) %>%
    tidyr::drop_na() %>%
    dplyr::filter(
      dplyr::if_all(
        .cols = where(~ !is.factor(.x) & !is.character(.x)),
        .fns  = is.finite
      )
    ) %>%
    base::saveRDS(ruta_df) 

  # 6. Limpiar temporales
  file.remove(rutas_tmp)
  rm(multibanda)
  gc(full = TRUE)
}

# 4. Crear datasets de variables normalizadas
# 4.1. Lista de datasets
datasets <- list(
  list(
    lista = lista_2008,
    ruta_raster = here::here("data/modelos/normalizacion/variables_2008.tif"),
    ruta_df = here::here("data/modelos/normalizacion/df_2008.rds"),
    temp = here::here("E:/jose/tmp/w1")  
  ),
  list(
    lista       = lista_2015,
    ruta_raster = here::here("data/modelos/normalizacion/variables_2015.tif"),
    ruta_df     = here::here("data/modelos/normalizacion/df_2015.rds"),
    temp        = here::here("E:/jose/tmp/w2")  
  ),
  list(
    lista = lista_2008_2012,
    ruta_raster = here::here("data/modelos/normalizacion/variables_2008_2012.tif"),
    ruta_df = here::here("data/modelos/normalizacion/df_2008_2012.rds"),
    temp = here::here("E:/jose/tmp/w3")  
  ),
  list(
    lista = lista_2008_2023,
    ruta_raster = here::here("data/modelos/normalizacion/variables_2008_2023.tif"),
    ruta_df = here::here("data/modelos/normalizacion/df_2008_2023.rds"),
    temp = here::here("E:/jose/tmp/w4")
  ),
  list(
    lista = lista_2015_2023,
    ruta_raster = here::here("data/modelos/normalizacion/variables_2015_2023.tif"),
    ruta_df = here::here("data/modelos/normalizacion/df_2015_2023.rds"),
    temp = here::here("E:/jose/tmp/w5")
  ),
  list(
    lista = lista_2020_2023,
    ruta_raster = here::here("data/modelos/normalizacion/variables_2020_2023.tif"),
    ruta_df = here::here("data/modelos/normalizacion/df_2020_2023.rds"),
    temp = here::here("E:/jose/tmp/w6")
  )
)

# 4.2. Crear directorios temporales
purrr::walk(datasets, ~dir.create(.x$temp, recursive = TRUE, showWarnings = FALSE))

# 4.3. Ejecutar en paralelo
furrr::future_walk(
  datasets,
  ~normalizacion(
    lista = .x$lista,
    ruta_raster = .x$ruta_raster,
    ruta_df = .x$ruta_df,
    temp = .x$temp,
    aoi = roi
  ),
  .options = furrr::furrr_options(seed = TRUE)
)

# 4.4. Detener paralelización
future::plan(sequential)