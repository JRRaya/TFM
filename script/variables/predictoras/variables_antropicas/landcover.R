# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
gc(verbose = FALSE, full = TRUE)
# terra::tmpFiles(current = TRUE, orphan = TRUE, old = TRUE, remove = TRUE)
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(terra, dplyr, here)

# 2. Carga de datos
lista <- base::lapply(
  X = list.files(
    here::here("data/variables/variables_geograficas/landcover"),
    full.names = TRUE
  ),
  FUN = terra::rast
)

# 3. Reclasificación
lista_clas <- base::lapply(
  X = lista,
  FUN = function(x) {
    levels(x) <- data.frame(
      ID = 1:5,
      landcover = c("Artificial", "Agricola", "Forestal", "Humedal", "Agua") 
    )

    return(x)
  }
)

# 4. Guardado
# 4.1. Obtener los nombres de los archivos de entrada
nombres <- base::lapply(
  X = base::lapply(
    X = list.files(
      here::here("data/variables/variables_geograficas/landcover"),
      full.names = TRUE
    ),
    FUN = basename
  ),
  FUN = tools::file_path_sans_ext
)

# 4.2. Guardado final
invisible(
  mapply(
    function(x, n) {
      terra::writeRaster(
        x = x,
        filename = here::here("data/variables/variables_geograficas/landcover", paste0(n, "_clas.tif")),
        overwrite = TRUE,
        datatype = "INT1U"
      )
    },
    x = lista_clas,
    n = nombres
  )
)