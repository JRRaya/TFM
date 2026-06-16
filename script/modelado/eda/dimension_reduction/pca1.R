# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
future::plan(future::sequential)
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(FactoMineR, factoextra, dplyr, tidyr, purrr, furrr, future, here)

# 1.3. Configurar paralelismo
future::plan(multisession, workers = 2L)

# 2. Función de balanceo
balanceo <- function(ruta_input, ruta_output) {
  # 1. Lectura del dataframe
  df <- base::readRDS(ruta_input)

  # 2. PCA
  pca <- FactoMineR::PCA(
    df, 
    scale.unit = FALSE,
    graph = FALSE
  )

  # 3. Gráfico de varianza explicada por cada componente
  var <- factoextra::fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 50))

  # 4. Gráfico de contribución de las variables
  con1 <- factoextra::fviz_contrib(pca, choice = "var", axes = 1, top = 10)

  con2 <- factoextra::fviz_contrib(pca, choice = "var", axes = 2, top = 10)

  # 5. Gráfico de las 2 primeras componentes
  comp <- factoextra::fviz_pca_var(
    pca, 
    col.var ="contrib",
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE
  )

  # 6. Guardado
  base::saveRDS(pca, file.path(ruta_output, paste0("pca_", etiqueta, "rds")))

  ggplot2::ggsave(
    filename = file.path(ruta_output, paste0("con1_", etiqueta, ".png")),
    plot     = con1,
    width    = 8,
    height   = 5,
    dpi      = 600
  )

  ggplot2::ggsave(
    filename = file.path(ruta_output, paste0("con2_", etiqueta, ".png")),
    plot     = con2,
    width    = 8,
    height   = 5,
    dpi      = 600
  )

  ggplot2::ggsave(
    filename = file.path(ruta_output, paste0("comp_", etiqueta, ".png")),
    plot     = comp,
    width    = 8,
    height   = 5,
    dpi      = 600
  )
  
  # 7. Limpieza
  rm(df, pca, con1, con2, comp)
  gc(full = TRUE)
}

# 3. Definir datasets
datasets <- list(
  list(
    ruta_input  = here::here("data/modelos/escalado/df_be_2008_2023.rds"),
    ruta_output = "data/modelos/eda/pca",
    etiqueta = "2008_2023"
  ),
  list(
    ruta_input  = here::here("data/modelos/escalado/df_be_2015_2023.rds"),
    ruta_output = "data/modelos/eda/pca",
    etiqueta = "2015_2023"
  )
)

# 4. Ejecutar en paralelo
furrr::future_walk(
  datasets,
  ~balanceo(
    ruta_input  = .x$ruta_input,
    ruta_output = .x$ruta_output
  ),
  .options = furrr::furrr_options(seed = TRUE)
)

# 5. Detener paralelización
future::plan(sequential)