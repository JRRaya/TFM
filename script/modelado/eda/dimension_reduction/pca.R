# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)
# terra::tmpFiles(remove = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(terra, sf, dplyr, tidyr, purrr, here, stats, corrplot, usdm, ggfortify, factoextra)

# 2. Carga de datos
# 2.1. Dataframes de variables
lista <- lapply(
  X = list(
    diferencia_2008_2012 = here::here("data/modelos/balanceo/df_balanceo_2008_2012.RDS"),
    diferencia_2008_2023 = here::here("data/modelos/balanceo/df_balanceo_2008_2023.RDS")
  ),
  FUN = base::readRDS
)

# 2.2. Dataframes escalado de variables
lista_escalado <- lapply(
  X = list(
    diferencia_2008_2012 = here::here("data/modelos/escalado/df_escalado_2008_2012.RDS"),
    diferencia_2008_2023 = here::here("data/modelos/escalado/df_escalado_2008_2023.RDS")
  ),
  FUN = base::readRDS
)

# 3. Análisis de Componentes Principales (PCA)
# 3.1. Aplicar PCA
print(
  pca_2008_2013 <- lista_escalado[["diferencia_2008_2012"]] %>%
    stats::prcomp(center = TRUE, scale. = TRUE)
)

print(
  pca_2008_2023 <- lista_escalado[["diferencia_2008_2023"]] %>%
    stats::prcomp(center = TRUE, scale. = TRUE)
)

# 3.2. Resumen del PCA
summary(pca_2008_2013)

summary(pca_2008_2023)

# 3.3. Análisis de la varianza explicada
grDevices::pdf(
  here::here("data/modelos/eda/pca/screeplot_2008_2013.pdf"), 
  width = 8, 
  height = 8
)

stats::screeplot(
  pca_2008_2013,
  type = "lines"
)

grDevices::dev.off()

grDevices::pdf(
  here::here("data/modelos/eda/pca/screeplot_2008_2023.pdf"), 
  width = 8, 
  height = 8
)

stats::screeplot(
  pca_2008_2023,
  type = "lines"
)

grDevices::dev.off()

# 3.4. Gráfico de las 2 componentes principales
grDevices::pdf(
  here::here("data/modelos/eda/pca/pca_2008_2013.pdf"), 
  width = 8, 
  height = 8
)

ggplot2::autoplot(
  pca_2008_2013,
  data = lista[["diferencia_2008_2012"]],
  loadings = TRUE,
  loadings.label = TRUE,
  colour = "diferencia_2008_2012_rast",
  alpha = 0.7
)

grDevices::dev.off()

grDevices::pdf(
  here::here("data/modelos/eda/pca/pca_2008_2023.pdf"), 
  width = 8, 
  height = 8
)

ggplot2::autoplot(
  pca_2008_2023,
  data = lista[["diferencia_2008_2023"]],
  loadings = TRUE,
  loadings.label = TRUE,
  colour = "diferencia_2008_2023_rast",
  alpha = 0.7
)

grDevices::dev.off()

# 4. Guardado
saveRDS(
  pca_2008_2013,
  here::here("data/modelos/eda/pca/pca_2008_2012.RDS")
)

saveRDS(
  pca_2008_2023,
  here::here("data/modelos/eda/pca/pca_2008_2023.RDS")
)