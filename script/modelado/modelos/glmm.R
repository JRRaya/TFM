# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
gc(verbose = FALSE, full = TRUE)
# terra::tmpFiles(current = TRUE, orphan = TRUE, old = TRUE, remove = TRUE)
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(terra, sf, dplyr, here, performance, ggplot2, stats, gtsummary, caret, MLeval, doParallel, purrr, lmerTest, lme4)

# 1.3. Configuración de reproducibilidad paralela
RNGkind("L'Ecuyer-CMRG")
set.seed(123)

# 1.4. Configurar paralelismo
cl <- parallel::makeCluster(3)
doParallel::registerDoParallel(cl)

# 1.5. Lista de semillas para el procesamiento paralelo
semillas <- lapply(1:201, function(x) sample.int(1000, 110))
semillas[[201]] <- sample.int(1000, 1)

# 2. Carga de datos
lista <- lapply(
  X = list(
    diferencia_2008_2012 = here::here("data/modelos/balanceo/df_balanceo_2008_2012.RDS"),
    diferencia_2008_2023 = here::here("data/modelos/balanceo/df_balanceo_2008_2023.RDS")
  ),
  FUN = base::readRDS
)

lista[["diferencia_2008_2012"]]$landcover_2006 <- as.factor(lista[["diferencia_2008_2012"]]$landcover_2006)
lista[["diferencia_2008_2023"]]$landcover_2006 <- as.factor(lista[["diferencia_2008_2023"]]$landcover_2006)

# 3. Comprobación del intercepto y la pendiente
# 3.1. Extraer los coeficientes en función de la variable categórica ('random effect')
m <- glmer(diferencia_2008_2023_rast ~ orto_2007_vi_VARI + (1|landcover_2006) + (0 + orto_2007_vi_VARI|landcover_2006), data = lista[["diferencia_2008_2023"]], family = binomial)
summary(m)
coefs_por_grupo <- coef(m)$landcover_2006
coefs_por_grupo[order(coefs_por_grupo$orto_2007_vi_VARI, decreasing = TRUE), ]