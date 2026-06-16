# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)
terra::tmpFiles(remove = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(terra, sf, dplyr, tidyr, purrr, here, stats, corrplot, usdm, ggfortify, cluster, factoextra, mclust, kernlab)

# 2. Carga de datos
# 2.1. Dataframes de variables
lista <- lapply(
  X = list(
    ailanto_2008_2013 = here::here("data/modelos/balanceo/df_balanceo_2008_2013.RDS"),
    ailanto_2008_2023 = here::here("data/modelos/balanceo/df_balanceo_2008_2023.RDS")
  ),
  FUN = base::readRDS
)

# 2.2. Dataframes escalado de variables
lista_escalado <- lapply(
  X = list(
    ailanto_2008_2013 = here::here("data/modelos/escalado/df_escalado_2008_2013.RDS"),
    ailanto_2008_2023 = here::here("data/modelos/escalado/df_escalado_2008_2023.RDS")
  ),
  FUN = base::readRDS
)

# 2.3. PCA
lista_pca <- lapply(
  X = list(
    pca_2008_2013 = here::here("data/modelos/eda/pca/pca_2008_2013.RDS"),
    pca_2008_2023 = here::here("data/modelos/eda/pca/pca_2008_2023.RDS")
  ),
  FUN = base::readRDS
)

# 3. Análisis de clusters
# 3.1. Centroid-based clustering
# 3.1.1. K-means
# 3.1.1.1. Análisis del número óptimo de clusteres ('k')
grDevices::pdf(
  here::here("data/modelos/eda/cluster/km_k1.pdf"), 
  width = 8, 
  height = 8
)

lista_escalado[["ailanto_2008_2013"]] %>% 
  factoextra::fviz_nbclust(
    FUNcluster = stats::kmeans,
    method = "silhouette",
    verbose = FALSE
  )

grDevices::dev.off()

##########################

grDevices::pdf(
  here::here("data/modelos/eda/cluster/km_k2.pdf"), 
  width = 8, 
  height = 8
)

lista_escalado[["ailanto_2008_2013"]] %>% 
  factoextra::fviz_nbclust(
    FUNcluster = stats::kmeans,
    method = "wss",
    verbose = FALSE
  )

grDevices::dev.off()

# 3.1.1.2. Cálculo de clusteres
print(
  km <- lista_escalado[["ailanto_2008_2013"]] %>% 
    stats::kmeans(centers = 2, trace = FALSE, iter.max = 100)
)

# 3.1.1.3. Graficar clusteres
grDevices::pdf(
  here::here("data/modelos/eda/cluster/km1.pdf"), 
  width = 8, 
  height = 8
)

ggplot2::autoplot(
  lista_pca[["pca_2008_2013"]],
  data = lista[["ailanto_2008_2013"]],
  colour = km$cluster,
  shape = "ailanto_2008_2013_rast",
  alpha = 0.7,
  size = 2
)

grDevices::dev.off()

######################

grDevices::pdf(
  here::here("data/modelos/eda/cluster/km2.pdf"), 
  width = 8, 
  height = 8
)

factoextra::fviz_cluster(
  km, 
  data = lista_escalado[["ailanto_2008_2013"]], 
  geom = "point", 
  ellipse.type = "norm",
  pointsize = 2,
  ggtheme = theme_classic()
)

grDevices::dev.off()

# 3.1.2. Partitioning Around Medoids (PAM)
# 3.1.2.1. Análisis del número óptimo de clusteres ('k')
grDevices::pdf(
  here::here("data/modelos/eda/cluster/pam_k1.pdf"), 
  width = 8, 
  height = 8
)

lista_escalado[["ailanto_2008_2013"]] %>% 
  factoextra::fviz_nbclust(
    FUNcluster = cluster::pam,
    method = "silhouette",
    verbose = FALSE
  )

grDevices::dev.off()

#######################

grDevices::pdf(
  here::here("data/modelos/eda/cluster/pam_k2.pdf"), 
  width = 8, 
  height = 8
)

lista_escalado[["ailanto_2008_2013"]] %>% 
  factoextra::fviz_nbclust(
    FUNcluster = cluster::pam,
    method = "wss",
    verbose = FALSE
  )

grDevices::dev.off()

# 3.1.2.2. Cálculo de clusteres
print(
  pam <- lista_escalado[["ailanto_2008_2013"]] %>% 
    cluster::pam(k = 2, metric = "manhattan", trace.lev = 0)
)

# 3.1.2.3. Graficar clusteres
grDevices::pdf(
  here::here("data/modelos/eda/cluster/pam1.pdf"), 
  width = 8, 
  height = 8
)

ggplot2::autoplot(
  lista_pca[["pca_2008_2013"]],
  data = lista[["ailanto_2008_2013"]],
  colour = pam$clustering,
  shape = "ailanto_2008_2013_rast",
  alpha = 0.7,
  size = 2
)

grDevices::dev.off()

############################

grDevices::pdf(
  here::here("data/modelos/eda/cluster/pam2.pdf"), 
  width = 8, 
  height = 8
)

factoextra::fviz_cluster(
  pam, 
  data = lista_escalado[["ailanto_2008_2013"]], 
  geom = "point", 
  ellipse.type = "norm",
  pointsize = 2,
  ggtheme = theme_classic()
)

grDevices::dev.off()

# 3.2. Fuzzy clustering
# 3.2.1. Análisis del número óptimo de clusteres ('k')
grDevices::pdf(
  here::here("data/modelos/eda/cluster/fanny_k1.pdf"), 
  width = 8, 
  height = 8
)

lista_escalado[["ailanto_2008_2013"]] %>% 
  factoextra::fviz_nbclust(
    FUNcluster = cluster::fanny,
    method = "silhouette",
    verbose = FALSE
  )

grDevices::dev.off()

###############

grDevices::pdf(
  here::here("data/modelos/eda/cluster/fanny_k2.pdf"), 
  width = 8, 
  height = 8
)

lista_escalado[["ailanto_2008_2013"]] %>% 
  factoextra::fviz_nbclust(
    FUNcluster = cluster::fanny,
    method = "wss",
    verbose = FALSE
  )

grDevices::dev.off()

# 3.2.2. Cálculo de clusteres
print(
  fanny <- lista_escalado[["ailanto_2008_2013"]] %>% 
    cluster::fanny(k = 6, memb.exp = 1.2, maxit = 5000, metric = "manhattan", trace.lev = 0)
)

# 3.2.3. Graficar clusteres
grDevices::pdf(
  here::here("data/modelos/eda/cluster/fanny1.pdf"), 
  width = 8, 
  height = 8
)

ggplot2::autoplot(
  lista_pca[["pca_2008_2013"]],
  data = lista[["ailanto_2008_2013"]],
  colour = fanny$clustering,
  shape = "ailanto_2008_2013_rast",
  alpha = 0.7,
  size = 2
)

grDevices::dev.off()

#######################

grDevices::pdf(
  here::here("data/modelos/eda/cluster/fanny.pdf"), 
  width = 8, 
  height = 8
)

factoextra::fviz_cluster(
  fanny, 
  data = lista_escalado[["ailanto_2008_2013"]], 
  geom = "point", 
  ellipse.type = "norm",
  pointsize = 2,
  ggtheme = theme_classic()
)

grDevices::dev.off()

# 3.3. Model-based clustering
# 3.3.1. Cálculo de los clusteres
print(
  mcb <- lista_escalado[["ailanto_2008_2013"]] %>% 
    mclust::Mclust()
)

# 3.3.2. Graficar 
grDevices::pdf(
  here::here("data/modelos/eda/cluster/mbc1.pdf"), 
  width = 8, 
  height = 8
)

ggplot2::autoplot(
  lista_pca[["pca_2008_2013"]],
  data = lista[["ailanto_2008_2013"]],
  colour = mcb$clustering,
  shape = "ailanto_2008_2013_rast",
  alpha = 0.7,
  size = 2
)

grDevices::dev.off()

###################

grDevices::pdf(
  here::here("data/modelos/eda/cluster/mbc2.pdf"), 
  width = 8, 
  height = 8
)

factoextra::fviz_cluster(
  mcb, 
  data = lista_escalado[["ailanto_2008_2013"]], 
  geom = "point", 
  ellipse.type = "norm",
  pointsize = 2,
  ggtheme = theme_classic()
)

grDevices::dev.off()