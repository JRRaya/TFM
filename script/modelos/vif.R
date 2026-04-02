# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)
terra::tmpFiles(remove = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(terra, sf, dplyr, tidyr, purrr, here, stats, corrplot, usdm, GGally)

# 2. Carga de datos
# 2.1. Ráster multibanda de variables
variables <- terra::rast(
  here::here("data/variables/variables_2008_2023.tif")
)

# 2.2. Dataframe de variables
df <- terra::as.data.frame(
  variables,
  na.rm = TRUE
) %>% 
  dplyr::filter(if_all(everything(), is.finite)) %>%
  dplyr::group_by(ailanto_2008_2023_rast) %>%
  tidyr::nest() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(n = purrr::map_int(data, nrow)) %>%
  dplyr::mutate(data = purrr::map(data, ~ dplyr::slice_sample(.x, n = min(n)))) %>%
  tidyr::unnest(data) %>%
  dplyr::select(-n)

# 2.3. Comprobación del balanceo
print(
  resumen <- df %>% 
    as.data.frame() %>% 
    dplyr::group_by(ailanto_2008_2023_rast) %>% 
    dplyr::summarise(
      n_filas = n()
    ) %>% 
    dplyr::ungroup()
)

# 2.4. Conversión a dataframe estandar
df <- df %>% 
  dplyr::ungroup() %>% 
  base::as.data.frame() %>% 
  dplyr::select(-ailanto_2008_2023_rast)

# 2.5. Correlación con diagrama de dispersión
GGally::ggpairs(df)

# 3. Test de correlación inicial
# 3.1. Obtener la colinealidad de las variables
df_cor <- stats::cor(df, method = "spearman")

# 3.2. Representación gráfica
df_cor_plot <- corrplot::corrplot(df_cor, type = "lower", method = "color")

# 4. Análisis VIF y de correlación
# 4.1. Obtener los valores de correlación (de Spearman) y VIF
# 4.1.1. Obtener el valor VIF de cada una de las variables
print(
  vif <- usdm::vif(df)
)
 
# 4.1.2. Identificar variables con un VIF > 5 (análisis VIF)
print(
  vifstep <- df %>% 
    usdm::vifstep(th = 5, size = 10000, method = 'spearman')
)

# 4.1.3. Identificar variables con una correlación de Spearman > 0.7 (análisis de correlación)
print(
  vifcor <- df %>% 
    usdm::vifcor(th = 0.6, size = 10000, method = 'spearman')
)

# 4.2. Selección de variables
filtradas <- usdm::exclude(df, vifcor)














# 1. Carga de datos
# 1.1. Cargar stack de variables
variables <- terra::rast(here::here("data/variables/variables.tif"))

# 1.2. Transformar una muestra de los datos a dataframe
set.seed(123)

var_df <- variables %>% 
  terra::spatSample(
    size = 10000, 
    method = "random", 
    na.rm = TRUE, 
    values = TRUE, 
    as.df = TRUE
  ) 

# 2. Test de correlación inicial
# 2.1. Obtener la colinealidad de las variables
var_cor <- stats::cor(var_df, method = "spearman")

# 2.2. Representación gráfica
var_cor_plot <- corrplot::corrplot(var_cor, type = "lower", method = "color")

# 3. Análisis VIF y de correlación
# 3.1. Obtener los valores de correlación (de Spearman) y VIF
# 3.1.1. Obtener el valor VIF de cada una de las variables
print(
  var_vif <- usdm::vif(var_df)
)
 
# 3.1.2. Identificar variables con un VIF > 5 (análisis VIF)
print(
  var_vifstep <- usdm::vifstep(var_df, th = 5, size = 10000, method = 'spearman')
)

# 3.1.3. Identificar variables con una correlación de Spearman > 0.7 (análisis de correlación)
print(
  var_vifcor <- usdm::vifcor(var_df, th = 0.6, size = 10000, method = 'spearman')
)

# 3.2. Selección de variables
variables_filtradas <- usdm::exclude(variables, var_vifcor)

# 4. Análisis de colinealidad tras cribado
# 4.1. Obtener dataframe de las variables
variables_df <- variables_filtradas %>% 
  terra::spatSample(
    size = 10000, 
    method = "random", 
    na.rm = TRUE, 
    values = TRUE, 
    as.df = TRUE
  )

# 4.2. Obtener la colinealidad de las variables
variables_cor <- cor(variables_df, method = "spearman")

# 4.3. Representación gráfica
variables_cor_plot <- corrplot(variables_cor, type = "lower", method = "color")

# 5. Guardado del stack definitivo de variables predictoras
terra::writeRaster(variables_filtradas, here::here("data/variables/variables_filtradas.tif"), overwrite = TRUE)