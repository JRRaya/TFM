# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
gc(verbose = FALSE, full = TRUE)
terra::tmpFiles(current = TRUE, orphan = TRUE, old = TRUE, remove = TRUE)
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(terra, sf, mapview, dplyr, here, performance, ggplot2, stats, DescTools, plotly, rayshader)

# 2. Carga de datos
variables_2008_2023 <- terra::rast("data/variables/variables_2008_2023.tif")

df <- terra::as.data.frame(
  variables_2008_2023[[c("ailanto_2008_2023_rast", "orto_2007_vi_VARI", "radiacion", "mde3_knnidw")]],
  cells = FALSE,
  na.rm = TRUE
)

df_clean <- df %>% 
  filter(
    is.finite(orto_2007_vi_VARI), 
    is.finite(radiacion),
    is.finite(mde3_knnidw)
  )

df_0 <- df_clean %>% filter(ailanto_2008_2023_rast == 0)
df_1 <- df_clean %>% filter(ailanto_2008_2023_rast == 1)

n_0 <- nrow(df_0)
n_1 <- nrow(df_1)

set.seed(123) # Para reproducibilidad científica

if (n_0 > n_1) {
  df_0_sub <- df_0[sample(nrow(df_0), n_1), ]
  df_balanceado <- bind_rows(df_0_sub, df_1)
} else if (n_1 > n_0) {
  df_1_sub <- df_1[sample(nrow(df_1), n_0), ]
  df_balanceado <- bind_rows(df_0, df_1_sub)
} else {
  df_balanceado <- df_clean
}

glm_multi <- glm(
  formula = ailanto_2008_2023_rast ~ orto_2007_vi_VARI + radiacion + mde3_knnidw, 
  data = df_balanceado, 
  family = stats::binomial(link = "logit")
)

# 1. Generamos las predicciones (clase 0 o 1) usando un umbral de 0.5
df_balanceado$prediccion <- ifelse(predict(glm_multi, type = "response") > 0.5, 1, 0)

# 2. Creamos la matriz de confusión
matriz <- table(Real = df_balanceado$ailanto_2008_2023_rast, 
                Predicho = df_balanceado$prediccion)

print(matriz)

# 3. Calculamos la precisión global
accuracy <- sum(diag(matriz)) / sum(matriz)
cat("\nPrecisión total del modelo:", round(accuracy * 100, 2), "%")


# 1. Estandarizamos las variables para que el cubo sea proporcional
df_3d_std <- df_balanceado %>%
  mutate(
    VARI_std = as.numeric(scale(orto_2007_vi_VARI)),
    RAD_std  = as.numeric(scale(radiacion)),
    MDE_std  = as.numeric(scale(mde3_knnidw)),
    Ailanto  = factor(ailanto_2008_2023_rast, labels = c("Ausencia", "Presencia"))
  )

# 2. Generamos el gráfico con las variables estandarizadas
plot_ly(df_3d, 
        x = ~VARI_std, y = ~RAD_std, z = ~MDE_std, 
        color = ~Ailanto, colors = c("steelblue", "red"),
        type = 'scatter3d', mode = 'markers',
        marker = list(size = 3, opacity = 0.6)) %>%
  layout(scene = list(
    xaxis = list(title = "VARI (Estandarizado)"),
    yaxis = list(title = "Radiación (Estandarizada)"),
    zaxis = list(title = "MDE (Estandarizado)"),
    aspectmode = "cube" # Fuerza a que sea un cubo perfecto
  ))

