# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
gc(verbose = FALSE, full = TRUE)
terra::tmpFiles(current = TRUE, orphan = TRUE, old = TRUE, remove = TRUE)
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(terra, sf, mapview, dplyr, here, performance, ggplot2, stats, DescTools)

# 2. Carga de datos
variables_2008_2013 <- terra::rast("data/variables/variables_2008_2013.tif")

variables_2014_2023 <- terra::rast("data/variables/variables_2014_2023.tif")

variables_2008_2023 <- terra::rast("data/variables/variables_2008_2023.tif")

# 3. Scatterplots
# 3.1. Función de generación de scatterplots
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

ggplot(df_balanceado, aes(x = as.factor(ailanto_2008_2023_rast), y = orto_2007_vi_VARI, fill = as.factor(ailanto_2008_2023_rast))) +
  geom_boxplot() +
  labs(title = "Diferencias de VARI según presencia de Ailanto",
       x = "Presencia (0 = No, 1 = Sí)",
       y = "Índice VARI") +
  scale_fill_manual(values = c("steelblue", "red"), guide = "none") +
  theme_minimal()










ggplot(df_clean, aes(x = orto_2007_vi_VARI, y = orto_2007_vi_TGI, color = as.factor(ailanto_2008_2023_rast))) +
  geom_point(alpha = 0.5) +
  # 1. Expandimos con valores aditivos si los multiplicadores no bastan
  scale_x_continuous(expand = expansion(mult = 0.08)) + 
  scale_y_continuous(expand = expansion(mult = 0.08)) +
  scale_color_manual(values = c("0" = "steelblue", "1" = "red"), 
                     name = "Presencia Ailanto",
                     labels = c("Ausencia (0)", "Presencia (1)")) +
  # 2. CRUCIAL: Permitir que los puntos se dibujen fuera del área de trazado
  coord_cartesian(clip = "off") +
  labs(title = "Scatterplot",
       x = "Índice VARI",
       y = "Índice TGI") +
  theme_minimal() +
  # 3. Añadimos margen extra al plot completo para que no choquen con los números
  theme(plot.margin = margin(10, 20, 10, 10))



