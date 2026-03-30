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

# 3. Construcción de Modelos Lineales Generalizados (GLMs) univariantes para cada dataset
# 3.1. Función de procesado
glm_univariante <- function(input_raster, predicha, predictoras, ruta_salida) {
  # 1. Crear directorios base de forma dinámica
  dir_graficos <- file.path(ruta_salida, predicha, "graficos")
  dir_modelos  <- file.path(ruta_salida, predicha, "modelos")
  
  dir.create(dir_graficos, recursive = TRUE, showWarnings = FALSE)
  dir.create(dir_modelos,  recursive = TRUE, showWarnings = FALSE)
  
  # Lista de los productos para cada variable
  lista_variables <- list()

  # Bucle iterativo de generación de productos
  for (i in predictoras) {
    # Conversión a dataframe
    df <- terra::as.data.frame(
      input_raster[[c(predicha, i)]], 
      cells = FALSE, 
      na.rm = TRUE
    )
    
    # Eliminamos valores no finitos
    df <- df[is.finite(df[[i]]) & is.finite(df[[predicha]]), ]

    # Separación de clases para el balanceo
    df_1 <- df[df[[predicha]] == 1, ]
    df_0 <- df[df[[predicha]] == 0, ]
    
    # Extraemos el número de filas para cada una de las clases
    n_1 <- nrow(df_1)
    n_0 <- nrow(df_0)

    # Control de calidad
    if (n_1 < 10 | n_0 < 10) {
      next
    }

    # Downsampling de la clase mayoritaria
    set.seed(123)

    if (n_0 > n_1) {
      df_0_sub <- df_0[sample(nrow(df_0), n_1), ]
      df_balanceado <- bind_rows(df_0_sub, df_1)
    } else if (n_1 > n_0) {
      df_1_sub <- df_1[sample(nrow(df_1), n_0), ]
      df_balanceado <- bind_rows(df_0, df_1_sub)
    } else {
      df_balanceado <- df
    }

    ## Modelado 
    # Fórmula GLM
    f <- stats::as.formula(paste(predicha, "~", i))
    
    # Modelado (GLM regresión logística)
    m <- glm(f, data = df_balanceado, family = stats::binomial(link = "logit"))

    ## Gráficos y estadísticos
    # Cálculo de Pseudo-R2
    r2 <- DescTools::PseudoR2(m, which = "all")

    # Gráfico de curva de respuesta (regresión logística)
    curva <- ggplot2::ggplot(df_balanceado, aes(x = .data[[i]], y = .data[[predicha]])) +
      ggplot2::geom_point(alpha = 0.3, color = "red") +
      ggplot2::stat_smooth(
        method      = "glm",
        formula = y ~ x,
        method.args = list(family = binomial),
        se          = TRUE,
        color       = "steelblue"
      ) +
      ggplot2::labs(
        x        = i,
        y        = "Probabilidad de invasión",
        title    = paste("Curva de respuesta:", i),
        subtitle = paste("Pseudo-R2 (Nagelkerke):", round(r2["Nagelkerke"], 3))
      ) +
      ggplot2::theme_classic()

    # Gráfico de cajas
    cajas <- ggplot(df_balanceado, aes(x = as.factor(.data[[predicha]]), y = .data[[i]], fill = as.factor(.data[[predicha]]))) +
      ggplot2::geom_boxplot() +
      ggplot2::labs(
        title = paste0("Valores de ", i, " para las clases de presencias"),
        x = "Presencia (0 = No, 1 = Sí)",
        y = "Índice VARI"
      ) +
      ggplot2::scale_fill_manual(values = c("steelblue", "red"), guide = "none") +
      ggplot2::theme_minimal()

    # Guardado de los gráficos
    ggplot2::ggsave(
      filename = file.path(dir_graficos, paste0("curva_", i, ".png")),
      plot = curva, width = 8, height = 5, dpi = 300
    )

    ggplot2::ggsave(
      filename = file.path(dir_graficos, paste0("cajas_", i, ".png")),
      plot = cajas, width = 8, height = 5, dpi = 300
    )

    stats_df <- as.data.frame(summary(m)$coefficients)
    stats_df$variable <- i
    stats_df$nagelkerke <- r2["Nagelkerke"]
    
    write.csv(
      stats_df, 
      file = file.path(dir_modelos, paste0("stats_", i, ".csv")),
      row.names = TRUE
    )

    # Almacenamiento en lista
    lista_variables[[i]] <- list(
      modelo = m, 
      pseudo_r2 = r2, 
      grafico_regresion = curva, 
      grafico_cajas = cajas
    )
    
    # Limpieza
    gc(verbose = FALSE, full = TRUE)
  }

  terra::tmpFiles(current = TRUE, orphan = TRUE, old = TRUE, remove = TRUE)
  return(lista_variables)
}

# 3.2. Aplicar función
glm_2008_2013 <- glm_univariante(
  input_raster = variables_2008_2013,
  predicha = "ailanto_2008_2013_rast",
  predictoras = names(variables_2008_2013)[-1],
  ruta_salida = here::here("output/glm_univariantes")
)

glm_2014_2023 <- glm_univariante(
  input_raster = variables_2014_2023,
  predicha = "ailanto_2014_2023_rast",
  predictoras = names(variables_2014_2023)[-1],
  ruta_salida = here::here("output/glm_univariantes")
)

glm_2008_2023 <- glm_univariante(
  input_raster = variables_2008_2023,
  predicha = "ailanto_2008_2023_rast",
  predictoras = names(variables_2008_2023)[-1],
  ruta_salida = here::here("output/glm_univariantes")
)