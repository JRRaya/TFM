# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
future::plan(future::sequential)
rm(list = ls(all.names = TRUE))
gc(verbose = FALSE, full = TRUE)
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(dplyr, purrr, furrr, future, here, stats, ggcorrplot, usdm, ggplot2)

# 1.3. Configurar paralelismo
future::plan(multisession, workers = 8L)

# 2. Función auxiliar para guardar corrplots con ggcorrplot
guardar_corrplot <- function(cor_matrix, ruta_pdf) {
  n    <- ncol(cor_matrix)
  size <- max(8, n * 0.28)

  # Convertir matriz a formato largo, triángulo inferior
  cor_df <- as.data.frame(as.table(cor_matrix)) %>%
    dplyr::rename(x = Var1, y = Var2, value = Freq) %>%
    dplyr::mutate(
      x = factor(x, levels = rownames(cor_matrix)),
      y = factor(y, levels = rownames(cor_matrix))
    ) %>%
    dplyr::filter(as.integer(x) > as.integer(y))

  p <- ggplot2::ggplot(cor_df, ggplot2::aes(x = x, y = y, fill = value)) +
    ggplot2::geom_tile(color = "grey85", linewidth = 0.15) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(value, 2)),
      size  = 1.6,
      color = ifelse(abs(cor_df$value) > 0.6, "white", "grey20")
    ) +
    ggplot2::scale_fill_gradient2(
      low      = "blue",
      mid      = "white",
      high     = "red",
      midpoint = 0,
      limits   = c(-1, 1),
      name     = "Corr"
    ) +
    ggplot2::scale_x_discrete(position = "bottom") +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal(base_size = 7) +
    ggplot2::theme(
      axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1, size = 5.5),
      axis.text.y      = ggplot2::element_text(size = 5.5),
      axis.title       = ggplot2::element_blank(),
      panel.grid       = ggplot2::element_blank(),
      legend.key.height = ggplot2::unit(0.8, "cm"),
      legend.key.width  = ggplot2::unit(0.3, "cm"),
      legend.text       = ggplot2::element_text(size = 6),
      legend.title      = ggplot2::element_text(size = 7),
      plot.margin       = ggplot2::margin(5, 5, 5, 5)
    )

  ggplot2::ggsave(
    filename  = ruta_pdf,
    plot      = p,
    width     = size,
    height    = size,
    units     = "in",
    limitsize = FALSE
  )
}

# 3. Función principal
filtrado_vif <- function(ruta_escalado, ruta_original, ruta_salida, etiqueta) {
  # 1. Crear directorio en caso de que no exista
  dir.create(ruta_salida, recursive = TRUE, showWarnings = FALSE)

  # 2. Carga datasets
  df_escalado <- readRDS(ruta_escalado) %>% base::as.data.frame()
  df_original <- readRDS(ruta_original) %>% base::as.data.frame()

  # 3. Identificar variables categóricas del dataset original
  cols_categoricas <- c(
    names(df_original)[startsWith(names(df_original), "diferencia")],
    names(df_original)[startsWith(names(df_original), "mucva")],
    names(df_original)[startsWith(names(df_original), "landcover")]
  )

  # 4. Extraer columnas categóricas del dataset original
  df_categoricas <- df_original[, cols_categoricas, drop = FALSE]

  # 5. Calcular correlaciones iniciales
  cor_i <- stats::cor(df_escalado, method = "spearman")

  guardar_corrplot(
    cor_matrix = cor_i,
    ruta_pdf   = file.path(ruta_salida, paste0("cor_i_", etiqueta, ".pdf"))
  )

  # 6. Análisis VIF
  vifcor <- usdm::vifcor(df_escalado, th = 0.7, method = "spearman")

  # 7. Filtrado de variables
  cols_conservadas <- names(usdm::exclude(df_escalado, vifcor))
  df_escalado_f    <- df_escalado[, cols_conservadas, drop = FALSE]

  # 8. Correlaciones post-filtrado
  cor_f <- stats::cor(df_escalado_f, method = "spearman")

  guardar_corrplot(
    cor_matrix = cor_f,
    ruta_pdf   = file.path(ruta_salida, paste0("cor_f_", etiqueta, ".pdf"))
  )

  # 9. Generar dataset final
  df_final <- dplyr::bind_cols(df_categoricas, df_escalado_f) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::contains("diferencia"),
        ~ factor(
            ifelse(as.integer(.x) == 1L, "Presencia", "Ausencia"),
            levels = c("Presencia", "Ausencia")
          )
      )
    )

  # 10. Guardado
  saveRDS(df_final, file.path(ruta_salida, paste0("df_f_", etiqueta, ".rds")))
}

# 4. Definir datasets
# 4. Definir datasets
datasets <- list(
  # Datasets sin balanceo
  list(
    ruta_escalado = here::here("data/modelos/escalado/df_e_2008_2012.rds"),
    ruta_original = here::here("data/modelos/normalizacion/df_2008_2012.rds"),
    ruta_salida   = here::here("data/modelos/eda/vif/2008_2012"),
    etiqueta      = "e_2008_2012"
  ),
  list(
    ruta_escalado = here::here("data/modelos/escalado/df_e_2008_2023.rds"),
    ruta_original = here::here("data/modelos/normalizacion/df_2008_2023.rds"),
    ruta_salida   = here::here("data/modelos/eda/vif/2008_2023"),
    etiqueta      = "e_2008_2023"
  ),
  list(
    ruta_escalado = here::here("data/modelos/escalado/df_e_2015_2023.rds"),
    ruta_original = here::here("data/modelos/normalizacion/df_2015_2023.rds"),
    ruta_salida   = here::here("data/modelos/eda/vif/2015_2023"),
    etiqueta      = "e_2015_2023"
  ),
  list(
    ruta_escalado = here::here("data/modelos/escalado/df_e_2020_2023.rds"),
    ruta_original = here::here("data/modelos/normalizacion/df_2020_2023.rds"),
    ruta_salida   = here::here("data/modelos/eda/vif/2020_2023"),
    etiqueta      = "e_2020_2023"
  ),
  # Datasets balanceados
  list(
    ruta_escalado = here::here("data/modelos/escalado/df_be_2008_2012.rds"),
    ruta_original = here::here("data/modelos/balanceo/df_b_2008_2012.rds"),
    ruta_salida   = here::here("data/modelos/eda/vif/2008_2012"),
    etiqueta      = "be_2008_2012"
  ),
  list(
    ruta_escalado = here::here("data/modelos/escalado/df_be_2008_2023.rds"),
    ruta_original = here::here("data/modelos/balanceo/df_b_2008_2023.rds"),
    ruta_salida   = here::here("data/modelos/eda/vif/2008_2023"),
    etiqueta      = "be_2008_2023"
  ),
  list(
    ruta_escalado = here::here("data/modelos/escalado/df_be_2015_2023.rds"),
    ruta_original = here::here("data/modelos/balanceo/df_b_2015_2023.rds"),
    ruta_salida   = here::here("data/modelos/eda/vif/2015_2023"),
    etiqueta      = "be_2015_2023"
  ),
  list(
    ruta_escalado = here::here("data/modelos/escalado/df_be_2020_2023.rds"),
    ruta_original = here::here("data/modelos/balanceo/df_b_2020_2023.rds"),
    ruta_salida   = here::here("data/modelos/eda/vif/2020_2023"),
    etiqueta      = "be_2020_2023"
  )
)

# 5. Ejecutar en paralelo
furrr::future_walk(
  datasets,
  ~ filtrado_vif(
      ruta_escalado = .x$ruta_escalado,
      ruta_original = .x$ruta_original,
      ruta_salida   = .x$ruta_salida,
      etiqueta      = .x$etiqueta
    ),
  .options = furrr::furrr_options(seed = TRUE)
)

# 6. Detener paralelización
future::plan(sequential)