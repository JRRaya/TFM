# Conversión por lotes de ECW a GeoTIFF mediante GDAL (terminal)
# Requiere GDAL instalado en el sistema con soporte ECW

library(here)

# Directorio con los archivos ECW
dir_entrada <- here::here("data/variables/variables_bioticas/funcional/rgb/orto_2013")

# Buscar archivos ECW
archivos_ecw <- list.files(dir_entrada, pattern = "\\.ecw$", full.names = TRUE)

# Construir y ejecutar el comando gdal_translate para cada archivo
for (ruta_ecw in archivos_ecw) {
  ruta_tif <- sub("\\.ecw$", ".tif", ruta_ecw)
  comando  <- paste("gdal_translate -of GTiff", shQuote(ruta_ecw), shQuote(ruta_tif))
  system(comando)
}