# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear·
pacman::p_load(callr)

# 2. Definir nombres de los scripts a ejecutar
scripts <- c(
  "script/variables/variables_bioticas/basic.R",
  "script/variables/variables_bioticas/canopydensity.R",
  "script/variables/variables_bioticas/echo.R",
  "script/variables/variables_bioticas/echo2.R",
  "script/variables/variables_bioticas/HOME.R",
  "script/variables/variables_bioticas/interval.R",
  "script/variables/variables_bioticas/kde.R", 
  "script/variables/variables_bioticas/lad.R",
  "script/variables/variables_bioticas/Lmoments.R", 
  "script/variables/variables_bioticas/percabove.R", 
  "script/variables/variables_bioticas/percentiles.R",
  "script/variables/variables_bioticas/rumple.R",
  "script/variables/variables_bioticas/texture.R",
  "script/variables/variables_bioticas/voxels.R",
  "script/variables/variables_bioticas/fd.R"
)

# 3. Localizar automáticamente el ejecutable de la sesión actual
r_path <- file.path(R.home("bin"), "Rscript")

# 3. Ejecutar scripts en entornos independientes
for (s in scripts) {
  status <- system2(
    r_path, 
    args = shQuote(s), 
    wait = TRUE, 
    stdout = "", 
    stderr = ""
  )
}