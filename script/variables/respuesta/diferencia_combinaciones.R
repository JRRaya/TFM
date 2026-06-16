# 1. Carga de paquetes y limpieza de entorno
# 1.1. Limpiamos la RAM
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
gc(full = TRUE)

# 1.2. Cargamos las librerías a emplear
pacman::p_load(sf, terra, dplyr, here)

# 2. Carga de datos
# 2.1. Polígonos anuales de ailanto
ailanto <-  sf::st_read(
  here::here("data/presencias/ailanto/ailanthus_all.shp"),
  quiet = TRUE
)

# 2.2. Plantilla ráster
plantilla <- terra::rast(
  here::here("data/variables/variables_bioticas/basic/basic_lidar1.tif"), 
  lyrs = 1
)

# 3. Generar diferencias
# 3.1. Especificar años de la serie
years <- 2008:2023

# 3.2. Bucle de generación y guardado iterativo
for (year_a in years) {
  for (year_b in years) {
    # Saltar si son el mismo año   
    if (year_a == year_b) next  
    
    # Extraer polígonos para los años específicos
    p_a <- ailanto %>% 
      dplyr::filter(Year == year_a) %>% 
      dplyr::select(ID) %>% 
      sf::st_make_valid() 
    
    p_b <- ailanto %>% 
      dplyr::filter(Year == year_b) %>% 
      dplyr::select(ID) %>% 
      sf::st_make_valid() 

    # Verificamos que ambas capas contengan poligonos validos
    if (nrow(p_a) == 0 || nrow(p_b) == 0) next

    # Extraer diferencia A - B (e.g.: 2023 - 2008)
    diferencia <- sf::st_difference(
      sf::st_union(p_a), 
      sf::st_union(p_b)
    )
    
    # En caso de que existan polígonos:
    if (!all(sf::st_is_empty(diferencia))) { 
      # Generamos el nombre para la operación concreta
      nombre <- sprintf("diferencia_%s_%s", year_a, year_b)

      # Convertir a sf 
      diferencia_sf <- sf::st_as_sf(
        data.frame(id = 1), 
        geometry = diferencia
      )
      
      # Guardado del shapefile
      sf::st_write(
        diferencia, 
        here("data/presencias/ailanto/diferencia/shapefile", paste0(nombre, ".gpkg")), 
        quiet = TRUE, 
        delete_dsn = TRUE
      )
      
      # Rasterización 
      diferencia_rast <- diferencia %>% 
        terra::vect() %>% 
        terra::rasterize(
          plantilla,
          field = 1,
          background = 0
        )
      
      # En caso de que A sea menor que B, convertir B en NA
      if (year_a > year_b) {
        diferencia_rast <- diferencia_rast %>% 
          terra::mask(
            p_b,
            inverse = TRUE
          )
      }

      # Guardado del ráster
      terra::writeRaster(
        diferencia_rast, 
        here("data/presencias/ailanto/diferencia/raster", paste0(nombre, ".tif")), 
        overwrite = TRUE
      )

      # Limpieza de memoria
      rm(p_a, p_b, diferencia, diferencia_rast)
      gc(verbose = TRUE, full = TRUE)
    } 
  }

  # Limpieza de archivos temporales en disco
  terra::tmpFiles(current = TRUE, orphan = TRUE, old = TRUE, remove = TRUE)
}