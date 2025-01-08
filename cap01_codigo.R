####### SCRIPT FET AMB R Version: 2024.12.0+467# #Released: 2024-12-16 #

#1.Tratamiento inicial ####
##1.1. Instalar paquetes (con condicional) y cargar librerias ----

packages <- c("lwgeom", "osmdata", "sf", "tidyverse", "readr", "ggspatial", "readxl", "openxlsx", "dplyr", "Polychrome", "scales", "gridExtra", "fmsb", "likert", "reshape")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}
lapply(packages, install_if_missing)

## 1.2. Definir ruta base para los datos----

## Ruta para obtener datos originales
ruta_base <- "D:/BASES DE DATOS/CAPITULO_01/datos_cap01/datos_originales_cap01/"

#Ruta para obtener datos limpios
ruta_base_limpios <- "D:/BASES DE DATOS/CAPITULO_01/datos_cap01/datos_limpios_cap01/"


## 1.3. Importar Excel de la BD ----

# Crear la ruta completa del archivo
file_path <- paste0(ruta_base, "BIOTRANS_dataset2.0_v2_311224.xlsx")

# Obtener los nombres de todas las hojas del excel
sheet_names <- getSheetNames(file_path)

# Leer todas las hojas como dataframe
all_sheets <- lapply(sheet_names, function(sheet) {
  read.xlsx(file_path, sheet = sheet)
})

# Nombra la lista para referencia 
names(all_sheets) <- sheet_names
sheet_names

#2. Mapa_entrevistas####

##A) VERSION A (% embarcaciones) ####

##2.1. Elige la hojas y trasnformalo en dataframe----
head(all_sheets[["00_entrevistas"]])

df_mapa <- read_excel(file_path, sheet = "00_entrevistas")

##2.2. Definir límits pel mapa (bb)----
lon_min <- 1.00 # xmin
lon_max <- 4.50 # xmax
lat_min <- 38.50 # ymin
lat_max <- 40.50 # ymax

bb <- c(lon_min, lat_min, lon_max, lat_max)

##2.3. Obtenir d'Open Street Map (osm) la línea de costa, acotada als límits de "bb"
osm_data_coastline <- opq(bb = bb) %>%
  add_osm_feature(key = 'natural', value = 'coastline') %>%
  osmdata_sf()

##2.4. Obtenir els polígons per la terra (land) i mar (sea) a partir de les línees d'osm----
# OSM només són objectes lineals, no poligonals. Per obtenir polígons hem de fer el pas 4): A partir de les linees de costa (4.1), retallem un rectangle creat, que ocupa la zona a mapejar (4.2), per obtenir els polígons per les zones de terra (4.3 i 4.4) 

### 2.4.1. Obtenir d'Open Street Map (osm) la línea de costa, acotada als límits de "bb"----
# Aquest punt és igual que "3)", però allà hem generat un objecte (osm_data_coastline) que utilitzarem en sí mateix per fer el mapa (la línea de costa) i ara generem un altre objecte (coast) que farem servir per retallar els polígons que volem. Podria haver fet servir el matiex pels dos casos, però ho separo perquè quedi més clar el pas
coast <- opq(bb) %>%
  add_osm_feature(key = "natural", value = "coastline") %>%
  osmdata_sf()

### 2.4.2. Crear el rectangle (poligon) dins dels limits de "bb"----
bb_rect <- data.frame(
  lat = c(lat_min, lat_max),
  lon = c(lon_min, lon_max)
) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

### 2.4.3. Retallar dins del rectangle, en base a la línea de costa (coast) de 4.1----
bb_rect_split <- bb_rect %>% 
  st_split(coast$osm_lines) %>% 
  st_collection_extract("POLYGON")

### 2.4.4. Extreure del polígons retallats a 4.3, els que són terra (land)---- 
# En aquest cas, de la llista, 1 era mar i del 2 al 10 polígons de terra. No sempre serà fins a 10, depenent del mapa tindràs més polígons que formen trossos de terra. I no sé si el mar sempre serà l'1 (suposo que sí, pero si hi ha barreres naturals que divideixi la massa d'aigua, per exemple, pot ser que sigui més d'un polígon).
land <- bb_rect_split[2:10]

### 2.4.5. Mapa ràpid (provisional) per comprovar que "land" són els poligons 2 a 10 de la llista----
# Si al fer aquest mapa, no surt "pintada" la terra que vols, és que els polígons que vols de la llista (land) no són 2:10, potser són més o menys, segons la zona. Pots mirar la llista per tenir alguna idea 
ggplot() +
  geom_sf(
    data = land,
    fill = "navy",
    color = NA
  )

##2.5. Convertir data (df_mapa) a sf object----
data_sf <- st_as_sf(df_mapa, coords = c("x", "y"), crs = 4326)

##2.6. Preparar parametros---- 

### 2.6.1. Afegir columna del percentatge d'n respecte nt----
data_sf <- data_sf %>%
  mutate(Percent = (n_emb / emb_inv_2022) * 100)
View(data_sf)

### 2.6.2. Afegir columna del color segons rang del percentatge (creat a 6.1)----
data_sf <- data_sf %>%
  mutate(color = case_when(
    Percent < 20 ~ "red",
    Percent >= 20 & Percent <= 30 ~ "yellow",
    Percent > 30 ~ "green"
  ))

View(data_sf)

### 2.6.3. Valors d'n per l'escala de la llegenda al mapa----
valores_n <- c(1,2,3,4,5,6,7,8,9)
valores_n <- sort(valores_n)

## 2.7. Hacer el mapa----

ggplot() +
  # Terra (land) de color gris
  geom_sf(data = land, fill = "lightgrey", color = NA) +
  
  # Linea de costa (osm_data_coastline) de color negre
  geom_sf(data = osm_data_coastline$osm_lines, aes(geometry = geometry), 
          color = 'black') +
  
  
  # Afegir punts amb el color segons columna color (punt 6.2), amb transparencia (alpha)
  geom_sf(data = data_sf, aes(geometry = geometry, size = entrevistas_tot, color = color), 
          alpha = 0.4, show.legend = TRUE) +
  
  # Afegir cercles una mica més foscos (6.3) al voltant dels punts, pero sense afegirlos a la llegenda
  geom_sf(data = data_sf, aes(geometry = geometry, size = entrevistas_tot, color=color),
          alpha = 1, shape=1, stroke=1.5, show.legend = FALSE) +
  
  # Personalitzar escala de colors per la llegenda del percentatge
  scale_color_manual(
    name = "Percentage",
    values = c("red" = "red", "yellow" = "yellow", "green" = "green"),
    labels = c("< 20%", "20-30%", "> 30%"),
    breaks = c("red", "yellow", "green")) +
  
  # Personalitzar escala de tamanys per la llegenda de Count (n)
  scale_size_continuous(name = "Count (entrevistas_tot)", range = c(3, 10),
                        breaks = valores_n) +
  
  # Personalitzar retocs estètics d'ambdues llegendes
  guides(
    color = guide_legend(override.aes = list(size = 5)),
    size = guide_legend(override.aes = list(color = "black"))
  ) +
  
  # Afegir etiquetes i títols
  labs(
    title = "Interviews with SSF Fishers in the Balearic Islands",
    subtitle = "",
    x = "Longitude",
    y = "Latitude"
  ) +
  
  # Aplicar "theme minimal" amb personalitzacions de font, graella, fons, etc
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "#F5F5F5"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white")
  ) +
  
  # Afegir fletxa del Nord
  annotation_north_arrow(
    location = "tl", 
    which_north = "true", 
    style = north_arrow_fancy_orienteering,
    height = unit(1.5, "cm"),
    width = unit(1.5, "cm") 
  ) +
  
  # Afegir barra d'escala (variar km amb "widht_hint")
  annotation_scale(
    location = "br", 
    width_hint = 0.17, # en aquest cas, degut l'escala del mapa, el valor 0.20 és el que crea una barra de 50 km. Per diferents escales de mapa, el valor correspondrà a diferents km
    bar_cols = c("black", "white"),
    text_col = "black",
    line_width = 0.5,
    height = unit(0.3, "cm"),
    unit_category = "metric",
  )


##2.8. Guardar el mapa----

# Adjust the width and height to inches
width_in_inches <- 12
height_in_inches <- 8.24

# Save the plot with high quality
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/mapa_entrevistasA.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)


##B) VERSION B (% TRIPULACION) ####

##2.1. Elige la hojas y trasnformalo en dataframe----
head(all_sheets[["00_entrevistas"]])

df_mapa <- read_excel(file_path, sheet = "00_entrevistas")

##2.2. Definir límits pel mapa (bb)----
lon_min <- 1.00 # xmin
lon_max <- 4.50 # xmax
lat_min <- 38.50 # ymin
lat_max <- 40.50 # ymax

bb <- c(lon_min, lat_min, lon_max, lat_max)

##2.3. Obtenir d'Open Street Map (osm) la línea de costa, acotada als límits de "bb"
osm_data_coastline <- opq(bb = bb) %>%
  add_osm_feature(key = 'natural', value = 'coastline') %>%
  osmdata_sf()

##2.4. Obtenir els polígons per la terra (land) i mar (sea) a partir de les línees d'osm----
# OSM només són objectes lineals, no poligonals. Per obtenir polígons hem de fer el pas 4): A partir de les linees de costa (4.1), retallem un rectangle creat, que ocupa la zona a mapejar (4.2), per obtenir els polígons per les zones de terra (4.3 i 4.4) 

### 2.4.1. Obtenir d'Open Street Map (osm) la línea de costa, acotada als límits de "bb"----
# Aquest punt és igual que "3)", però allà hem generat un objecte (osm_data_coastline) que utilitzarem en sí mateix per fer el mapa (la línea de costa) i ara generem un altre objecte (coast) que farem servir per retallar els polígons que volem. Podria haver fet servir el matiex pels dos casos, però ho separo perquè quedi més clar el pas
coast <- opq(bb) %>%
  add_osm_feature(key = "natural", value = "coastline") %>%
  osmdata_sf()

### 2.4.2. Crear el rectangle (poligon) dins dels limits de "bb"----
bb_rect <- data.frame(
  lat = c(lat_min, lat_max),
  lon = c(lon_min, lon_max)
) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

### 2.4.3. Retallar dins del rectangle, en base a la línea de costa (coast) de 4.1----
bb_rect_split <- bb_rect %>% 
  st_split(coast$osm_lines) %>% 
  st_collection_extract("POLYGON")

### 2.4.4. Extreure del polígons retallats a 4.3, els que són terra (land)---- 
# En aquest cas, de la llista, 1 era mar i del 2 al 10 polígons de terra. No sempre serà fins a 10, depenent del mapa tindràs més polígons que formen trossos de terra. I no sé si el mar sempre serà l'1 (suposo que sí, pero si hi ha barreres naturals que divideixi la massa d'aigua, per exemple, pot ser que sigui més d'un polígon).
land <- bb_rect_split[2:10]

### 2.4.5. Mapa ràpid (provisional) per comprovar que "land" són els poligons 2 a 10 de la llista----
# Si al fer aquest mapa, no surt "pintada" la terra que vols, és que els polígons que vols de la llista (land) no són 2:10, potser són més o menys, segons la zona. Pots mirar la llista per tenir alguna idea 
ggplot() +
  geom_sf(
    data = land,
    fill = "navy",
    color = NA
  )

##2.5. Convertir data (df_mapa) a sf object----
data_sf <- st_as_sf(df_mapa, coords = c("x", "y"), crs = 4326)

##2.6. Preparar parametros---- 

### 2.6.1. Afegir columna del percentatge d'n respecte nt----
data_sf <- data_sf %>%
  mutate(Percent = (n_emb / trip_inv_2022) * 100)
View(data_sf)

### 2.6.2. Afegir columna del color segons rang del percentatge (creat a 6.1)----
data_sf <- data_sf %>%
  mutate(color = case_when(
    Percent < 20 ~ "red",
    Percent >= 20 & Percent <= 30 ~ "yellow",
    Percent > 30 ~ "green"
  ))

View(data_sf)

### 2.6.3. Valors d'n per l'escala de la llegenda al mapa----
valores_n <- c(1,2,3,4,5,6,7,8,9)
valores_n <- sort(valores_n)

## 2.7. Hacer el mapa----

ggplot() +
  # Terra (land) de color gris
  geom_sf(data = land, fill = "lightgrey", color = NA) +
  
  # Linea de costa (osm_data_coastline) de color negre
  geom_sf(data = osm_data_coastline$osm_lines, aes(geometry = geometry), 
          color = 'black') +
  
  
  # Afegir punts amb el color segons columna color (punt 6.2), amb transparencia (alpha)
  geom_sf(data = data_sf, aes(geometry = geometry, size = entrevistas_tot, color = color), 
          alpha = 0.4, show.legend = TRUE) +
  
  # Afegir cercles una mica més foscos (6.3) al voltant dels punts, pero sense afegirlos a la llegenda
  geom_sf(data = data_sf, aes(geometry = geometry, size = entrevistas_tot, color=color),
          alpha = 1, shape=1, stroke=1.5, show.legend = FALSE) +
  
  # Personalitzar escala de colors per la llegenda del percentatge
  scale_color_manual(
    name = "Percentage",
    values = c("red" = "red", "yellow" = "yellow", "green" = "green"),
    labels = c("< 20%", "20-30%", "> 30%"),
    breaks = c("red", "yellow", "green")) +
  
  # Personalitzar escala de tamanys per la llegenda de Count (n)
  scale_size_continuous(name = "Count (entrevistas_tot)", range = c(3, 10),
                        breaks = valores_n) +
  
  # Personalitzar retocs estètics d'ambdues llegendes
  guides(
    color = guide_legend(override.aes = list(size = 5)),
    size = guide_legend(override.aes = list(color = "black"))
  ) +
  
  # Afegir etiquetes i títols
  labs(
    title = "Interviews with SSF Fishers in the Balearic Islands",
    subtitle = "",
    x = "Longitude",
    y = "Latitude"
  ) +
  
  # Aplicar "theme minimal" amb personalitzacions de font, graella, fons, etc
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "#F5F5F5"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white")
  ) +
  
  # Afegir fletxa del Nord
  annotation_north_arrow(
    location = "tl", 
    which_north = "true", 
    style = north_arrow_fancy_orienteering,
    height = unit(1.5, "cm"),
    width = unit(1.5, "cm") 
  ) +
  
  # Afegir barra d'escala (variar km amb "widht_hint")
  annotation_scale(
    location = "br", 
    width_hint = 0.17, # en aquest cas, degut l'escala del mapa, el valor 0.20 és el que crea una barra de 50 km. Per diferents escales de mapa, el valor correspondrà a diferents km
    bar_cols = c("black", "white"),
    text_col = "black",
    line_width = 0.5,
    height = unit(0.3, "cm"),
    unit_category = "metric",
  )


##2.8. Guardar el mapa----

# Adjust the width and height to inches
width_in_inches <- 12
height_in_inches <- 8.24

# Save the plot with high quality
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/mapa_entrevistasB.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)


#3. Mapa zonas ####
##3.1. Elige la hojas y trasnformalo en dataframe----
head(all_sheets[["00_entrevistas"]])

df_mapa <- read_excel(file_path, sheet = "00_entrevistas")

##3.2. Definir límits pel mapa (bb)----
lon_min <- 1.00 # xmin
lon_max <- 4.50 # xmax
lat_min <- 38.50 # ymin
lat_max <- 40.50 # ymax

bb <- c(lon_min, lat_min, lon_max, lat_max)

##3.3. Obtenir d'Open Street Map (osm) la línea de costa, acotada als límits de "bb"----
osm_data_coastline <- opq(bb = bb) %>%
  add_osm_feature(key = 'natural', value = 'coastline') %>%
  osmdata_sf()

##3.4. Obtenir els polígons per la terra (land) i mar (sea) a partir de les línees d'osm----
# OSM només són objectes lineals, no poligonals. Per obtenir polígons hem de fer el pas 4): A partir de les linees de costa (4.1), retallem un rectangle creat, que ocupa la zona a mapejar (4.2), per obtenir els polígons per les zones de terra (4.3 i 4.4) 

### 3.4.1. Obtenir d'Open Street Map (osm) la línea de costa, acotada als límits de "bb"----
# Aquest punt és igual que "3)", però allà hem generat un objecte (osm_data_coastline) que utilitzarem en sí mateix per fer el mapa (la línea de costa) i ara generem un altre objecte (coast) que farem servir per retallar els polígons que volem. Podria haver fet servir el matiex pels dos casos, però ho separo perquè quedi més clar el pas
coast <- opq(bb) %>%
  add_osm_feature(key = "natural", value = "coastline") %>%
  osmdata_sf()

### 3.4.2. Crear el rectangle (poligon) dins dels limits de "bb"----
bb_rect <- data.frame(
  lat = c(lat_min, lat_max),
  lon = c(lon_min, lon_max)
) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

### 3.4.3. Retallar dins del rectangle, en base a la línea de costa (coast) de 4.1----
bb_rect_split <- bb_rect %>% 
  st_split(coast$osm_lines) %>% 
  st_collection_extract("POLYGON")

### 3.4.4. Extreure del polígons retallats a 4.3, els que són terra (land)---- 
# En aquest cas, de la llista, 1 era mar i del 2 al 10 polígons de terra. No sempre serà fins a 10, depenent del mapa tindràs més polígons que formen trossos de terra. I no sé si el mar sempre serà l'1 (suposo que sí, pero si hi ha barreres naturals que divideixi la massa d'aigua, per exemple, pot ser que sigui més d'un polígon).
land <- bb_rect_split[2:10]

### 3.4.5. Mapa ràpid (provisional) per comprovar que "land" són els poligons 2 a 10 de la llista----
# Si al fer aquest mapa, no surt "pintada" la terra que vols, és que els polígons que vols de la llista (land) no són 2:10, potser són més o menys, segons la zona. Pots mirar la llista per tenir alguna idea 
ggplot() +
  geom_sf(
    data = land,
    fill = "navy",
    color = NA
  )

##3.5. Convertir data (df_mapa) a sf object----
data_sf <- st_as_sf(df_mapa, coords = c("x", "y"), crs = 4326)

##3.6. Cargar la shapefile con las reservas----

shapefile_path1 <- "D:/BASES DE DATOS/01_GIS/Reservas_Baleares_1224/Límits_Reserves_Marines_Illes_Balears.shp" 
shapefile_data1 <- st_read(shapefile_path1)

shapefile_path2 <- "D:/BASES DE DATOS/01_GIS/PNMT_Archipielago_Cabrera/Limites/Limite_Archipielago_Cabrera.shp" 
shapefile_data2 <- st_read(shapefile_path2)


## 3.7 Hacer el mapa----
ggplot() +
  # Superposición de la primera shapefile con la etiqueta "AMPs"
  geom_sf(data = shapefile_data1, aes(fill = "AMPs"), color = NA, alpha = 0.3) +
  
  # Superposición de la segunda shapefile con la etiqueta "Cabrera Archipelago National Park"
  geom_sf(data = shapefile_data2, aes(fill = "Cabrera National Park"), color = NA, alpha = 0.3) +
  
  # Tierra (land) de color gris
  geom_sf(data = land, fill = "lightgrey", color = NA) +
  
  # Línea de costa (osm_data_coastline) de color negro
  geom_sf(data = osm_data_coastline$osm_lines, color = 'black') +
  
  # Puntos negros en las localizaciones de las cofradías
  geom_sf(data = data_sf, color = "darkgreen", size = 4) +
  
  
  # Añadir etiquetas y títulos
  labs(
    title = "SSF Fishers Areas",
    subtitle = "",
    x = "Longitude",
    y = "Latitude",
    fill = "" # Título de la leyenda
  ) +
  
  # Ajustar la leyenda y las capas
  scale_fill_manual(
    values = c("AMPs" = "red", 
               "Cabrera National Park" = "blue")
  ) +
  
  # Personalizar retocos estéticos de las leyendas
  guides(
    fill = guide_legend(
      override.aes = list(alpha = 0.3),
      title.position = "top",
      title.hjust = 0.3
    )
  ) +

  # Aplicar "theme minimal" con personalizaciones
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = c(0.8, 0.05), # Usar coordenadas relativas para la posición
    legend.justification = c(0, 0), # Anclar la esquina inferior izquierda
    legend.background = element_rect(fill = "white", color = NA, linewidth = 0.2), # Fondo blanco con borde
    panel.grid.major = element_line(color = "#F5F5F5"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white")
  ) +
  
  # Añadir flecha del norte
  annotation_north_arrow(
    location = "tl", 
    which_north = "true", 
    style = north_arrow_fancy_orienteering,
    height = unit(1.5, "cm"),
    width = unit(1.5, "cm") 
  ) +
  
  # Añadir barra de escala
  annotation_scale(
    location = "br", 
    width_hint = 0.17, 
    bar_cols = c("black", "white"),
    text_col = "black",
    line_width = 0.5,
    height = unit(0.3, "cm"),
    unit_category = "metric"
  )

##3.8. Guardar el mapa----

# Adjust the width and height to inches
width_in_inches <- 12
height_in_inches <- 8.24

# Save the plot with high quality
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/mapa_zonas.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)


#4.Conflictos por zonas ---Gráfico likert actividades####
##4.1. cargar datos y organizarlos----
head(all_sheets[["E_ambiental"]])
head(all_sheets[["A_perfil"]])

# Read the specific sheets into dataframes
e_ambiental <- read_excel(file_path, sheet = "E_ambiental")
a_perfil <- read_excel(file_path, sheet = "A_perfil")

# Select the relevant columns from A_perfil
a_perfil_selected <- a_perfil %>% select(ID, cofradia)

# Merge the dataframes on the ID column
merged_data_actividades <- e_ambiental %>%
  left_join(a_perfil_selected, by = "ID")

View(merged_data_actividades)

unique(merged_data_actividades$cofradia)

##4.2. Preparar los datos para el gráfico likert----
# Crear una nueva columna "zona" basada en los valores de 'cofradia'
merged_data_actividades <- merged_data_actividades %>%
  mutate(zona = case_when(
    cofradia %in% c("Andratx","Soller", "Pollença") ~ "Mallorca North",
    cofradia %in% c("Palma", "Colonia de Sant Jordi", "Santanyi", "Portocolom" ) ~ "Mallorca South-West",
    cofradia %in% c("Alcudia","Cala Ratjada", "Porto Cristo") ~ "Mallorca South-East",
    cofradia %in% c("San Antoni de Portmany", "Eivissa", "Formentera") ~ "Pitiusas",
    cofradia %in% c("Ciutadella", "Fornells", "Mao") ~ "Menorca",
    TRUE ~ "otro" # Para valores que no están en los rangos especificados
  ))

#Contar entrevistas por zona
recuento_zona <- merged_data_actividades %>%
  group_by(zona) %>%
  summarise(recuento = n())
print(recuento_zona)

#Filtrar las columnas relevantes

likert_data_actividades <- merged_data_actividades %>% 
  select(zona, Q21a_profesional:Q21m_turismo)

# Renombrar una columna
likert_data_actividades <- likert_data_actividades %>%
  dplyr::rename(isla_cofradia = zona)

# Definir los niveles comunes
common_levels <- c("Very Negative", "Negative", "Neutral","Positive", "Very Positive")


# Función para renombrar los niveles de una columna
rename_levels <- function(column) {
  column <- as.character(column)
  column <- case_when(
    column == "muy negativo" ~ "Very Negative",
    column == "negativo" ~ "Negative",
    column == "neutro" ~ "Neutral",
    column == "positivo" ~ "Positive",
    column == "muy positivo" ~ "Very Positive",
    TRUE ~ column  # Caso por defecto para mantener el valor original si no hay coincidencia
  )
  factor(column, levels = common_levels)
}

# Aplicar la función a todas las columnas de interés
likert_data_actividades[ , -1] <- lapply(likert_data_actividades[ , -1], rename_levels)

# Asegurar que todas las columnas tengan los mismos niveles
for (col in names(likert_data_actividades)[-1]) {
  likert_data_actividades[[col]] <- factor(likert_data_actividades[[col]], levels = common_levels)
}

# Verificar los niveles de cada columna
lapply(likert_data_actividades, levels)

#convertir en dataframe

likert_data_actividades <- as.data.frame(likert_data_actividades) 

# Especificar la ruta y el nombre del archivo CSV
ruta_archivo <- "D:/BASES DE DATOS/CAPITULO_01/datos_cap01/datos_limpios_cap01/likert_data_actividades.csv"

# Guardar el dataframe en formato CSV
write.csv(likert_data_actividades, file = ruta_archivo, row.names = FALSE)


#cambiar los nombres de las columnas
print(colnames(likert_data_actividades))



# Cambiar los nombres de las columnas
colnames(likert_data_actividades) <- c(
  "isla_cofradia",
  "Industrial Fisheries",
  "Recreational Spearfishing",
  "Recreational Boat Fishing",
  "Recreational Shore Fishing",
  "Recreational Boats",
  "Jet Skis",
  "Scuba Diving",
  "Other Recreational Activities",
  "Maritime Transport",
  "Sewage Outfalls",
  "Coastal Development",
  "Beach Users", 
  "Tourism"
  
)

print(colnames(likert_data_actividades))


##4.3.Crear gráficos likert por zona----
# Función para crear gráficos de Likert por isla
create_likert_chart <- function(data, island_name, common_levels) {
  # Filtrar los datos por isla
  island_data <- data %>%
    filter(isla_cofradia == island_name)
  
  # Remover la columna 'isla_cofradia' para el análisis de Likert
  island_data <- island_data %>% select(-isla_cofradia)
  
  # Asegurarse de que todas las columnas tengan los mismos niveles
  island_data[] <- lapply(island_data, function(column) factor(column, levels = common_levels))
  
  # Crear el objeto de Likert
  likert_obj <- likert(island_data)
  
  # Crear el gráfico de Likert
  plot(likert_obj, group.order = c(
    "Industrial Fisheries",
    "Recreational Spearfishing",
    "Recreational Boat Fishing",
    "Recreational Shore Fishing",
    "Recreational Boats",
    "Jet Skis",
    "Scuba Diving",
    "Other Recreational Activities",
    "Maritime Transport",
    "Sewage Outfalls",
    "Coastal Development",
    "Beach Users", 
    "Tourism")
  ) + ggtitle(paste("Likert Chart for Activities", island_name))+
    theme_minimal(base_size = 15) + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 20, face = "bold"),
      legend.title = element_blank(),
      legend.position = "top",
      legend.text = element_text(size = 20),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# Nombres de las islas
island_names <- unique(merged_data_actividades$zona)


# Crear una lista para almacenar los gráficos
likert_plots <- list()

# Crear gráficos de Likert para cada isla
for (island in island_names) {
  print(create_likert_chart(likert_data_actividades, island, common_levels))
}

# Crear gráficos de Likert para cada isla y almacenarlos en la lista
for (island in island_names) {
  likert_plots[[island]] <- create_likert_chart(likert_data_actividades, island, common_levels)
}

##4.4. Guardar los gráficos con calidad específica----
width_in_inches <- 14
height_in_inches <- 8
for (island in names(likert_plots)) {
  ggsave(paste0("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/likert_chart_act", island, ".png"), plot = likert_plots[[island]], 
         dpi = 300, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)
}

##4.5. Crear Likert agrupado----
###4.5.1.Likert agrupado todo junto

xlikgroup = likert(likert_data_actividades[,2:14], grouping = likert_data_actividades$isla_cofradia)
xlikgroup

plot(xlikgroup, type = "bar", centered = T)#Se ordena por orden alfabético

###4.5.2 Guardar el gráfico con calidad específica----
width_in_inches <- 14
height_in_inches <- 25
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/likert_act_todo.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)


###4.5.3.Gráfico densidad todo junto----
plot(xlikgroup, type = "density", centered = F)

###4.5.4 Guardar el gráfico con calidad específica----
width_in_inches <- 14
height_in_inches <- 25
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/likert_act_todo_dens.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)


###4.5.5 Grafico likert y densidad agrupado por temática----
####4.5.5.1. Otras pescas----

#grafico likert otras pescas
xlikgroup1 = likert(likert_data_actividades[,2:5], grouping = likert_data_actividades$isla_cofradia)
xlikgroup1


plot(xlikgroup1, type = "bar", centered = T)

# Guardar los gráficos con calidad específica
width_in_inches <- 14
height_in_inches <- 8
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/likert_act_todo01.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)


#Grafico densidad rçotras pescas
plot(xlikgroup1, type = "density", centered = F)


# Guardar los gráficos con calidad específica
width_in_inches <- 14
height_in_inches <- 8
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/likert_act_dens01.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)


####4.5.5.2. Recreativo no extractivo----

#grafico likert recreativo no extractivo
xlikgroup2 = likert(likert_data_actividades[c(6:9, 13:14)], grouping = likert_data_actividades$isla_cofradia)
xlikgroup2


plot(xlikgroup2, type = "bar", centered = T)

# Guardar los gráficos con calidad específica
width_in_inches <- 14
height_in_inches <- 12
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/likert_act_todo02.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)


#Grafico densidad recreativo no extractivo
plot(xlikgroup2, type = "density", centered = T)


# Guardar los gráficos con calidad específica
width_in_inches <- 14
height_in_inches <- 12
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/likert_act_dens02.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)

####4.5.5.2. Desarrollo----

#grafico likert desarrollo
xlikgroup3 = likert(likert_data_actividades[,10:12], grouping = likert_data_actividades$isla_cofradia)
xlikgroup3


plot(xlikgroup3, type = "bar", centered = T)

# Guardar los gráficos con calidad específica
width_in_inches <- 14
height_in_inches <- 6
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/likert_act_todo03.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)


#Grafico densidad recreativo no extractivo
plot(xlikgroup3, type = "density", centered = F)


# Guardar los gráficos con calidad específica


width_in_inches <- 14
height_in_inches <- 6
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/likert_act_dens03.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)



#5.Cambios ambientales####
##5.1. cargar datos y organizarlos----
head(all_sheets[["E_ambiental"]])
head(all_sheets[["A_perfil"]])

# Read the specific sheets into dataframes
e_ambiental <- read_excel(file_path, sheet = "E_ambiental")
a_perfil <- read_excel(file_path, sheet = "A_perfil")

# Select the relevant columns from A_perfil
a_perfil_selected <- a_perfil %>% select(ID, cofradia)

# Merge the dataframes on the ID column
merged_data_actividades <- e_ambiental %>%
  left_join(a_perfil_selected, by = "ID")

View(merged_data_actividades)

unique(merged_data_actividades$cofradia)

##5.2. Preparar los datos para el gráfico likert----
# Crear una nueva columna "zona" basada en los valores de 'cofradia'
merged_data_ambiental <- merged_data_actividades %>%
  mutate(zona = case_when(
    cofradia %in% c("Andratx","Soller", "Pollença") ~ "Mallorca North",
    cofradia %in% c("Palma", "Colonia de Sant Jordi", "Santanyi", "Portocolom" ) ~ "Mallorca South-West",
    cofradia %in% c("Alcudia","Cala Ratjada", "Porto Cristo") ~ "Mallorca South-East",
    cofradia %in% c("San Antoni de Portmany", "Eivissa", "Formentera") ~ "Pitiusas",
    cofradia %in% c("Ciutadella", "Fornells", "Mao") ~ "Menorca",
    TRUE ~ "otro" # Para valores que no están en los rangos especificados
  ))

# Filtrar las columnas relevantes
likert_data_ambiental <- merged_data_ambiental %>% 
  select(zona, Q19a_oaire:Q19k_estaciones)

# Función para renombrar los niveles de una columna
rename_levels <- function(column) {
  column <- as.character(column)
  column <- case_when(
    column == "disminuido" ~ "Decrease",
    column == "NS/NC" ~ "Don't Know",
    column == "igual" ~ "No Change",
    column == "cambio" ~ "Change",
    column == "aumentado" ~ "Increase",
    TRUE ~ column  # Caso por defecto para mantener el valor original si no hay coincidencia
  )
  factor(column, levels = common_levels)
}

# Definir los niveles comunes
common_levels <- c("Decrease", "Don't Know", "No Change", "Change", "Increase")

#Aplicar la función a todas las columnas de interés
likert_data_ambiental[ , -1] <- lapply(likert_data_ambiental[ , -1], rename_levels)

# Asegurar que todas las columnas tengan los mismos niveles
for (col in names(likert_data_ambiental)[-1]) {
  likert_data_ambiental[[col]] <- factor(likert_data_ambiental[[col]], levels = common_levels)
}

# Verificar los niveles de cada columna
lapply(likert_data_ambiental, levels)

# Convertir en dataframe
likert_data_ambiental <- as.data.frame(likert_data_ambiental)  


#cambiar los nombres de las columnas
print(colnames(likert_data_ambiental))



# Cambiar los nombres de las columnas
colnames(likert_data_ambiental) <- c(
  "isla_cofradia",
  "Air Heat-Waves",
  "Marine Heat-Waves",
  "Storm Freq.",
  "Storms Intensity",
  "Precipitation",
  "Sea Temperature",
  "Current Intensity",
  "Current Direction",
  "Wind Intensity",
  "Wind Direction",
  "Seasonality"
)

View(likert_data_ambiental)

## 5.3.Función para crear gráficos de Likert por isla CENTRADO----
create_likert_chart <- function(data, island_name, common_levels) {
  # Filtrar los datos por isla
  island_data <- data %>%
    filter(isla_cofradia == island_name)
  
  # Remover la columna 'isla_cofradia' para el análisis de Likert
  island_data <- island_data %>% select(-isla_cofradia)
  
  # Asegurarse de que todas las columnas tengan los mismos niveles
  island_data[] <- lapply(island_data, function(column) factor(column, levels = common_levels))
  
  # Crear el objeto de Likert
  likert_obj <- likert(island_data)
  
  # Crear el gráfico de Likert
  plot(likert_obj, plot.percent.low = FALSE, plot.percent.high = FALSE, group.order = c(
    "Air Heat-Waves",
    "Marine Heat-Waves",
    "Sea Temperature",
    "Precipitation",
    "Storm Freq.",
    "Storms Intensity",
    "Wind Intensity",
    "Wind Direction",
    "Current Intensity",
    "Current Direction",
    "Seasonality"
  )) + ggtitle(paste("Likert Chart for", island_name)) +
    theme_minimal(base_size = 15) + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 20, face = "bold"),
      legend.title = element_blank(),
      legend.position = "top",
      legend.text = element_text(size = 20),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
}


# Nombres de las islas
island_names <- unique(merged_data_ambiental$zona)

# Crear una lista para almacenar los gráficos
likert_plots <- list()

# Crear gráficos de Likert para cada isla y almacenarlos en la lista
for (island in island_names) {
  likert_plots[[island]] <- create_likert_chart(likert_data_ambiental, island, common_levels)
}


# Imprimir los gráficos
for (plot in likert_plots) {
  print(plot)
}

##5.4. Guardar los gráficos con calidad específica----
width_in_inches <- 10
height_in_inches <- 7
for (island in names(likert_plots)) {
  ggsave(paste0("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/likert_chart_amb", island, ".png"), plot = likert_plots[[island]], 
         dpi = 300, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)
}

## 5.5.Función para crear gráficos de Likert por isla-NO CENTRADO----
create_likert_chart <- function(data, island_name, common_levels) {
  # Filtrar los datos por isla
  island_data <- data %>%
    filter(isla_cofradia == island_name)
  
  # Remover la columna 'isla_cofradia' para el análisis de Likert
  island_data <- island_data %>% select(-isla_cofradia)
  
  # Asegurarse de que todas las columnas tengan los mismos niveles
  island_data[] <- lapply(island_data, function(column) factor(column, levels = common_levels))
  
  # Crear el objeto de Likert
  likert_obj <- likert(island_data)
  
  # Crear el gráfico de Likert
  plot(likert_obj, centered = FALSE, plot.percent.low = FALSE, plot.percent.high = FALSE, plot.percent.mid=FALSE, group.order = c(
    "Air Heat-Waves",
    "Marine Heat-Waves",
    "Sea Temperature",
    "Precipitation",
    "Storm Freq.",
    "Storms Intensity",
    "Wind Intensity",
    "Wind Direction",
    "Current Intensity",
    "Current Direction",
    "Seasonality"
  )) + ggtitle(paste("Likert Chart for", island_name)) +
    theme_minimal(base_size = 15) + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 20, face = "bold"),
      legend.title = element_blank(),
      legend.position = "top",
      legend.text = element_text(size = 20),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
}


# Nombres de las islas
island_names <- unique(merged_data_ambiental$zona)

# Crear una lista para almacenar los gráficos
likert_plots <- list()

# Crear gráficos de Likert para cada isla y almacenarlos en la lista
for (island in island_names) {
  likert_plots[[island]] <- create_likert_chart(likert_data_ambiental, island, common_levels)
}


# Imprimir los gráficos
for (plot in likert_plots) {
  print(plot)
}

##5.6. Guardar los gráficos con calidad específica----
width_in_inches <- 10
height_in_inches <- 7
for (island in names(likert_plots)) {
  ggsave(paste0("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/likert_chart_amb2", island, ".png"), plot = likert_plots[[island]], 
         dpi = 300, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)
}



##5.7. Crear Likert agrupado----
###5.7.1.Likert agrupado todo junto-CENTRADO

xlikgroup = likert(likert_data_ambiental[,2:12], grouping = likert_data_actividades$isla_cofradia)
xlikgroup

plot(xlikgroup, type = "bar", centered = T)#Se ordena por orden alfabético

###5.7.2 Guardar el gráfico con calidad específica----
width_in_inches <- 14
height_in_inches <- 25
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/likert_amb_todo.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)

###5.7.3.Likert agrupado todo junto-NO CENTRADO

xlikgroup = likert(likert_data_ambiental[,2:12], grouping = likert_data_actividades$isla_cofradia)
xlikgroup

plot(xlikgroup, type = "bar", centered = F)#Se ordena por orden alfabético

###5.7.4 Guardar el gráfico con calidad específica----
width_in_inches <- 14
height_in_inches <- 25
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/likert_amb_todo2.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)

###5.7.5.Gráfico densidad todo junto----
plot(xlikgroup, type = "density", centered = F)

xlikgroup

###5.7.6 Guardar el gráfico con calidad específica----
width_in_inches <- 14
height_in_inches <- 25
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/likert_amb_todo_dens.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)



likert_data_ambiental

##5.8 Gráfico calor por cambio individual entre zonas----

likert_data_ambiental <- na.omit(likert_data_ambiental)
likert_data_ambiental


# Definir los niveles comunes de la escala Likert
common_levels <- c("Decrease", "Don't Know", "No Change", "Change", "Increase")

###5.8.1.Air Heat-Waves----

# Asegurarnos de que la columna `Air Heat-Waves` tenga los niveles definidos
likert_data_ambiental$`Air Heat-Waves` <- factor(likert_data_ambiental$`Air Heat-Waves`, levels = common_levels)

# Calcular la frecuencia de respuestas por zona y nivel de Likert
heatmap_data <- likert_data_ambiental %>%
  group_by(isla_cofradia, `Air Heat-Waves`) %>%
  summarize(count = n(), .groups = "drop") %>%
  complete(isla_cofradia, `Air Heat-Waves`, fill = list(count = 0)) %>%  # Rellenar combinaciones faltantes
  group_by(isla_cofradia) %>%  # Agrupar por isla_cofradia
  mutate(percent = (count / sum(count)) * 100)  # Calcular porcentaje

# Crear el heatmap
ggplot(heatmap_data, aes(x = `Air Heat-Waves`, y = isla_cofradia, fill = percent)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkblue", 
                      limits = c(0, 100),  # Escala fija de 0 a 100
                      name = "Percent") +
  geom_text(aes(label = sprintf("%.1f%%", percent)), color = "black", size = 4) +  # Etiquetas de porcentaje
  labs(
    title = "Heatmap respuestas 'Air Heat-Waves'",
    x = "",
    y = ""
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 18),         # Tamaño del título
    axis.title.x = element_text(size = 14),      # Tamaño de la etiqueta del eje X
    axis.title.y = element_text(size = 14),      # Tamaño de la etiqueta del eje Y
    axis.text.x = element_text(size = 12),  # Tamaño y rotación del texto del eje X
    axis.text.y = element_text(size = 12),       # Tamaño del texto del eje Y
    legend.title = element_text(size = 14),      # Tamaño del título de la leyenda
    legend.text = element_text(size = 12)        # Tamaño del texto de la leyenda
  )

#Guardar el gráfico con calidad específica
width_in_inches <- 10
height_in_inches <- 2.5
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/heat_AHW.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)


###5.8.2.Marine Heat-Waves----

# Asegurarnos de que la columna `Air Heat-Waves` tenga los niveles definidos
likert_data_ambiental$`Marine Heat-Waves` <- factor(likert_data_ambiental$`Marine Heat-Waves`, levels = common_levels)

# Calcular la frecuencia de respuestas por zona y nivel de Likert
heatmap_data <- likert_data_ambiental %>%
  group_by(isla_cofradia, `Marine Heat-Waves`) %>%
  summarize(count = n(), .groups = "drop") %>%
  complete(isla_cofradia, `Marine Heat-Waves`, fill = list(count = 0)) %>%  # Rellenar combinaciones faltantes
  group_by(isla_cofradia) %>%  # Agrupar por isla_cofradia
  mutate(percent = (count / sum(count)) * 100)  # Calcular porcentaje

# Crear el heatmap
ggplot(heatmap_data, aes(x = `Marine Heat-Waves`, y = isla_cofradia, fill = percent)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkblue", 
                      limits = c(0, 100),  # Escala fija de 0 a 100
                      name = "Percent") +
  geom_text(aes(label = sprintf("%.1f%%", percent)), color = "black", size = 4) +  # Etiquetas de porcentaje
  labs(
    title = "Heatmap respuestas 'Marine Heat-Waves'",
    x = "",
    y = ""
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 18),         # Tamaño del título
    axis.title.x = element_text(size = 14),      # Tamaño de la etiqueta del eje X
    axis.title.y = element_text(size = 14),      # Tamaño de la etiqueta del eje Y
    axis.text.x = element_text(size = 12),  # Tamaño y rotación del texto del eje X
    axis.text.y = element_text(size = 12),       # Tamaño del texto del eje Y
    legend.title = element_text(size = 14),      # Tamaño del título de la leyenda
    legend.text = element_text(size = 12)        # Tamaño del texto de la leyenda
  )

#Guardar el gráfico con calidad específica
width_in_inches <- 10
height_in_inches <- 2.5
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/heat_MHW.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)

###5.8.3.Storm Freq.----

# Asegurarnos de que la columna `Air Heat-Waves` tenga los niveles definidos
likert_data_ambiental$`Storm Freq.` <- factor(likert_data_ambiental$`Storm Freq.`, levels = common_levels)

# Calcular la frecuencia de respuestas por zona y nivel de Likert
heatmap_data <- likert_data_ambiental %>%
  group_by(isla_cofradia, `Storm Freq.`) %>%
  summarize(count = n(), .groups = "drop") %>%
  complete(isla_cofradia, `Storm Freq.`, fill = list(count = 0)) %>%  # Rellenar combinaciones faltantes
  group_by(isla_cofradia) %>%  # Agrupar por isla_cofradia
  mutate(percent = (count / sum(count)) * 100)  # Calcular porcentaje

# Crear el heatmap
ggplot(heatmap_data, aes(x = `Storm Freq.`, y = isla_cofradia, fill = percent)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkblue", 
                      limits = c(0, 100),  # Escala fija de 0 a 100
                      name = "Percent") +
  geom_text(aes(label = sprintf("%.1f%%", percent)), color = "black", size = 4) +  # Etiquetas de porcentaje
  labs(
    title = "Heatmap respuestas 'Storm Freq.'",
    x = "",
    y = ""
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 18),         # Tamaño del título
    axis.title.x = element_text(size = 14),      # Tamaño de la etiqueta del eje X
    axis.title.y = element_text(size = 14),      # Tamaño de la etiqueta del eje Y
    axis.text.x = element_text(size = 12),  # Tamaño y rotación del texto del eje X
    axis.text.y = element_text(size = 12),       # Tamaño del texto del eje Y
    legend.title = element_text(size = 14),      # Tamaño del título de la leyenda
    legend.text = element_text(size = 12)        # Tamaño del texto de la leyenda
  )

#Guardar el gráfico con calidad específica
width_in_inches <- 10
height_in_inches <- 2.5
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/heat_SFQ.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)

###5.8.4.Storms Intensity----

# Asegurarnos de que la columna `Air Heat-Waves` tenga los niveles definidos
likert_data_ambiental$`Storms Intensity` <- factor(likert_data_ambiental$`Storms Intensity`, levels = common_levels)

# Calcular la frecuencia de respuestas por zona y nivel de Likert
heatmap_data <- likert_data_ambiental %>%
  group_by(isla_cofradia, `Storms Intensity`) %>%
  summarize(count = n(), .groups = "drop") %>%
  complete(isla_cofradia, `Storms Intensity`, fill = list(count = 0)) %>%  # Rellenar combinaciones faltantes
  group_by(isla_cofradia) %>%  # Agrupar por isla_cofradia
  mutate(percent = (count / sum(count)) * 100)  # Calcular porcentaje

# Crear el heatmap
ggplot(heatmap_data, aes(x = `Storms Intensity`, y = isla_cofradia, fill = percent)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkblue", 
                      limits = c(0, 100),  # Escala fija de 0 a 100
                      name = "Percent") +
  geom_text(aes(label = sprintf("%.1f%%", percent)), color = "black", size = 4) +  # Etiquetas de porcentaje
  labs(
    title = "Heatmap respuestas 'Storms Intensity'",
    x = "",
    y = ""
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 18),         # Tamaño del título
    axis.title.x = element_text(size = 14),      # Tamaño de la etiqueta del eje X
    axis.title.y = element_text(size = 14),      # Tamaño de la etiqueta del eje Y
    axis.text.x = element_text(size = 12),  # Tamaño y rotación del texto del eje X
    axis.text.y = element_text(size = 12),       # Tamaño del texto del eje Y
    legend.title = element_text(size = 14),      # Tamaño del título de la leyenda
    legend.text = element_text(size = 12)        # Tamaño del texto de la leyenda
  )

#Guardar el gráfico con calidad específica
width_in_inches <- 10
height_in_inches <- 2.5
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/heat_SIN.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)

###5.8.5.Precipitation----

# Asegurarnos de que la columna `Air Heat-Waves` tenga los niveles definidos
likert_data_ambiental$`Precipitation` <- factor(likert_data_ambiental$`Precipitation`, levels = common_levels)

# Calcular la frecuencia de respuestas por zona y nivel de Likert
heatmap_data <- likert_data_ambiental %>%
  group_by(isla_cofradia, `Precipitation`) %>%
  summarize(count = n(), .groups = "drop") %>%
  complete(isla_cofradia, `Precipitation`, fill = list(count = 0)) %>%  # Rellenar combinaciones faltantes
  group_by(isla_cofradia) %>%  # Agrupar por isla_cofradia
  mutate(percent = (count / sum(count)) * 100)  # Calcular porcentaje

# Crear el heatmap
ggplot(heatmap_data, aes(x = `Precipitation`, y = isla_cofradia, fill = percent)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkblue", 
                      limits = c(0, 100),  # Escala fija de 0 a 100
                      name = "Percent") +
  geom_text(aes(label = sprintf("%.1f%%", percent)), color = "black", size = 4) +  # Etiquetas de porcentaje
  labs(
    title = "Heatmap respuestas 'Precipitation'",
    x = "",
    y = ""
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 18),         # Tamaño del título
    axis.title.x = element_text(size = 14),      # Tamaño de la etiqueta del eje X
    axis.title.y = element_text(size = 14),      # Tamaño de la etiqueta del eje Y
    axis.text.x = element_text(size = 12),  # Tamaño y rotación del texto del eje X
    axis.text.y = element_text(size = 12),       # Tamaño del texto del eje Y
    legend.title = element_text(size = 14),      # Tamaño del título de la leyenda
    legend.text = element_text(size = 12)        # Tamaño del texto de la leyenda
  )

#Guardar el gráfico con calidad específica
width_in_inches <- 10
height_in_inches <- 2.5
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/heat_PREC.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)

###5.8.6.Sea Temperature----

# Asegurarnos de que la columna `Air Heat-Waves` tenga los niveles definidos
likert_data_ambiental$`Sea Temperature` <- factor(likert_data_ambiental$`Sea Temperature`, levels = common_levels)

# Calcular la frecuencia de respuestas por zona y nivel de Likert
heatmap_data <- likert_data_ambiental %>%
  group_by(isla_cofradia, `Sea Temperature`) %>%
  summarize(count = n(), .groups = "drop") %>%
  complete(isla_cofradia, `Sea Temperature`, fill = list(count = 0)) %>%  # Rellenar combinaciones faltantes
  group_by(isla_cofradia) %>%  # Agrupar por isla_cofradia
  mutate(percent = (count / sum(count)) * 100)  # Calcular porcentaje

# Crear el heatmap
ggplot(heatmap_data, aes(x = `Sea Temperature`, y = isla_cofradia, fill = percent)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkblue", 
                      limits = c(0, 100),  # Escala fija de 0 a 100
                      name = "Percent") +
  geom_text(aes(label = sprintf("%.1f%%", percent)), color = "black", size = 4) +  # Etiquetas de porcentaje
  labs(
    title = "Heatmap respuestas 'Sea Temperature'",
    x = "",
    y = ""
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 18),         # Tamaño del título
    axis.title.x = element_text(size = 14),      # Tamaño de la etiqueta del eje X
    axis.title.y = element_text(size = 14),      # Tamaño de la etiqueta del eje Y
    axis.text.x = element_text(size = 12),  # Tamaño y rotación del texto del eje X
    axis.text.y = element_text(size = 12),       # Tamaño del texto del eje Y
    legend.title = element_text(size = 14),      # Tamaño del título de la leyenda
    legend.text = element_text(size = 12)        # Tamaño del texto de la leyenda
  )

#Guardar el gráfico con calidad específica
width_in_inches <- 10
height_in_inches <- 2.5
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/heat_STEMP.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)

###5.8.7.Current Intensity----

# Asegurarnos de que la columna `Air Heat-Waves` tenga los niveles definidos
likert_data_ambiental$`Current Intensity` <- factor(likert_data_ambiental$`Current Intensity`, levels = common_levels)

# Calcular la frecuencia de respuestas por zona y nivel de Likert
heatmap_data <- likert_data_ambiental %>%
  group_by(isla_cofradia, `Current Intensity`) %>%
  summarize(count = n(), .groups = "drop") %>%
  complete(isla_cofradia, `Current Intensity`, fill = list(count = 0)) %>%  # Rellenar combinaciones faltantes
  group_by(isla_cofradia) %>%  # Agrupar por isla_cofradia
  mutate(percent = (count / sum(count)) * 100)  # Calcular porcentaje

# Crear el heatmap
ggplot(heatmap_data, aes(x = `Current Intensity`, y = isla_cofradia, fill = percent)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkblue", 
                      limits = c(0, 100),  # Escala fija de 0 a 100
                      name = "Percent") +
  geom_text(aes(label = sprintf("%.1f%%", percent)), color = "black", size = 4) +  # Etiquetas de porcentaje
  labs(
    title = "Heatmap respuestas 'Current Intensity'",
    x = "",
    y = ""
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 18),         # Tamaño del título
    axis.title.x = element_text(size = 14),      # Tamaño de la etiqueta del eje X
    axis.title.y = element_text(size = 14),      # Tamaño de la etiqueta del eje Y
    axis.text.x = element_text(size = 12),  # Tamaño y rotación del texto del eje X
    axis.text.y = element_text(size = 12),       # Tamaño del texto del eje Y
    legend.title = element_text(size = 14),      # Tamaño del título de la leyenda
    legend.text = element_text(size = 12)        # Tamaño del texto de la leyenda
  )

#Guardar el gráfico con calidad específica
width_in_inches <- 10
height_in_inches <- 2.5
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/heat_CINT.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)

###5.8.8.Current Direction----

# Asegurarnos de que la columna `Air Heat-Waves` tenga los niveles definidos
likert_data_ambiental$`Current Direction` <- factor(likert_data_ambiental$`Current Direction`, levels = common_levels)

# Calcular la frecuencia de respuestas por zona y nivel de Likert
heatmap_data <- likert_data_ambiental %>%
  group_by(isla_cofradia, `Current Direction`) %>%
  summarize(count = n(), .groups = "drop") %>%
  complete(isla_cofradia, `Current Direction`, fill = list(count = 0)) %>%  # Rellenar combinaciones faltantes
  group_by(isla_cofradia) %>%  # Agrupar por isla_cofradia
  mutate(percent = (count / sum(count)) * 100)  # Calcular porcentaje

# Crear el heatmap
ggplot(heatmap_data, aes(x = `Current Direction`, y = isla_cofradia, fill = percent)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkblue", 
                      limits = c(0, 100),  # Escala fija de 0 a 100
                      name = "Percent") +
  geom_text(aes(label = sprintf("%.1f%%", percent)), color = "black", size = 4) +  # Etiquetas de porcentaje
  labs(
    title = "Heatmap respuestas 'Current Direction'",
    x = "",
    y = ""
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 18),         # Tamaño del título
    axis.title.x = element_text(size = 14),      # Tamaño de la etiqueta del eje X
    axis.title.y = element_text(size = 14),      # Tamaño de la etiqueta del eje Y
    axis.text.x = element_text(size = 12),  # Tamaño y rotación del texto del eje X
    axis.text.y = element_text(size = 12),       # Tamaño del texto del eje Y
    legend.title = element_text(size = 14),      # Tamaño del título de la leyenda
    legend.text = element_text(size = 12)        # Tamaño del texto de la leyenda
  )

#Guardar el gráfico con calidad específica
width_in_inches <- 10
height_in_inches <- 2.5
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/heat_CDIR.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)


###5.8.9.Wind Intensity----

# Asegurarnos de que la columna `Air Heat-Waves` tenga los niveles definidos
likert_data_ambiental$`Wind Intensity` <- factor(likert_data_ambiental$`Wind Intensity`, levels = common_levels)

# Calcular la frecuencia de respuestas por zona y nivel de Likert
heatmap_data <- likert_data_ambiental %>%
  group_by(isla_cofradia, `Wind Intensity`) %>%
  summarize(count = n(), .groups = "drop") %>%
  complete(isla_cofradia, `Wind Intensity`, fill = list(count = 0)) %>%  # Rellenar combinaciones faltantes
  group_by(isla_cofradia) %>%  # Agrupar por isla_cofradia
  mutate(percent = (count / sum(count)) * 100)  # Calcular porcentaje

# Crear el heatmap
ggplot(heatmap_data, aes(x = `Wind Intensity`, y = isla_cofradia, fill = percent)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkblue", 
                      limits = c(0, 100),  # Escala fija de 0 a 100
                      name = "Percent") +
  geom_text(aes(label = sprintf("%.1f%%", percent)), color = "black", size = 4) +  # Etiquetas de porcentaje
  labs(
    title = "Heatmap respuestas 'Wind Intensity'",
    x = "",
    y = ""
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 18),         # Tamaño del título
    axis.title.x = element_text(size = 14),      # Tamaño de la etiqueta del eje X
    axis.title.y = element_text(size = 14),      # Tamaño de la etiqueta del eje Y
    axis.text.x = element_text(size = 12),  # Tamaño y rotación del texto del eje X
    axis.text.y = element_text(size = 12),       # Tamaño del texto del eje Y
    legend.title = element_text(size = 14),      # Tamaño del título de la leyenda
    legend.text = element_text(size = 12)        # Tamaño del texto de la leyenda
  )

#Guardar el gráfico con calidad específica
width_in_inches <- 10
height_in_inches <- 2.5
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/heat_WINT.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)


###5.8.10.Wind Direction----

# Asegurarnos de que la columna `Air Heat-Waves` tenga los niveles definidos
likert_data_ambiental$`Wind Direction` <- factor(likert_data_ambiental$`Wind Direction`, levels = common_levels)

# Calcular la frecuencia de respuestas por zona y nivel de Likert
heatmap_data <- likert_data_ambiental %>%
  group_by(isla_cofradia, `Wind Direction`) %>%
  summarize(count = n(), .groups = "drop") %>%
  complete(isla_cofradia, `Wind Direction`, fill = list(count = 0)) %>%  # Rellenar combinaciones faltantes
  group_by(isla_cofradia) %>%  # Agrupar por isla_cofradia
  mutate(percent = (count / sum(count)) * 100)  # Calcular porcentaje

# Crear el heatmap
ggplot(heatmap_data, aes(x = `Wind Direction`, y = isla_cofradia, fill = percent)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkblue", 
                      limits = c(0, 100),  # Escala fija de 0 a 100
                      name = "Percent") +
  geom_text(aes(label = sprintf("%.1f%%", percent)), color = "black", size = 4) +  # Etiquetas de porcentaje
  labs(
    title = "Heatmap respuestas 'Wind Direction'",
    x = "",
    y = ""
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 18),         # Tamaño del título
    axis.title.x = element_text(size = 14),      # Tamaño de la etiqueta del eje X
    axis.title.y = element_text(size = 14),      # Tamaño de la etiqueta del eje Y
    axis.text.x = element_text(size = 12),  # Tamaño y rotación del texto del eje X
    axis.text.y = element_text(size = 12),       # Tamaño del texto del eje Y
    legend.title = element_text(size = 14),      # Tamaño del título de la leyenda
    legend.text = element_text(size = 12)        # Tamaño del texto de la leyenda
  )

#Guardar el gráfico con calidad específica
width_in_inches <- 10
height_in_inches <- 2.5
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/heat_WDIR.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)

###5.8.11.Seasonality----

# Asegurarnos de que la columna `Air Heat-Waves` tenga los niveles definidos
likert_data_ambiental$`Seasonality` <- factor(likert_data_ambiental$`Seasonality`, levels = common_levels)

# Calcular la frecuencia de respuestas por zona y nivel de Likert
heatmap_data <- likert_data_ambiental %>%
  group_by(isla_cofradia, `Seasonality`) %>%
  summarize(count = n(), .groups = "drop") %>%
  complete(isla_cofradia, `Seasonality`, fill = list(count = 0)) %>%  # Rellenar combinaciones faltantes
  group_by(isla_cofradia) %>%  # Agrupar por isla_cofradia
  mutate(percent = (count / sum(count)) * 100)  # Calcular porcentaje

# Crear el heatmap
ggplot(heatmap_data, aes(x = `Seasonality`, y = isla_cofradia, fill = percent)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkblue", 
                      limits = c(0, 100),  # Escala fija de 0 a 100
                      name = "Percent") +
  geom_text(aes(label = sprintf("%.1f%%", percent)), color = "black", size = 4) +  # Etiquetas de porcentaje
  labs(
    title = "Heatmap respuestas 'Seasonality'",
    x = "",
    y = ""
  )+
  theme_bw()+
  theme(
    plot.title = element_text(size = 18),         # Tamaño del título
    axis.title.x = element_text(size = 14),      # Tamaño de la etiqueta del eje X
    axis.title.y = element_text(size = 14),      # Tamaño de la etiqueta del eje Y
    axis.text.x = element_text(size = 12),  # Tamaño y rotación del texto del eje X
    axis.text.y = element_text(size = 12),       # Tamaño del texto del eje Y
    legend.title = element_text(size = 14),      # Tamaño del título de la leyenda
    legend.text = element_text(size = 12)        # Tamaño del texto de la leyenda
  )

#Guardar el gráfico con calidad específica
width_in_inches <- 10
height_in_inches <- 2.5
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/heat_SEAS.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)

#6. Cambios en las especies####
##6.1. cargar datos y organizarlos----
head(all_sheets[["C_especies"]])
head(all_sheets[["A_perfil"]])

# Read the specific sheets into dataframes
e_especies <- read_excel(file_path, sheet = "C_especies")
a_perfil <- read_excel(file_path, sheet = "A_perfil")

# Select the relevant columns from A_perfil
a_perfil_selected <- a_perfil %>% select(ID, cofradia)

# Merge the dataframes on the ID column
merged_data_especies <- e_especies %>%
  left_join(a_perfil_selected, by = "ID")

View(merged_data_especies)

unique(merged_data_especies$sp_cien)

# Crear una nueva columna "zona" basada en los valores de 'cofradia'
merged_data_especies <- merged_data_especies %>%
  mutate(zona = case_when(
    cofradia %in% c("Andratx","Soller", "Pollença") ~ "Mallorca North",
    cofradia %in% c("Palma", "Colonia de Sant Jordi", "Santanyi", "Portocolom" ) ~ "Mallorca South-West",
    cofradia %in% c("Alcudia","Cala Ratjada", "Porto Cristo") ~ "Mallorca South-East",
    cofradia %in% c("San Antoni de Portmany", "Eivissa", "Formentera") ~ "Pitiusas",
    cofradia %in% c("Ciutadella", "Fornells", "Mao") ~ "Menorca",
    TRUE ~ "otro" # Para valores que no están en los rangos especificados
  ))

# Eliminar NAs en sp_cien y abundancia 
filtered_especies <- merged_data_especies %>%
  filter(sp_cien != "NA")

filtered_especies <- filtered_especies %>%
  filter(abundancia != "NA")

# Obtener las especies únicas
especies_unicas <- unique(filtered_especies$sp_cien)
lista_especies <- paste0("c(", paste(shQuote(especies_unicas), collapse = ", "), ")")
cat(lista_especies)
lista_especies <- c("Spicara smaris", "Mullus surmuletus", "Trachurus ssp.", "Octopus vulgaris", "Dentex dentex", "Dasyatis pastinaca", "Scyliorhinus canicula", "Epinephelus marginatus", "Scorpaena scrofa", "Pagrus pagrus", "Palinurus elephas", "Rostroraja alba", "Gymnura altavela", "Cnidaria spp.", "Sepia officinalis", "Coryphaena hippurus", "Zeus faber", "Aristeus antennatus", "Nephrops norvegicus", "Scorpaena porcus", "Raja spp.", "Lophius spp.", "Labrus spp.", "Pagellus bogaraveo", "Chlorophyta sp.", "Balistes capriscus", "Loligo vulgaris", "Ostrea edulis", "Arca noae", "Lithophaga lithophaga", "Aphia minuta", "Pinna nobilis", "Bivalvia spp.", "Mollusca spp.", "Diplodus sargus", "Sparus aurata", "Maja squinado", "Melicertus kerathurus", "Uranoscopus scaber", "Sciaena umbra", "Amphipods & Isopods", "Thunnus thynnus", "Seriola dumerili", "Scyllarides latus", "Tursiops truncatus", "Scomber scombrus", "Raja clavata", "Sphyraena spp.", "Salpidae spp.", "Serranus scriba", "Sarda sarda", "Engraulis encrasicolus", "Epinephelus costae", "Trachinus ssp.", "Bothus podas", "Xyrichtys novacula", "Phycis spp.", "Belone belone", "Squatina squatina", "Argyrosomus regius", "Auxis rochei", "Euthynnus alletteratus", "Thunnus alalunga", "Scyliorhinus stellaris", "Mustelus mustelus", "Spondyliosoma cantharus", "Laminaria rodriguezii", "Spicara maena", "Pelagia noctiluca", "Muraena helena", "Dactylopterus volitans ", "Naucrates ductor", "Polyprion americanus", "Chelonioidea spp.", "Chelidonichthys lucerna", "Brachyura spp.", "Ensis magnus", "Donax trunculus")




##6.2.Gráficos abundancias----
###6.2.1.Preparar datos

# Convertir la columna de abundancia a factor para mantener el orden deseado y cambiar los niveles a inglés
filtered_especies$abundancia <- factor(filtered_especies$abundancia, levels = c("menor", "igual", "mayor"),
                                    labels = c("Decrease", "No Change", "Increase"))

# Definir colores personalizados
colors_fill <- c("Decrease" = "#F45C1C", 
                 "No Change" = "grey", 
                 "Increase" = "#33BDB7")


# Crear un dataframe con todas las especies de la lista y valores 0 por defecto
df_completo <- data.frame(
  sp_cien = lista_especies,
  Decrease = 0,
  Increase = 0,
)

###6.2.2. Crear gráfico por zona----
####6.2.2.1.Pitiusas----
# Filtrar los datos para la zona específica
pitiusas_data_ab <- filtered_especies %>% filter(zona == "Pitiusas")

# Crear una tabla resumen para obtener el conteo de cada combinación
data_summary_pitiusas <- pitiusas_data_ab %>%
  count(sp_cien, abundancia) %>%
  pivot_wider(names_from = abundancia, values_from = n, values_fill = list(n = 0))


# Combinar con el dataframe original
df_completo_pitiusas <- merge(df_completo, data_summary_pitiusas, by = "sp_cien", all.x = TRUE)
df_completo_pitiusas <- df_completo_pitiusas[, -c(2:4)]
colnames(df_completo_pitiusas) <- c("sp_cien", "Decrease", "Increase")

df_completo_pitiusas$Decrease[is.na(df_completo_pitiusas$Decrease)] <- 0
df_completo_pitiusas$Increase[is.na(df_completo_pitiusas$Increase)] <- 0

# Crear un nuevo dataframe con las proporciones
df_proporcion_pitiusas <- df_completo_pitiusas

# Dividir todas las columnas numéricas por 17
df_proporcion_pitiusas[, -1] <- (df_proporcion_pitiusas[, -1] / 17) * 100


# Convertir la tabla resumen a formato largo para ggplot
data_long <- df_proporcion_pitiusas %>%
  pivot_longer(cols = -c(sp_cien), names_to = "abundancia", values_to = "count")

data_long$count <- ifelse(data_long$abundancia == "Decrease", -abs(data_long$count), abs(data_long$count))
data_long$sp_cien <- factor(data_long$sp_cien, levels = rev(unique(data_long$sp_cien)))

# Asegurar que los niveles de abundancia se respeten en el orden deseado
data_long$abundancia <- factor(data_long$abundancia, levels = c("Decrease", "Increase"))
data_long$sp_cien <- factor(data_long$sp_cien, levels = sort(unique(data_long$sp_cien)))
ggplot(data_long, aes(y = sp_cien, x = count, fill = abundancia)) +
  geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
  labs(
    title = "Cambios en las especies - Pitiusas",
    subtitle = "Porcentaje de respuestas por tipo de cambio",
    x = "",
    y = "Especie",
    fill = ""
  ) +
  scale_fill_manual(
    values = colors_fill, 
    breaks = c("Decrease", "Increase"))+
    scale_x_continuous(labels = percent_format(scale = 1),
                       limits = c(-60, 60)) + # Fijar límites de -60 a 60) 
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title.y = element_text(face = "italic", size = 24),
    axis.title.x = element_text(size = 20),
    axis.text.y = element_text(face = "italic"),
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  geom_vline(xintercept = 0.0, linetype = "dashed", color = "gray50")

###Guardar el gráfico con calidad específica
width_in_inches <- 10
height_in_inches <- 25
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/sp_pitiusas.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)

####6.2.2.2.Menorca----
# Filtrar los datos para la zona específica
menorca_data_ab <- filtered_especies %>% filter(zona == "Menorca")

# Crear una tabla resumen para obtener el conteo de cada combinación
data_summary_menorca <- menorca_data_ab %>%
  count(sp_cien, abundancia) %>%
  pivot_wider(names_from = abundancia, values_from = n, values_fill = list(n = 0))


# Crear un dataframe con todas las especies de la lista y valores 0 por defecto
df_completo <- data.frame(
  sp_cien = lista_especies,
  Decrease = 0,
  Increase = 0,
)

# Combinar con el dataframe original
df_completo_menorca <- merge(df_completo, data_summary_menorca, by = "sp_cien", all.x = TRUE)
df_completo_menorca <- df_completo_menorca[, -c(2:4)]
colnames(df_completo_menorca) <- c("sp_cien", "Increase", "Decrease")

df_completo_menorca$Decrease[is.na(df_completo_menorca$Decrease)] <- 0
df_completo_menorca$Increase[is.na(df_completo_menorca$Increase)] <- 0

# Crear un nuevo dataframe con las proporciones
df_proporcion_menorca <- df_completo_menorca

# Dividir todas las columnas numéricas por 16
df_proporcion_menorca[, -1] <- (df_proporcion_menorca[, -1] / 16) * 100


# Convertir la tabla resumen a formato largo para ggplot
data_long <- df_proporcion_menorca %>%
  pivot_longer(cols = -c(sp_cien), names_to = "abundancia", values_to = "count")

data_long$count <- ifelse(data_long$abundancia == "Decrease", -abs(data_long$count), abs(data_long$count))
data_long$sp_cien <- factor(data_long$sp_cien, levels = rev(unique(data_long$sp_cien)))

# Asegurar que los niveles de abundancia se respeten en el orden deseado
data_long$abundancia <- factor(data_long$abundancia, levels = c("Decrease", "Increase"))
data_long$sp_cien <- factor(data_long$sp_cien, levels = sort(unique(data_long$sp_cien)))
ggplot(data_long, aes(y = sp_cien, x = count, fill = abundancia)) +
  geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
  labs(
    title = "Cambios en las especies - Menorca",
    subtitle = "Porcentaje de respuestas por tipo de cambio",
    x = "",
    y = "Especie",
    fill = ""
  ) +
  scale_fill_manual(
    values = colors_fill, 
    breaks = c("Decrease", "Increase"))+
  scale_x_continuous(labels = percent_format(scale = 1),
                     limits = c(-60, 60)) + # Fijar límites de -60 a 60) 
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title.y = element_text(face = "italic", size = 24),
    axis.title.x = element_text(size = 20),
    axis.text.y = element_text(face = "italic"),
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  geom_vline(xintercept = 0.0, linetype = "dashed", color = "gray50")

###Guardar el gráfico con calidad específica
width_in_inches <- 10
height_in_inches <- 25
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/sp_menorca.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)


#Revisar especies con contradicciones
menorca_data_dentex <- menorca_data_ab %>% filter(sp_cien == "Dentex dentex")
menorca_data_scorpaena <- menorca_data_ab %>% filter(sp_cien == "Scorpaena scrofa")
menorca_data_spondylosoma <- menorca_data_ab %>% filter(sp_cien == "Spondyliosoma cantharus")

####6.2.2.3.Mallorca North----

# Filtrar los datos para la zona específica
mnorth_data_ab <- filtered_especies %>% filter(zona == "Mallorca North")

# Crear una tabla resumen para obtener el conteo de cada combinación
data_summary_mnorth <- mnorth_data_ab %>%
  count(sp_cien, abundancia) %>%
  pivot_wider(names_from = abundancia, values_from = n, values_fill = list(n = 0))


# Combinar con el dataframe original
df_completo_mnorth <- merge(df_completo, data_summary_mnorth, by = "sp_cien", all.x = TRUE)
df_completo_mnorth <- df_completo_mnorth[, -c(2:4)]
colnames(df_completo_mnorth) <- c("sp_cien", "Increase", "Decrease")

df_completo_mnorth$Decrease[is.na(df_completo_mnorth$Decrease)] <- 0
df_completo_mnorth$Increase[is.na(df_completo_mnorth$Increase)] <- 0

# Crear un nuevo dataframe con las proporciones
df_proporcion_mnorth <- df_completo_mnorth

# Dividir todas las columnas numéricas por 16
df_proporcion_mnorth[, -1] <- (df_proporcion_mnorth[, -1] / 11) * 100


# Convertir la tabla resumen a formato largo para ggplot
data_long <- df_proporcion_mnorth %>%
  pivot_longer(cols = -c(sp_cien), names_to = "abundancia", values_to = "count")

data_long$count <- ifelse(data_long$abundancia == "Decrease", -abs(data_long$count), abs(data_long$count))
data_long$sp_cien <- factor(data_long$sp_cien, levels = rev(unique(data_long$sp_cien)))

# Asegurar que los niveles de abundancia se respeten en el orden deseado
data_long$abundancia <- factor(data_long$abundancia, levels = c("Decrease", "Increase"))
data_long$sp_cien <- factor(data_long$sp_cien, levels = sort(unique(data_long$sp_cien)))
ggplot(data_long, aes(y = sp_cien, x = count, fill = abundancia)) +
  geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
  labs(
    title = "Cambios en las especies - Mallorca North",
    subtitle = "Porcentaje de respuestas por tipo de cambio",
    x = "",
    y = "Especie",
    fill = ""
  ) +
  scale_fill_manual(
    values = colors_fill, 
    breaks = c("Decrease", "Increase"))+
  scale_x_continuous(labels = percent_format(scale = 1),
                     limits = c(-60, 60)) + # Fijar límites de -60 a 60) 
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title.y = element_text(face = "italic", size = 24),
    axis.title.x = element_text(size = 20),
    axis.text.y = element_text(face = "italic"),
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  geom_vline(xintercept = 0.0, linetype = "dashed", color = "gray50")

###Guardar el gráfico con calidad específica
width_in_inches <- 10
height_in_inches <- 25
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/sp_mnorth.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)


#Revisar especies con contradicciones

mnorth_data_aphia <- mnorth_data_ab %>% filter(sp_cien == "Aphia minuta")
mnorth_data_coryphaena <- mnorth_data_ab %>% filter(sp_cien == "Coryphaena hippurus")
mnorth_data_dentex <- mnorth_data_ab %>% filter(sp_cien == "Dentex dentex")
mnorth_data_epinephelus <- mnorth_data_ab %>% filter(sp_cien == "Epinephelus marginatus")
mnorth_data_mullus <- mnorth_data_ab %>% filter(sp_cien == "Mullus surmuletus")
mnorth_data_scorpaena <- mnorth_data_ab %>% filter(sp_cien == "Scorpaena scrofa")
mnorth_data_sepia <- mnorth_data_ab %>% filter(sp_cien == "Sepia officinalis")

####6.2.2.4.Mallorca South-West----

# Filtrar los datos para la zona específica
meast_data_ab <- filtered_especies %>% filter(zona == "Mallorca South-West")

# Crear una tabla resumen para obtener el conteo de cada combinación
data_summary_meast <- meast_data_ab %>%
  count(sp_cien, abundancia) %>%
  pivot_wider(names_from = abundancia, values_from = n, values_fill = list(n = 0))
data_summary_meast <- data_summary_meast [, -c(4)]


# Combinar con el dataframe original
df_completo_meast <- merge(df_completo, data_summary_meast, by = "sp_cien", all.x = TRUE)
df_completo_meast <- df_completo_meast[, -c(2:4)]
colnames(df_completo_meast) <- c("sp_cien",  "Decrease", "Increase")

df_completo_meast$Decrease[is.na(df_completo_meast$Decrease)] <- 0
df_completo_meast$Increase[is.na(df_completo_meast$Increase)] <- 0

# Crear un nuevo dataframe con las proporciones
df_proporcion_meast <- df_completo_meast

# Dividir todas las columnas numéricas por 16
df_proporcion_meast[, -1] <- (df_proporcion_meast[, -1] / 17) * 100


# Convertir la tabla resumen a formato largo para ggplot
data_long <- df_proporcion_meast %>%
  pivot_longer(cols = -c(sp_cien), names_to = "abundancia", values_to = "count")

data_long$count <- ifelse(data_long$abundancia == "Decrease", -abs(data_long$count), abs(data_long$count))
data_long$sp_cien <- factor(data_long$sp_cien, levels = rev(unique(data_long$sp_cien)))

# Asegurar que los niveles de abundancia se respeten en el orden deseado
data_long$abundancia <- factor(data_long$abundancia, levels = c("Decrease", "Increase"))
data_long$sp_cien <- factor(data_long$sp_cien, levels = sort(unique(data_long$sp_cien)))
ggplot(data_long, aes(y = sp_cien, x = count, fill = abundancia)) +
  geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
  labs(
    title = "Cambios en las especies - Mallorca South-West",
    subtitle = "Porcentaje de respuestas por tipo de cambio",
    x = "",
    y = "Especie",
    fill = ""
  ) +
  scale_fill_manual(
    values = colors_fill, 
    breaks = c("Decrease", "Increase"))+
  scale_x_continuous(labels = percent_format(scale = 1),
                     limits = c(-60, 60)) + # Fijar límites de -60 a 60) 
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title.y = element_text(face = "italic", size = 24),
    axis.title.x = element_text(size = 20),
    axis.text.y = element_text(face = "italic"),
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  geom_vline(xintercept = 0.0, linetype = "dashed", color = "gray50")

###Guardar el gráfico con calidad específica
width_in_inches <- 10
height_in_inches <- 25
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/sp_mswest.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)


#Revisar especies con contradicciones
meast_data_epinephelus <- meast_data_ab %>% filter(sp_cien == "Epinephelus marginatus")
meast_data_mullus <- meast_data_ab %>% filter(sp_cien == "Mullus surmuletus")
meast_data_palinurus <- meast_data_ab %>% filter(sp_cien == "Palinurus elephas")

####6.2.2.4.Mallorca South-East----

# Filtrar los datos para la zona específica
meast_data_ab <- filtered_especies %>% filter(zona == "Mallorca South-East")

# Crear una tabla resumen para obtener el conteo de cada combinación
data_summary_meast <- meast_data_ab %>%
  count(sp_cien, abundancia) %>%
  pivot_wider(names_from = abundancia, values_from = n, values_fill = list(n = 0))
data_summary_meast <- data_summary_meast [, -c(4)]


# Combinar con el dataframe original
df_completo_meast <- merge(df_completo, data_summary_meast, by = "sp_cien", all.x = TRUE)
df_completo_meast <- df_completo_meast[, -c(2:4)]
colnames(df_completo_meast) <- c("sp_cien",  "Decrease", "Increase")

df_completo_meast$Decrease[is.na(df_completo_meast$Decrease)] <- 0
df_completo_meast$Increase[is.na(df_completo_meast$Increase)] <- 0

# Crear un nuevo dataframe con las proporciones
df_proporcion_meast <- df_completo_meast

# Dividir todas las columnas numéricas por 16
df_proporcion_meast[, -1] <- (df_proporcion_meast[, -1] / 14) * 100


# Convertir la tabla resumen a formato largo para ggplot
data_long <- df_proporcion_meast %>%
  pivot_longer(cols = -c(sp_cien), names_to = "abundancia", values_to = "count")

data_long$count <- ifelse(data_long$abundancia == "Decrease", -abs(data_long$count), abs(data_long$count))
data_long$sp_cien <- factor(data_long$sp_cien, levels = rev(unique(data_long$sp_cien)))

# Asegurar que los niveles de abundancia se respeten en el orden deseado
data_long$abundancia <- factor(data_long$abundancia, levels = c("Decrease", "Increase"))
data_long$sp_cien <- factor(data_long$sp_cien, levels = sort(unique(data_long$sp_cien)))
ggplot(data_long, aes(y = sp_cien, x = count, fill = abundancia)) +
  geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
  labs(
    title = "Cambios en las especies - Mallorca South-East",
    subtitle = "Porcentaje de respuestas por tipo de cambio",
    x = "",
    y = "Especie",
    fill = ""
  ) +
  scale_fill_manual(
    values = colors_fill, 
    breaks = c("Decrease", "Increase"))+
  scale_x_continuous(labels = percent_format(scale = 1),
                     limits = c(-60, 60)) + # Fijar límites de -60 a 60) 
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title.y = element_text(face = "italic", size = 24),
    axis.title.x = element_text(size = 20),
    axis.text.y = element_text(face = "italic"),
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  geom_vline(xintercept = 0.0, linetype = "dashed", color = "gray50")

###Guardar el gráfico con calidad específica
width_in_inches <- 10
height_in_inches <- 25
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/sp_meast.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)


#Revisar especies con contradicciones
meast_data_aphia <- meast_data_ab %>% filter(sp_cien == "Aphia minuta")
meast_data_mullus <- meast_data_ab %>% filter(sp_cien == "Mullus surmuletus")
meast_data_palinurus <- meast_data_ab %>% filter(sp_cien == "Palinurus elephas")

