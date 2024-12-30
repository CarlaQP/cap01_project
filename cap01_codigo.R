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
file_path <- paste0(ruta_base, "BIOTRANS_dataset2.0_v1_0924.xlsx")

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


# Guardar los gráficos con calidad específica----
width_in_inches <- 14
height_in_inches <- 6
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/likert_act_dens03.png", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)


