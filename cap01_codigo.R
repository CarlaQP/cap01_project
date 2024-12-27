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

#2. Mapa####

#2.1. Elige la hojas y trasnformalo en dataframe----
head(all_sheets[["00_entrevistas"]])

df_mapa <- read_excel(file_path, sheet = "00_entrevistas")

#2.2. Definir límits pel mapa (bb)----
lon_min <- 1.00 # xmin
lon_max <- 4.50 # xmax
lat_min <- 38.50 # ymin
lat_max <- 40.50 # ymax

bb <- c(lon_min, lat_min, lon_max, lat_max)

#2.3. Obtenir d'Open Street Map (osm) la línea de costa, acotada als límits de "bb"
osm_data_coastline <- opq(bb = bb) %>%
  add_osm_feature(key = 'natural', value = 'coastline') %>%
  osmdata_sf()

#2.4. Obtenir els polígons per la terra (land) i mar (sea) a partir de les línees d'osm----
# OSM només són objectes lineals, no poligonals. Per obtenir polígons hem de fer el pas 4): A partir de les linees de costa (4.1), retallem un rectangle creat, que ocupa la zona a mapejar (4.2), per obtenir els polígons per les zones de terra (4.3 i 4.4) 

## 2.4.1. Obtenir d'Open Street Map (osm) la línea de costa, acotada als límits de "bb"----
# Aquest punt és igual que "3)", però allà hem generat un objecte (osm_data_coastline) que utilitzarem en sí mateix per fer el mapa (la línea de costa) i ara generem un altre objecte (coast) que farem servir per retallar els polígons que volem. Podria haver fet servir el matiex pels dos casos, però ho separo perquè quedi més clar el pas
coast <- opq(bb) %>%
  add_osm_feature(key = "natural", value = "coastline") %>%
  osmdata_sf()

## 2.4.2. Crear el rectangle (poligon) dins dels limits de "bb"----
bb_rect <- data.frame(
  lat = c(lat_min, lat_max),
  lon = c(lon_min, lon_max)
) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

## 2.4.3. Retallar dins del rectangle, en base a la línea de costa (coast) de 4.1----
bb_rect_split <- bb_rect %>% 
  st_split(coast$osm_lines) %>% 
  st_collection_extract("POLYGON")

## 2.4.4. Extreure del polígons retallats a 4.3, els que són terra (land)---- 
# En aquest cas, de la llista, 1 era mar i del 2 al 10 polígons de terra. No sempre serà fins a 10, depenent del mapa tindràs més polígons que formen trossos de terra. I no sé si el mar sempre serà l'1 (suposo que sí, pero si hi ha barreres naturals que divideixi la massa d'aigua, per exemple, pot ser que sigui més d'un polígon).
land <- bb_rect_split[2:10]

## 2.4.5. Mapa ràpid (provisional) per comprovar que "land" són els poligons 2 a 10 de la llista----
# Si al fer aquest mapa, no surt "pintada" la terra que vols, és que els polígons que vols de la llista (land) no són 2:10, potser són més o menys, segons la zona. Pots mirar la llista per tenir alguna idea 
ggplot() +
  geom_sf(
    data = land,
    fill = "navy",
    color = NA
  )

#2.5. Convertir data (df_mapa) a sf object----
data_sf <- st_as_sf(df_mapa, coords = c("x", "y"), crs = 4326)

#2.6. Preparar parametros---- 

## 2.6.1. Afegir columna del percentatge d'n respecte nt----
data_sf <- data_sf %>%
  mutate(Percent = (entrevistas_tot / emb_inv_2022) * 100)
View(data_sf)

## 2.6.2. Afegir columna del color segons rang del percentatge (creat a 6.1)----
data_sf <- data_sf %>%
  mutate(color = case_when(
    Percent < 20 ~ "red",
    Percent >= 20 & Percent <= 30 ~ "yellow",
    Percent > 30 ~ "green"
  ))

View(data_sf)

## 2.6.3. Valors d'n per l'escala de la llegenda al mapa----
valores_n <- c(1,2,3,4,5,6,7,8,9)
valores_n <- sort(valores_n)

# 2.7. Hacer el mapa----

ggplot() +
  # Terra (land) de color gris
  geom_sf(data = land, fill = "lightgrey", color = NA) +
  
  # Linea de costa (osm_data_coastline) de color negre
  geom_sf(data = osm_data_coastline$osm_lines, aes(geometry = geometry), 
          color = 'black') +
  
  # Afegir noms (en aquest cas confradies). Per moure les etiquetes, utilitzo !nudge_x"/y i utilitzo el "case_when" per especificar alguns en concret i tots els demés tindrán en valor que posis a TRUE~
  geom_sf_text(data = data_sf, 
               aes(geometry = geometry, 
                   label = cofradia),
               nudge_y= case_when(data_sf$cofradia=="Santanyí"~ -0.02,
                                  data_sf$cofradia=="Sant Antoni"~ 0.07,
                                  data_sf$cofradia=="Maó"~0.02,
                                  data_sf$cofradia=="Eivissa"~-0.03,
                                  data_sf$cofradia=="Palma"~-0.07,
                                  data_sf$cofradia=="Sóller"~0.025,
                                  data_sf$cofradia=="Pollença"~0.005,
                                  TRUE~0),
               nudge_x= case_when(data_sf$cofradia=="Andratx" ~-0.21,
                                  data_sf$cofradia=="Colònia de Sant Jordi"~-0.45,
                                  data_sf$cofradia=="Sant Antoni"~-0.25,
                                  data_sf$cofradia=="Sóller"~-0.17,
                                  data_sf$cofradia=="Ciutadella"~-0.23,
                                  data_sf$cofradia=="Eivissa"~0.07,
                                  data_sf$cofradia=="Formentera"~0.07,
                                  data_sf$cofradia=="Palma"~-0.07,
                                  data_sf$cofradia=="Cala Ratjada"~0.07,
                                  data_sf$cofradia=="Portocolom"~0.035,
                                  TRUE~0.055),
               size = 3, hjust = 0, check_overlap = TRUE) +
  
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
  scale_size_continuous(name = "Count (n)", range = c(3, 10),
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
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
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


#2.8. Guardar el mapa----

# Adjust the width and height to inches
width_in_inches <- 12
height_in_inches <- 8.24

# Save the plot with high quality
ggsave("D:/BASES DE DATOS/CAPITULO_01/graficos_cap01/mapa.TIFF", dpi = 310, width = width_in_inches, height = height_in_inches, units = "in", limitsize = FALSE)

