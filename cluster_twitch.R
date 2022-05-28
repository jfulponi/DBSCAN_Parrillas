library(sf)
library(tidyverse)
library(geosphere)
library(dbscan)
library(ggmap)
library(leaflet)

suelo <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/relevamiento-usos-del-suelo/relevamiento-usos-del-suelo-2017.csv")

barrios <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios.geojson") 

suelo <- suelo %>% st_as_sf(coords=c("X","Y"), remove = FALSE) %>% 
  mutate(X=as.numeric(X), Y=as.numeric(Y)) %>% 
  rename(lng=X, lat=Y)

filtered_df <- suelo %>% filter(grepl("PARRILLA", suelo$TIPO2_16))

st_crs(filtered_df) <- st_crs(barrios)

intersect <- st_join(barrios, filtered_df)

intersect %>% st_drop_geometry() %>% 
  group_by(BARRIO.x) %>% 
  summarise(n_filtered_df = n(),
            area = min(as.numeric(AREA), na.rm = T),
            ratio = n_filtered_df/area) %>% 
  arrange(-ratio) %>% rename(BARRIO = BARRIO.x) %>% 
  left_join(barrios %>% select(BARRIO, geometry)) %>%  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill=ratio), color = NA)+
  geom_sf(data=filtered_df, color = "darkred", size = .02) + 
  theme_void() + 
  theme(legend.position = "none") +
  labs(title = "Distribución de las Parrillas en CABA por barrio, ajustado por área.", 
       subtitle = "Según Relevamiento de Usos del Suelo - 2017.") + 
  scale_fill_distiller(palette = "BrBG")

caba <- get_stamenmap(bbox = as.vector(st_bbox(intersect)), maptype = "toner-background", zoom = 13)

ggmap(caba) +
  geom_point(data = filtered_df, aes(lng, lat))


distancias <- filtered_df %>% st_drop_geometry() %>% 
  select(lng, lat) %>%  # extraemos las columnas de longitud y latitud
  distm(fun = distGeo) %>% # Calculamos las distancias de acuerdo a la curvatura de la Tierra
  as.dist() # convertimos en una matriz de distancias (el tipo de objeto que DBSCAN espera)

clusters <- dbscan(distancias, eps = 800, minPts = 20)
clusters

filtered_df <- filtered_df %>% 
  select(-cluster) %>% 
  cbind(cluster = clusters[["cluster"]])

ggmap(caba) +
  geom_point(data = filter(filtered_df, cluster == 0), 
             aes(x = lng, y = lat), 
             alpha = .5) +
  geom_point(data = filter(filtered_df, cluster > 0), 
             aes(x = lng, y = lat, color = factor(cluster)), 
             alpha = .5, size = 1.5) +
  labs(title = "Clusters de Parrillas",
       color = "Cluster") + 
  theme_void()
