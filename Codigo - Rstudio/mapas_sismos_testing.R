dir_datosTest <- file.choose()
datosTest <- read.csv(dir_datosTest,header = T, sep = ",")

library(sf)
#install.packages("tmap")
library(tmap)
library(sp)
library(ggplot2)

head(datosTest)

coordinates(datosTest) <- ~LONGITUD + LATITUD

datosTest <- st_as_sf(datosTest)

st_crs(datosTest) <- 4326


ggplot(data = datosTest) + geom_sf(alpha = 0.1)


install.packages("mapview")
install.packages("mapedit")

library(mapview)
library(mapedit)

mapview(peru_d) +
mapview(datosTest, zcol = "MAGNITUD",legend = FALSE, cex = "pop")

mapview(africa)

#Con mapas leaflet
datosTest2 <- read.csv(dir_datosTest,header = T, sep = ",")
library(leaflet)
library(leaflet.extras) 
library(rworldxtra)
library(tidyverse)
library(sf)
library(raster)

leaflet() %>% 
  addTiles() %>% 
  addCircle(data = datosTest2, lat = ~LATITUD, lng = ~LONGITUD)

#leaflet() %>% 
 # addTiles() %>% 
  #addAwesomeMarkers(data = datosTest2, lat = ~LATITUD, lng = ~LONGITUD)


leaflet() %>% 
  addTiles() %>% 
  addHeatmap(data = datosTest2, lat = ~LATITUD, lng = ~LONGITUD, blur = 2.5, radius = 2.1)

