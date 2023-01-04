library(sf)
library(purrr)
library(tidyverse)
library(ggplot2)
library(ggrepel)


dirmapas <- "C:/Users/Lenovo/Documents/GitHub/Computacion-Paralela_Yefer/Geoubicacion sismica peru/sismos-R/DEPARTAMENTOS_inei_geogpsperu_suyopomalia"

setwd(dirmapas)

peru_d <- st_read("DEPARTAMENTOS_inei_geogpsperu_suyopomalia.shp") # formato: Shapefile datos espaciales

peru_d

#Primer mapa: solo muestra los límites departamentales
ggplot(data = peru_d) +
  geom_sf()

#Graficar solo un departamento: graficaremos Puno
ggplot(data = peru_d %>%
         filter(NOMBDEP=="PUNO")) +
  geom_sf()
