#OBSEVATORIO SISMICO NACIONAL CON SHINY
library(lubridate)
library(tidyverse)
library(dplyr)
#importar datos CATALOGO SISMICO 1960-2021 (IGP) CSV
dir_datos <- file.choose()
datos <- read.csv(dir_datos,header = T, sep = ",")

str(datos)
head(datos)
datos <- datos[-8]

#transformacion a datos de fecha
datos = datos %>% transform(FECHA_UTC = as.Date(as.character(FECHA_UTC), format = "%Y%m%d"))

year(datos$FECHA_UTC)
max(year(datos$FECHA_UTC))
min(year(datos$FECHA_UTC))
  
