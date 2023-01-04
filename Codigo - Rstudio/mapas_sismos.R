#nuevas formas

str(datos)

library(sp)
library(rmarkdown)
library(dplyr)
library(lubridate)
library(ggmap)
library(ggplot2)
library(viridis)
library(rcartocolor)
library(plotly)
library(sf)

#######################NACIONAL#######################

p_nacional <- function(tipo, yearsSelect = 1960:2021){
  p = ggplot(data = peru_d) +
    geom_sf()
  #Mapa de calor 1
  if(tipo == 1){
    p = p + stat_density2d(data = datos %>%  filter(year(FECHA_UTC) %in% yearsSelect),
                     aes(x = LONGITUD ,
                         y = LATITUD,
                         fill = ..level..,
                         alpha = ..level..),
                     geom = "polygon",
                     size = 0.01,
                     bins = 8) +
      scale_fill_gradient(low = "yellow",
                          high = "red") +
      scale_alpha(range = c(0.2, 0.7)) +
      theme(legend.position = "none") +
      labs(title="Mapa de calor",
           subtitle=paste(yearsSelect, collapse = " - "),
           caption="Datos de IGP2022")
  }
  
  #Mapa de calor 2
  if(tipo == 2){
    p = p + geom_density2d(data = datos %>%  filter(year(FECHA_UTC) %in% yearsSelect),
                          aes(x = LONGITUD,
                              y = LATITUD),
                          size = 0.9,
                          bins = 8) +
      stat_density2d(data = datos %>%  filter(year(FECHA_UTC) %in% yearsSelect),
                     aes(x = LONGITUD,
                         y = LATITUD,
                         fill = ..level..,
                         alpha = ..level..),
                     geom = "polygon",
                     size = 0.01,
                     bins = 8) +
      theme(legend.position = "none")+
      labs(title="Mapa de calor - 2",
           subtitle=paste(yearsSelect, collapse = " - "),
           caption="Datos de IGP2022")
  }
  
  #Mapa de magnitudes
  if(tipo == 3){
    p = p + geom_point(data = datos %>%  filter(year(FECHA_UTC) %in% yearsSelect),
                       aes(x = LONGITUD,
                           y = LATITUD,
                           color = MAGNITUD,
                           alpha = MAGNITUD,
                           size = MAGNITUD))+
      scale_radius(range = c(0.1,1.5))+
      scale_color_viridis()+
      labs(title="Magnitud de los sismos",
           subtitle=paste(yearsSelect, collapse = " - "),
           caption="Datos de IGP2022")
  }
  
  #Mapa de profundidades
  if(tipo == 4){
    p = p + geom_point(data = datos %>%  filter(year(FECHA_UTC) %in% yearsSelect),
                       aes(x = LONGITUD,
                           y = LATITUD,
                           color = PROFUNDIDAD,
                           alpha = PROFUNDIDAD,
                           size = PROFUNDIDAD))+
      scale_radius(range = c(0.1,1.5))+
      scale_color_viridis()+
      labs(title="Profundidad de los sismos",
           subtitle=paste(yearsSelect, collapse = " - "),
           caption="Datos de IGP2022")
  }
  p = p + theme_bw()
  return(ggplotly(p))
  #return(p)
}

p_nacional(3, 1960:2021)
p_nacional(1, year_test)



##################REGIONAL##############

coordenadas_dep = data.frame(dep = c(), minx=c(), miny=c(), maxx = c(), maxy = c())

for (i in 1:25) {
  minx = as.data.frame(st_coordinates(peru_d[i,]$geometry))$X %>% min()
  miny = as.data.frame(st_coordinates(peru_d[i,]$geometry))$Y %>% min()
  maxx = as.data.frame(st_coordinates(peru_d[i,]$geometry))$X %>% max()
  maxy = as.data.frame(st_coordinates(peru_d[i,]$geometry))$Y %>% max()
  agregar = data.frame(dep = c(peru_d[i,]$NOMBDEP), minx=c(minx), miny=c(miny), maxx = c(maxx), maxy = c(maxy))
  coordenadas_dep = rbind(coordenadas_dep,agregar)
}

p_departamento <- function(tipo, departamento, yearsSelect){
  depa = coordenadas_dep[coordenadas_dep == departamento,]
  p = ggplot(data = peru_d %>%
               filter(NOMBDEP == departamento)) +
    geom_sf()
  #Mapa de calor 1
  if(tipo == 1){
    p = p + stat_density2d(data = datos %>% 
                             filter(LONGITUD > depa$minx,
                                    LONGITUD < depa$maxx,
                                    LATITUD > depa$miny,
                                    LATITUD < depa$maxy,
                                    year(FECHA_UTC) %in% yearsSelect),
                           aes(x = LONGITUD ,
                               y = LATITUD,
                               fill = ..level..,
                               alpha = ..level..),
                           geom = "polygon",
                           size = 0.01,
                           bins = 8) +
      scale_fill_gradient(low = "yellow",
                          high = "red") +
      scale_alpha(range = c(0.2, 0.7)) +
      theme(legend.position = "none") +
      labs(title="Mapa de calor",
           subtitle=paste(yearsSelect, collapse = " - "),
           caption="Datos de IGP2022")
  }
  
  #Mapa de calor 2
  if(tipo == 2){
    p = p + geom_density2d(data = datos %>%  
                             filter(LONGITUD > depa$minx,
                                                     LONGITUD < depa$maxx,
                                                     LATITUD > depa$miny,
                                                     LATITUD < depa$maxy,
                                                     year(FECHA_UTC) %in% yearsSelect),
                           aes(x = LONGITUD,
                               y = LATITUD),
                           size = 0.9,
                           bins = 8) +
      stat_density2d(data = datos %>% 
                       filter(LONGITUD > depa$minx,
                                               LONGITUD < depa$maxx,
                                               LATITUD > depa$miny,
                                               LATITUD < depa$maxy,
                                               year(FECHA_UTC) %in% yearsSelect),
                     aes(x = LONGITUD,
                         y = LATITUD,
                         fill = ..level..,
                         alpha = ..level..),
                     geom = "polygon",
                     size = 0.01,
                     bins = 8) +
      theme(legend.position = "none")+
      labs(title="Mapa de calor - 2",
           subtitle=paste(yearsSelect, collapse = " - "),
           caption="Datos de IGP2022")
  }
  
  #Mapa de magnitudes
  if(tipo == 3){
    p = p + geom_point(data = datos %>% 
                         filter(LONGITUD > depa$minx,
                                                 LONGITUD < depa$maxx,
                                                 LATITUD > depa$miny,
                                                 LATITUD < depa$maxy,
                                                 year(FECHA_UTC) %in% yearsSelect),
                       aes(x = LONGITUD,
                           y = LATITUD,
                           color = MAGNITUD,
                           alpha = MAGNITUD,
                           size = MAGNITUD))+
      scale_radius(range = c(0.1,1.5))+
      scale_color_viridis()+
      labs(title="Magnitud de los sismos",
           subtitle=paste(yearsSelect, collapse = " - "),
           caption="Datos de IGP2022")
  }
  
  #Mapa de profundidades
  if(tipo == 4){
    p = p + geom_point(data = datos %>%  
                         filter(LONGITUD > depa$minx,
                                                 LONGITUD < depa$maxx,
                                                 LATITUD > depa$miny,
                                                 LATITUD < depa$maxy,
                                                 year(FECHA_UTC) %in% yearsSelect),
                       aes(x = LONGITUD,
                           y = LATITUD,
                           color = PROFUNDIDAD,
                           alpha = PROFUNDIDAD,
                           size = PROFUNDIDAD))+
      scale_radius(range = c(0.1,1.5))+
      scale_color_viridis()+
      labs(title="Profundidad de los sismos",
           subtitle=paste(yearsSelect, collapse = " - "),
           caption="Datos de IGP2022")
  }
  p = p + theme_bw()
  return(ggplotly(p))
  #return(p)
}

year_test = c(1992,2001,2010,2020)
p_departamento(3,"PUNO",year_test)


