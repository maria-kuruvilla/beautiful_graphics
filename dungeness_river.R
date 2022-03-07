library(tidyverse) #including ggplot2
library(ggmap)
library(here)
library(marmap)
library(rgdal)  #may need to reinstall
library(raster) #may need to reinstall

bathy2 <- getNOAA.bathy(lon1=-123.5, lon2=-123, lat1=48, lat2=49,
                       resolution=2, keep=FALSE) 
map2 <- autoplot(bathy2, geom=c('raster', 'contour'), colour = "white", size = 0.1,
                show.legend=FALSE) +     #turn off legend
  scale_fill_etopo() +                   #special colors
  theme(axis.title = element_blank())  #remove the axis titles
map2
