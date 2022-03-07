#=====================================================================
library(tidyverse) #including ggplot2
library(ggmap)
library(here)
library(marmap)
library(rgdal)  #may need to reinstall
library(raster) #may need to reinstall

iceland <- map_data('iceland')   
head(iceland)

#read data
lump_data <- read_csv(here(here("Documents","data","beautiful_graphics","Nonzero lumpfish1989.csv")))
head(lump_data)

lump_data_zero <-  read_csv(here(here("Documents","data","beautiful_graphics","Zero lumpfish1989.csv")))
head(lump_data_zero)

plot_wo_map <- ggplot() + geom_point(data = lump_data_zero, mapping = aes(x = lon, y = lat))
plot_wo_map + theme_minimal() + xlab("Logitutde") + ylab("Latitude")

bathy <- getNOAA.bathy(lon1=-28, lon2=-10, lat1=63, lat2=68,
                       resolution=2, keep=FALSE) 

#create a ggplot object appropriate to the bathy data object
map <- autoplot(bathy, geom=c('raster', 'contour'), colour = "white", size = 0.1,
                show.legend=FALSE) +     #turn off legend
  scale_fill_etopo() +                   #special colors
  theme(axis.title = element_blank()) +  #remove the axis titles
  scale_x_continuous(breaks=seq(-27,-10, 5),  #where to place the values
                     labels=paste0(seq(27,10, -5),'W'), 
                     expand = c(0, 0)) + 
  scale_y_continuous(breaks=seq(63,68,2),  #where to place the values
                     labels=paste0(seq(63,68,2),'N'),
                     expand = c(0, 0))
map + geom_point(data = lump_data_zero, mapping = aes(x = lon, y = lat))
