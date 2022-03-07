#author - Maria Kuruvilla
#date - 6 Feb 2022

#read data from details.csv

library(tidyverse)
library(here)
library(RColorBrewer)

data <- read_csv(here("Documents","data","beautiful_graphics","details.csv"))    
#default reading in of data
head(data)


#plot it in a way to find the "hidden easter egg"
ggplot(data = data, aes(x=x, y=y)) +  theme_minimal() +
  geom_point(color = "lightcyan2", fill = "lightcyan2", size = 1, alpha = 0.1)

ggplot(data=data, aes(x=x, y=y)) + 
  geom_point(size=1, shape=21,  
             color='forestgreen', 
             fill = adjustcolor(col='forestgreen',alpha.f=0.1)) + 
  theme_minimal() 

ggplot(data=data, aes(x=x, y=y)) + 
  geom_hex(bins = 150) + 
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() 


ggplot(data = data, aes(x=x, y=y)) +  theme_minimal() +
  geom_point(color = 'forestgreen', fill ='forestgreen', size = 0.1, alpha = 0.1)

library(ggpointdensity)
ggplot(data=data, aes(x=x, y=y)) + 
  geom_hex(bins = 150) + 
  #scale_fill_gradient(low = "white", high = "blue") +
  geom_pointdensity() +
  theme_minimal() 


#problem 2

#read fairbanks weather data
data <- read_csv(here("Documents","data","beautiful_graphics","FairbanksAK weather.csv"))  
head(data)


#plot average annual high temperature against day of year using geom_col and
# scale_fill_gradient2

ggplot(data = data, aes(x = DayOfYear, y = HighTempC,fill = HighTempC)) +
  geom_col(color=NA,width = 1) + #the whole width is filled by colomn - no spaces
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0, limit =c(-25,25)) + 
  xlab("Day of the year") +
  ylab(expression(paste("Average high temperature (", degree,"C)")))


# 3 

# use geom_col() to plot precipitation for Olympic NP from data in HohRangerStation.csv
data <- read_csv(here("Documents","data","beautiful_graphics","HohRangerStation.csv"))  
head(data)

data$Month <- factor(data$Month, levels = month.name)
ggplot(data = data, aes(x=Month, y = Precipitationmm, fill = Season)) +
  geom_col() + 
  scale_fill_brewer(type = "qual",palette = "Set1")

ggplot(data = data, aes(x=Month, y = Precipitationmm, fill = Precipitationmm)) +
  geom_col() + 
  scale_fill_gradientn(colors = brewer.pal(7,"Greens"))

#make limit c(0,NA) so that 0 is white and not the lowest be white
       