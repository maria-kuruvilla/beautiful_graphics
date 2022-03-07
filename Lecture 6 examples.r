####################################
#Lecture on color, Trevor Branch, FISH554
#Using color in R
#Look at Stowers Guide to color on class website 
#for named colors
####################################

library(tidyverse)
library(RColorBrewer)

#list of named colors in R
colors()

#create some random data, with sorted y values
values <- as_tibble(cbind(x=1:10, 
      y=sort(sample(1:100, size=10, replace=TRUE))))
values

#the default barplot
ggplot(data=values, aes(x=x, y=y)) + 
   geom_col() + 
   theme_minimal()

#color the bars by the values in the bars
ggplot(data=values, aes(x=x, y=y, fill=y)) + 
   geom_col() + 
   theme_minimal()

#change the default colors of all bars, color=outline, fill=inside
ggplot(data=values, aes(x=x, y=y)) + 
   geom_col(fill='darkgreen', color='black') + 
   theme_minimal()

#create a smooth set of colors between two colors using 
#scale_fill_gradient
ggplot(data=values, aes(x=x, y=y, fill=y)) + 
   geom_col() + theme_minimal() + 
   scale_fill_gradient(low='red', high='blue')

#create a custom diverging palette with a midpoint using
#scale_fill_gradient2, midpoint is value of middle of scale
#limits let you decide what value is the low color and high color
ggplot(data=values, aes(x=x, y=y, fill=y)) + 
   geom_col() + theme_minimal() + 
   scale_fill_gradient2(low='red', mid='gray90', high='blue', 
                        midpoint=50, limits=c(0,100))

#change the fill colors to a smooth palette of lots of colors
#color is related to the height of the bar
#rich.colors() is a better palette than rainbow()
library(gplots)
ggplot(data=values, aes(x=x, y=y, fill=y)) + 
   geom_col() + 
   theme_minimal() + 
   scale_fill_gradientn(colors = rich.colors(n=10)) 

#instead just color each bar a different color unrelated to the 
#values (remove from the aes() function)
ggplot(data=values, aes(x=x, y=y)) + 
   geom_col(fill = rich.colors(n=10)) + 
   theme_minimal() 

#points instead of bars, shape 21 is circle with different outer
#and inner bars, use nrow() to find out how many colors to select
ggplot(data=values, aes(x=x, y=y)) + 
   geom_point(fill = rich.colors(n=nrow(values)), color='black',
              size=10, shape=21) + 
   theme_minimal() 

#==========colorBrewer================================================
library(RColorBrewer)
display.brewer.all()

#only plot the first 8 values (too many values for colors for all)
ggplot(data=values[1:8,], aes(x=x, y=y)) + 
   geom_point(fill = brewer.pal(n=8, name='Dark2'), 
              size=10, shape=21) + 
   theme_minimal() 

#go to website and look for colors www.colorbrewer2.org
#this is what I do since it is easy to see how good they look on a map
bcolors <- c('#e41a1c', '#377eb8', '#4daf4a', '#984ea3', '#ff7f00')
ggplot(data=values[1:5,], aes(x=x, y=y)) + 
   geom_point(fill = bcolors, size=10, shape=21) + 
   theme_minimal() 

#use colorbrewer as a palette to interpolate between colors
#scale_fill_gradientn means interpolate between whatever colors
# you give it, limit sets the minimum for the lowest color
# and NA allows the default upper value to stand
display.brewer.all()
ggplot(data=values, aes(x=x, y=y, fill=y)) + 
   geom_col() + theme_minimal() + 
   scale_fill_gradientn(colors=brewer.pal(n=7,name='Oranges'),
                        limit=c(0,NA))

#=====transparency==========================
#create some random data from two normal distributions
set.seed(100) #so that random draws can be repeated
values2 <- as_tibble(cbind(x=rnorm(1000), y=rnorm(1000)))

#shape 21 is a circle with outline=color, fill=fill
ggplot(data=values2, aes(x=x, y=y)) + 
   geom_point(color='black', fill = 'red', size=5, shape=21) + 
   theme_minimal() 

#shape 16 is circle of just one color
#alpha = 0.2 is very transparent (0 <= alpha <= 1)
ggplot(data=values2, aes(x=x, y=y)) + 
   geom_point(color = 'red', size=5, shape=16, alpha=0.2) + 
   theme_minimal() 

#less transparent
ggplot(data=values2, aes(x=x, y=y)) + 
   geom_point(color = 'red', size=5, shape=16, alpha=0.6) + 
   theme_minimal() 

#only want fill to be transparent but outer line of circle solid
#note: adding 5 circles with alpha = 0.2 results in solid color
ggplot(data=values2, aes(x=x, y=y)) + 
   geom_point(size=5, shape=21,  
              color='red', 
              fill = adjustcolor(col='red',alpha.f=0.2)) + 
   theme_minimal() 


#====miscellaneous useful functions
col2rgb('yellow')  #return the red, green, blue components of a color

#convert RGB colors scaled from 0 to 1 into a hexadecimal number  
rgb(1.0, 1.0, 0.0) 

#convert RGB integers between 0 and 255 into hexadecimal
rgb(255, 255, 0, maxColorValue=255)  

#return all named colors with "red" in their names
#'grep' = useful command to return locations of matches of strings
#         within vectors
colors()[grep('red',colors())]       
                                     


