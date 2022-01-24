###Getting started
#1. Install the latest version of RStudio (Help-Check for Updates)
#2. Download and install the latest version of R: https://cran.cnr.berkeley.edu/
#3. Create a new project in RStudio called "Lecture 1 554" (or similar) in the 
#   same directory as all of the .csv data files, to make it easy to read files. 
#Select part of a line and type ctrl+enter to run that line in the Console
#=======================================

library(tidyverse)    #loads the library for ggplot2 and other useful libraries

print("Hello world")  #prints values contained in object or string
?print                #use ? to get help on a function

#vectors and number sequences
x1 <- 5:12            #assign sequence of numbers 5 to 12 to x1
x1[-c(2,4)]           #exclude the 2nd and 4th value
print(x1+3)           #R does operations on all elements of a vector
x2 <- seq(from=2,to=30,by=3)     #every 3rd number from 2 to 30 
print(x2)

#matrix manipulations (row then column)
xmat <- cbind(x1,rev(x1))  #column binding x1 and reversed x1 to make matrix 
xmat[1:3,-2]           #first three rows, and not the second column 

#lists
xlist <- list(x1, xmat)
xlist[[2]][1:5,]      #first five rows and all columns of element 2 of the list

#data frames
mydata <- data.frame(ID=1:5, names=c("Tim","Anne","Mae","Luke","Pat"))
mydata$names[2:3]

#functions: easy to see everything that goes into a function, 
#  easy to reuse code, no global variables
#e.g. return sum of square of differences between numbers (or vectors)
sumxmy2 <- function(x,y) {  
  return(sum((x-y)^2))
}
sumxmy2(1,2)                      #works for individual values
sumxmy2(c(1,1,1,1),c(1,2,3,4))    #works for two vectors

#if-then-else statements
x <- 10
if (x==10) { 
  print("yes") 
} else { 
  print("no") 
}

#for-loops: 99% of all loops can be written this way
#(there are also while loops, but no need to worry about them)
for(i in 5:1) {       #i becomes each value 5 4 3 2 1 then loop ends
  print(i)             
  print(i^2)
  print(i+10)
}
#press ESC if you get stuck inside a loop---this also happens 
#when the console has lines that start with + instead of > 

######GGPLOT2
library(ggplot2)   #or library(tidyverse)
library(gapminder) #contains data
gapminder          #look at the data; it is contained in a tibble (data frame)
gapminder2007 <- filter(gapminder, year==2007)

ggplot(data=gapminder2007)  #specify where the data are contained
gapminder %>% ggplot()  #or you can "pipe" the data into ggplot

#adding data, aesthetics, and geometries
ggplot(data=gapminder2007) + 
   aes(x=gdpPercap) +
   aes(y=lifeExp) + 
   geom_point()

#this is the same as combining aesthetics
ggplot(data=gapminder2007) + 
  aes(x=gdpPercap, y=lifeExp) + 
  geom_point()

#what if we want a line plot? 
gapminderEurope <- filter(gapminder, continent=="Europe")
ggplot(data=gapminderEurope) + aes(x=year, y=lifeExp) + 
  geom_line() + aes(group=country) + aes(color=country) + 
  geom_point() + scale_y_continuous(limits=c(30,100))

#what if we want a boxplot?
gapminderAmericas <- filter(gapminder, continent=="Americas")
ggplot(data=gapminderAmericas) + aes(x=country, y=lifeExp) + 
  aes(group=country) + geom_boxplot()

#flip the x and y axes
gapminderAmericas <- filter(gapminder, continent=="Americas")
ggplot(data=gapminderAmericas) + aes(x=country, y=lifeExp) + 
  aes(group=country) + geom_boxplot() + coord_flip()

#reorder by GDP
library(tidyverse)   #or library(tidyverse)
library(gapminder) #contains data
gapminderAmericas <- filter(gapminder, continent=="Americas")
ggplot(data=gapminderAmericas) + aes(x=reorder(country, gdpPercap), y=lifeExp) +
  aes(group=country) + geom_boxplot() + coord_flip()

#reorder by life expectancy
library(tidyverse)   #or library(tidyverse)
library(gapminder) #contains data
gapminderAmericas <- filter(gapminder, continent=="Americas")
ggplot(data=gapminderAmericas) + aes(x=reorder(country, lifeExp), y=lifeExp) +
  aes(group=country) + geom_boxplot() + coord_flip()

#======reading in data
#Fall student enrollment at UW Seattle campus
library(tidyverse)
UWenroll <- read_csv(file=here("Documents","data","beautiful_graphics","UWenrollment.csv"))
UWenroll
ggplot(data=UWenroll) + aes(x=Year, y=FallEnrollment) + 
  geom_col()

#==========pie chart in ggplot=================================
#Color codes from http://www.colorschemer.com/colorpix_info.php
#Idea: http://flowingdata.com/2014/12/01/pie-chart-pyramid/
#code: Bryce Mecum and Trevor Branch 
library(tidyverse)
sides <- c("Sky","Sunny side of pyramid",
         "Shady side of pyramid")
greatpyr <- data.frame(x=factor(sides, levels=sides, ordered=TRUE),
                       y=c(14.7,3,1))
ggplot(data=greatpyr, 
            aes(x="", y=y, fill=x)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=2.45) + 
  scale_fill_manual(values=c("#0095D9","#F5E837","#C4B633")) +
  theme(legend.title=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.line=element_blank(),
        axis.text=element_blank(),
        panel.background=element_blank())
        
#======pie chart in base R graphics=================================
par(oma=c(0,0,0,10))  #make space around figure
pie(c(12,3,1,2.7), col=c("#0095D9", "#F5E837", "#C4B633", "#0095D9"),
    border=NA, labels=c(""))  #pie chart with specific colors
legend(x=0.90,y=0.9,pt.bg=c("#0095D9", "#F5E837", "#C4B633"), 
       xpd=NA, pch=22, pt.cex=2.5, border=NA,box.col=NA, col=NA,
       legend=c("Sky","Sunny side of the pyramid",
                "Shady side of the pyramid"))   #add a legend


