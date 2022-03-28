#===============WORKFLOW TEMPLATE=====================================
#Name, date, email
#Created by Trevor A. Branch on <date> tbranch@uw.edu
#Source of data
#Data provided by Emilio Vilanova on 7 March 2020
#Where was figure used / intended
#Figure created as part of Branch et al. xxxx Plos One e999999
#=====================================================================

#list all required packages 
library(tidyverse)

#create a function that does everything and does not create a bunch 
#of global variables. Anything that needs to change between png / pdf
#or Word vs Powerpoint can be changed as a parameter to the function
marvelous.fig <- function(filename, xbin, ybin) {
   PackForestData <- read_csv(file=filename)
   p1 <- ggplot(PackForestData, aes(x = X, y = Y, color = CommonName, 
                                    size = Diameter)) + 
      theme_minimal() +
      geom_point(alpha=0.6) + 
      scale_y_continuous(breaks = seq(0, 100, xbin)) + 
      scale_x_continuous(breaks = seq(0, 100, ybin))
   return(p1) #function returns the plot
}

#if data file changes, can just change it in the call to the function
marvelplot <- marvelous.fig(filename='PackForest.csv', xbin=20, ybin=20)
marvelplot

#save the plot to file, specify size, dpi, plot to save
ggsave('Marvel.png', width=8, height=5, dpi=600, plot=marvelplot)

#can set different parameters for a pdf file or PowerPoint presentation
ggsave('Marvel.pdf', width=5, height=3, plot=marvelplot)
