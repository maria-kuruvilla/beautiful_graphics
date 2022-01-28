#Read in the data in "CalCurrRevenue.csv" , and convert it to long format data.
#These data are in US$, adjusted for inflation to year 2000.

#Create a plot to show revenue for each of the 12 major species groups in
# the California Current ecosystem. Be creative, and spend some time
# modifying the plot to make it more beautiful. Do not just copy and paste
# the code from the lecture, type it out (this will help in remembering the
#  commands), and look at the help files for each of the functions used to see
# if there are additional useful options. An example plot is given in Fig. 4.

#Experiment with visualizing the data with line plots geom_line() , barplots
# geom_col() and area plots geom_area() . Which type of plot
# illustrates the data best in your opinion? Is multipanel plot needed for this
# data? 

library(tidyverse)
library(here)

FAOwide <- read_csv(here("Documents","data","beautiful_graphics","FAOTotalCatch.csv"))    
#default reading in of data
head(FAOwide)

CCRwide <- read_csv(here("Documents","data","beautiful_graphics","CalCurrRevenue.csv"))    
#default reading in of data
head(CCRwide)

#changing wide format to long format
CCRlong <- CCRwide %>% #piping data
  pivot_longer("Crustaceans":"Sharks & rays", names_to = "Species_type", 
              values_to = "Revenue") 

#plotting the CCRlong data - one plot for each species type

basic_plot <- ggplot(data=CCRlong, aes(x = Year, y = Revenue, color = Species_type))+
  geom_line()

basic_plot <-  ggplot(data=CCRlong, aes(x = Year, y = Revenue, color = Species_type))+
  geom_line() + facet_wrap(~Species_type)

#make y axis in millions

basic_plot <-  ggplot(data=CCRlong, aes(x = Year, y = Revenue/1000000, color = Species_type))+
  geom_line() + facet_wrap(~Species_type) + 
  ylab("Revenue (million US dollars)")

#make x axis and y axis to have only 3 labels

basic_plot <-  ggplot(data=CCRlong, aes(x = Year, y = Revenue/1000000, color = Species_type))+
  geom_line(show.legend = FALSE, size = 1) + facet_wrap(~Species_type) + 
  scale_x_continuous(breaks = c(1960,1980,2000)) +
  scale_y_continuous(breaks = c(0,100,200)) +
  ylab("Revenue (million US dollars)")

#make theme minimal

basic_plot + theme(panel.background = element_rect(fill = NA))

#get rid of the background of the titles

basic_plot + theme(panel.background = element_rect(fill = NA),
                   strip.background = element_rect(fill = NA),
                   axis.text = element_text(size = 10))

#get rid of the legend


basic_plot
