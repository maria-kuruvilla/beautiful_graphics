#author - Maria Kuruvilla
#date - 31 Jan 2022


# Read in the data in "CalCurrRevenue.csv" and create a complex multipanel plot that has a large 
# plot with total revenue from all species groups, and multiple smaller plots for the 
# other species groups. 
# Use the code in "Ex 1 template.r" to read in the data, convert to long format, 
# and calculate total revenues. 
# Create a plot that is as beautiful or better than Fig. 2 below. 
# Take the time to become familiar with the syntax used in the 
# different multipanel packages introduced here: cowplot, patchwork and gridExtra, 
# and recreate the figure using each of these packages. 
# Do not be satisfied with my figure below, consider ways in which this could be improved 
# further.   

#libraries that I am using

library(tidyverse)
library(here)
library(patchwork)

CCwide <- read_csv(here("Documents","data","beautiful_graphics","CalCurrRevenueTop9.csv"))    
#default reading in of data
head(CCwide)

totals <- tibble(Year=CCwide$Year, Revenue=rowSums(CCwide[,2:10]))
#turn into long format
CClong <- CCwide %>% 
  #change species columns to long
  pivot_longer("Crustaceans":"Anchovies", names_to="Taxa", 
               values_to="Revenue") %>%
  arrange(Taxa)  #sort by taxon (species groups)
print(CClong)

#first make plot with totals

p1 <- ggplot(totals, aes(x = Year, y = Revenue/1000000)) + geom_area() + 
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line())+ 
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(1950, 1970, 1990)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,800), 
                     breaks = c(0, 200, 400, 600, 800)) + 
  ylab("Total revenue for all species (millions of dollars)")


# make multipanel plot with all taxa

p2 <- ggplot(CClong, aes(x = Year, y = Revenue/1000000)) + geom_area()+
  facet_wrap(~Taxa) + 
  theme(strip.background = element_rect(fill=NA, color=NA),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line())+ 
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(1950, 1970, 1990)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,200), 
                     breaks = c(0, 100, 200)) + 
  ylab("Revenue in millions of dollars")
  
  
p1 + p2

p3 <- ggplot(CClong, aes(x = Year, y = Revenue/1000000, fill = Taxa)) + geom_area()+  
  theme(strip.background = element_rect(fill=NA, color=NA),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line())+ 
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(1950, 1970, 1990)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,200), 
                     breaks = c(0, 100, 200)) + 
  ylab("Revenue in millions of dollars") +
  scale_fill_jcolors("pal11")

p3
