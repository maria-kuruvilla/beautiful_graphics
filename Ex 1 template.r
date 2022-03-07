#=====================================================================
#Analysis of revenue data by LME (does not include High Seas) from 
#Data downloaded from http://www.seaaroundus.org/sponsor/feru.aspx
#Sea Around Us Project, Fisheries Economics Research Unit, Global 
#Fisheries Economics. 
#Date downloaded: 16 October 2011
#California Current LME ex-vessel revenue data, in year 2000 US$.  
#Sumaila UR et al. (2007) A global ex-vessel fish price database: 
#construction and applications. Journal of Bioeconomics 9:39-51.
#=====================================================================
#Template for Exercise 1
#=====================================================================
library(tidyverse)
library(cowplot)
CCwide <- read_csv("CalCurrRevenueTop9.csv")
totals <- tibble(Year=CCwide$Year, Revenue=rowSums(CCwide[,2:10]))
#turn into long format
CClong <- CCwide %>% 
   #change species columns to long
   pivot_longer("Crustaceans":"Anchovies", names_to="Taxa", 
                values_to="Revenue") %>%
   arrange(Taxa)  #sort by taxon (species groups)
print(CClong)
