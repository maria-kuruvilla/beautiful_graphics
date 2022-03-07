#Goal - To remake figure 2 of Porzio et al.

#imports
library(tidyverse)
library(here)
library(RColorBrewer)

#read data
porzio_data <- read_csv(here("Documents","data","beautiful_graphics","kite_diagrams_data_og.csv"))    

head(porzio_data)

porzio_long <- porzio_data %>% pivot_longer("S1":"S3_8", names_to = "sector"
                                            , values_to = "distribution")
for(i in 1:length(porzio_long$sector)){
  sector <- substr(porzio_long$sector[i],1,2)
  if(sector == "S1"){
    porzio_long$pH[i] <- 8.14
  }
  else if(sector == "S2"){
    porzio_long$pH[i] <- 7.83
  }
  else {
    porzio_long$pH[i] <- 6.57
  }
}

porzio_avg <- porzio_long %>%
  group_by(pH,Species) %>%
  summarise(avg_distribution = mean(distribution), sd_distribution = sd(distribution))

ggplot(data = porzio_avg, 
       aes(x = pH, y = avg_distribution)) +
  geom_line() + facet_wrap(~Species, ncol=1)

ggplot(data = porzio_long, 
       aes(x = factor(pH), y = distribution)) +
  geom_violin() + facet_wrap(~Species)


ggplot(data = porzio_avg, 
       aes(x = factor(pH), y = avg_distribution)) +
  geom_col() + facet_wrap(~Species)

ggplot(data = porzio_long, 
       aes(x = pH, y = Species)) +
  geom_count(aes(size = distribution)) 


ggplot(data = porzio_long, 
       aes(x = factor(pH), y = distribution)) +
  geom_point() + facet_wrap(~Species, ncol = 1, strip.position = "left")


ggplot(data = porzio_long, 
       aes(x = factor(pH), y = distribution)) +
  geom_col() + facet_wrap(~Species, ncol = 1, strip.position = "left")

ggplot(data = porzio_long, 
       aes(x = factor(pH), y = Species)) +
  geom_jitter(aes(size = distribution),width = 0.06, height = 0.1, alpha = 0.1)+
  theme_minimal() + scale_y_discrete(name="", limits = rev(names))

porzio_ordered=porzio_avg[order(porzio_avg$pH,-porzio_avg$avg_distribution),]
names = porzio_ordered$Species[1:25]



