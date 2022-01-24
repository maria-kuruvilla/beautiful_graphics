
#goal - to read FAO catch
# examine relationships between max length, average catch, and trophic level

# info - Each row of data is one species or group of species. 
#Trophic level is the position in the food web, where plants are at 
# level 1, herbivores at level 2, and carnivores at level 3.  

library(here)
library(tidyverse)

FAOdata1 <- read_csv(file=here("Documents","data","beautiful_graphics","FAO catch.csv"))

ggplot(data=fao) + aes(x=TrophicLevel, y=Lmax) +
  geom_point() + aes(group=Habitat)


ggplot(data=fao) + aes(x=TrophicLevel, y=MeanCatch) +
  geom_point()

ggplot(data=fao) + aes(x=TrophicLevel, y=MeanCatch) +
  geom_boxplot()+ aes(group=Habitat)