#Exercise for Lecture 1 FISH554: exploring data from the FAO database
#the trophic level, species, maximum length, habitat, mean catch
#for each of the species, based on data used in 
#Branch TA et al. (2010) The trophic fingerprint of marine fisheries. 
#Nature 468:431-435

#reading in data
library(tidyverse)
FAOdata1 <- read_csv(file="FAO catch.csv")
FAOdata1

#using ggplot to make explorations of the data
#relation between Lmax and Trophic level
ggplot(data=FAOdata1) +
  aes(x=Lmax, y=TrophicLevel) + 
  geom_point()

ggplot(data=FAOdata1) +
  aes(x=Lmax, y=TrophicLevel) + 
  geom_point() +
  scale_x_log10()

ggplot(data=FAOdata1) +
  aes(x=Lmax, y=TrophicLevel) + 
  geom_point() +
  scale_x_log10() +
  aes(size=MeanCatch) +
  scale_size_area()

roundedFAO <- FAOdata1
roundedFAO$TrophicLevel <- round(roundedFAO$TrophicLevel,1)
ggplot(data=roundedFAO) +
  aes(x=Lmax, y=TrophicLevel) + 
  geom_point() +
  scale_x_log10() +
  aes(size=MeanCatch) +
  scale_size_area()

#boxplots
ggplot(data=roundedFAO) +
  aes(x=TrophicLevel, y=Lmax) + 
  geom_boxplot() + aes(group=TrophicLevel)

#trophic level vs maximum length
ggplot(data=roundedFAO) +
  aes(x=TrophicLevel, y=Lmax) + 
  geom_boxplot() + aes(group=TrophicLevel) + 
  scale_y_log10()

#trophic level vs average catch
ggplot(data=roundedFAO) +
  aes(x=TrophicLevel, y=MeanCatch) + 
  geom_boxplot() + aes(group=TrophicLevel) + 
  scale_y_log10()
