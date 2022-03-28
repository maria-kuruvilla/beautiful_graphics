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

porzio_ordered2=porzio_avg[order(porzio_avg$pH,-porzio_avg$avg_distribution),]
names2 = porzio_ordered2$Species[1:25]

file_name <- "project_fig1.pdf"
ggsave(here("Documents","output","beautiful_graphics",
            file_name), width=7, height=7)


#######################################################

for(i in 1:length(porzio_long$Species)){
  species_split <- strsplit(porzio_long$Species[i], " ")
  porzio_long$type[i] <- tail(species_split[[1]],1)
}

ggplot(data = porzio_long, 
       aes(x = factor(pH), y = Species)) +
  geom_jitter(aes(size = distribution),width = 0.06, height = 0.1, alpha = 0.1)+
  theme_minimal()

ggplot(data = porzio_long, 
       aes(x = factor(pH), y = Species)) +
  geom_jitter(aes(size = distribution),width = 0.06, height = 0.1, alpha = 0.1)+
  theme_minimal() + facet_wrap(~type, ncol = 1)

ggplot(data = porzio_long, 
       aes(x = factor(pH), y = Species)) +
  geom_jitter(aes(size = distribution),width = 0.06, height = 0.1, alpha = 0.1)+
  theme_minimal() + scale_size(range = c(0, 10))


ggplot(data = porzio_long, 
       aes(x = factor(pH), y = Species)) +
  geom_jitter(aes(size = distribution),width = 0.06, height = 0.1, alpha = 0.1)+
  theme_minimal() + scale_size(range = c(0, 10))+ 
  facet_wrap(vars(type,Species, ncol = 1))

ggplot(data = porzio_long, 
       aes(x = factor(pH), y = Species)) +
  geom_jitter(aes(size = distribution),width = 0.06, height = 0.1, alpha = 0.1)+
  theme_minimal() + scale_size(range = c(0, 10))           

porzio_ordered3=porzio_long[order(porzio_long$type),]
names2 = porzio_ordered2$Species[1:25]

for(i in 1:length(porzio_avg$Species)){
  species_split <- strsplit(porzio_avg$Species[i], " ")
  porzio_avg$type[i] <- tail(species_split[[1]],1)
}

porzio_avg$type <- factor(porzio_avg$type, levels = c("(O)", "(C)","(R)"))
porzio_ordered4=porzio_avg[order(porzio_avg$pH,porzio_avg$type,-porzio_avg$avg_distribution),]
names4 = porzio_ordered4$Species[1:25]


ggplot(data = porzio_long, 
       aes(x = factor(pH), y = Species)) +
  geom_jitter(aes(size = distribution),width = 0.06, height = 0.1, alpha = 0.1)+
  theme_minimal() + scale_size(range = c(0, 10)) + 
  scale_y_discrete(name="", limits = rev(names4))  

ggplot(data = porzio_long, 
       aes(x = factor(pH), y = Species)) +
  geom_jitter(aes(size = distribution, color = type),width = 0.06, height = 0.1, alpha = 0.3)+
  theme_minimal() + scale_size(name = "Distribution",
                               range = c(0, 10),
                               breaks = c(0,50,100)) + 
  scale_y_discrete(name="", limits = rev(names4))  +
  scale_x_discrete(name="", breaks = c(6.57,7.83,8.14),
                   labels = c("S3 (pH = 6.57)","S2 (pH = 7.83)","S1 (pH = 8.14)"))  +
  scale_color_manual(name = "Taxa",
                     values = c("(O)" = "#7570b3", "(C)" = "#1b9e77", "(R)" = "#d95f02"),
                     breaks = c("(O)","(C)","(R)"),
                     labels = c("Ochrophyta","Chlorophyta","Rhodophyta"), guide = FALSE)+
  theme(plot.caption = element_text(hjust = 0)) +
  labs(caption = "Distribution of the most abundant macroalgal species (>3% coverage)
in 27 (20 * 20 cm) quadrats taken along a pH gradient. There were 6 replicates
per pH. Purple corresponds to Ochrophyta, green corresponds to Chlorophyta
and Orange corresponds to Rhodophyta")


file_name <- "project_fig1_2.png"
ggsave(here("Documents","output","beautiful_graphics",
            file_name), width=7, height=7)
