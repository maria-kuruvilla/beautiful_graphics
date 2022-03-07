#Goal - pictures in figure, texts and mathematics expressions
#exercise 1

#read data

library(ggplot2)
library(cowplot)
library(magick)
library(Rcpp)
library(tidyverse)
library(here)

fisheries_data <- read_csv(here("Documents","data","beautiful_graphics","Worm2009Fig3.csv"))    
head(data)

p <- ggplot(data=fisheries_data, aes(x=UUmsy, y=BBmsy, label=ScientificName)) + 
  geom_point(size=4, color='red', alpha = 0.2) + theme_minimal() +
  labs(x = expression(U[current]/U[msy]),y=expression(B[current]/B[msy])) + 
  geom_hline(yintercept=1, linetype='dashed', col = 'gray50') + 
  geom_vline(xintercept=1, linetype='dashed', col = 'gray50')

p
#exercise 2 phylopic.org


p <- ggplot(data=fisheries_data, aes(x=UUmsy +1, y=BBmsy +1, label=ScientificName)) + 
  geom_point(size=4, color='red', alpha = 0.2) + theme_minimal() +
  labs(x = expression(U[current]/U[msy]),y=expression(B[current]/B[msy])) + 
  geom_hline(yintercept=2, linetype='dashed', col = 'gray50') + 
  geom_vline(xintercept=2, linetype='dashed', col = 'gray50') + 
  scale_y_log10(breaks=c(1,3,5),
                labels=c(0,2,4)) +
  scale_x_log10(breaks=c(1,3,5),
                labels=c(0,2,4))


ggdraw(p) + 
  draw_image(image=here("Documents","data","beautiful_graphics","salmon.png"), x = 0.55, y = 0.55, 
                       width=0.4, height=0.4)

#4 truncate the axes is at 2 and add all the points to 2

p1 <- ggplot(data=fisheries_data, aes(x=ifelse(UUmsy>2,2,UUmsy), y=ifelse(BBmsy>2,2,BBmsy), label=ScientificName)) + 
  geom_point(size=4, color='red', alpha = 0.2) + theme_minimal() +
  labs(x = expression(U[current]/U[msy]),y=expression(B[current]/B[msy])) + 
  geom_hline(yintercept=1, linetype='dashed', col = 'gray50') + 
  geom_vline(xintercept=1, linetype='dashed', col = 'gray50')
p1

p <- ggplot(data=fisheries_data, aes(x=UUmsy, y=BBmsy, label=ScientificName)) + 
  geom_point(size=4, color='red', alpha = 0.2) + theme_minimal() +
  labs(x = expression(U[current]/U[msy]),y=expression(B[current]/B[msy])) + 
  geom_hline(yintercept=1, linetype='dashed', col = 'gray50') + 
  geom_vline(xintercept=1, linetype='dashed', col = 'gray50') +
  scale_x_continuous(oob=scales::squish, limits = c(0,2), labels = c('0','0.5','1','1.5','2+')) + 
  scale_y_continuous(oob=scales::squish, limits = c(0,2), labels = c('0','0.5','1','1.5','2+'))
p

#highlight Atlantic cod (ScientificName = Gadus morhua)

p2 <- ggplot(data=fisheries_data, aes(x=UUmsy, y=BBmsy, label=ScientificName)) + 
  geom_point(size=4, color='red', alpha = 0.2) + theme_minimal() +
  labs(x = expression(U[current]/U[msy]),y=expression(B[current]/B[msy])) + 
  geom_hline(yintercept=1, linetype='dashed', col = 'gray50') + 
  geom_vline(xintercept=1, linetype='dashed', col = 'gray50') +
  scale_x_continuous(oob=scales::squish, limits = c(0,2), labels = c('0','0.5','1','1.5','2+')) + 
  scale_y_continuous(oob=scales::squish, limits = c(0,2), labels = c('0','0.5','1','1.5','2+'))
ggdraw(p2) + 
  draw_image(image=here("Documents","data","beautiful_graphics","salmon.png"), x = 0.55, y = 0.55, 
             width=0.4, height=0.4)
library(ggrepel)
p2 + geom_text_repel(color = 'gray50', size=2)

p2 <- ggplot(data=fisheries_data, aes(x=UUmsy, y=BBmsy)) + 
  geom_point(size=4, alpha = 0.2) + theme_minimal() +
  labs(x = expression(U[current]/U[msy]),y=expression(B[current]/B[msy])) + 
  geom_hline(yintercept=1, linetype='dashed', col = 'gray50') + 
  geom_vline(xintercept=1, linetype='dashed', col = 'gray50') +
  scale_x_continuous(oob=scales::squish, limits = c(0,2), labels = c('0','0.5','1','1.5','2+')) + 
  scale_y_continuous(oob=scales::squish, limits = c(0,2), labels = c('0','0.5','1','1.5','2+'))
p2
