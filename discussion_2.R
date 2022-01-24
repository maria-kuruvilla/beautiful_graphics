#========EXERCISES===============================================


library(tidyverse)
library(here)

#**DO EXERCISE 1: accident data from xkcd cartoon
#Try to get ordered data with horizontal barplots
acc <- read_csv(here("Documents","data","beautiful_graphics","Accidents.csv"))    #read in data

ggplot(data=acc, aes(x=reorder(Type,Number), y=Number)) + 
  geom_col(color = "black", fill = "darkred") +
  geom_text(aes(label=Number), position=position_dodge(width=1), vjust=0,hjust=-0.5) +
  xlab("Type of Accident") +
  ylab("Google results") +
  theme_minimal()  + #no expansion below or above min/max
  scale_y_continuous(expand = c(0, 0),limits=c(0,790)) + #no expansion below or above min/max +
  coord_flip()+
  labs(title = "DANGERS: Indexed by the number of google results for \n\"DIED IN A __ ACCIDENT\"")
  

#**DO EXERCISE 2: Old Faithful eruptions at Yellowstone N.P.
#explore different plotting types to see which represents the data the best
OldF <- read_csv(here("Documents","data","beautiful_graphics","OldFaithfulDuration.csv"))

ggplot(data = OldF, aes(x = NextIntervalMin, y = DurationSec))+
  geom_col() +
  xlab("Next interval minute") +
  ylab("Duration of eruption") +
  theme_minimal()  + #no expansion below or above min/max
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))

ggplot(data = OldF, aes(x = NextIntervalMin, y = DurationSec))+
  geom_line(linetype=1) +
  xlab("Next interval minute") +
  ylab("Duration of eruption") +
  theme_minimal()  + #no expansion below or above min/max
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))

ggplot(data = OldF, aes(x = NextIntervalMin, y = DurationSec))+
  geom_pointrange(aes(ymax=DurationSec*1.2, ymin=DurationSec*0.8)) +
  xlab("Next interval minute") +
  ylab("Duration of eruption") +
  theme_minimal()  + #no expansion below or above min/max
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))

ggplot(data = OldF, aes(x = NextIntervalMin, y = DurationSec))+
  
  xlab("Next interval minute") +
  ylab("Duration of eruption") +
  theme_minimal()  + #no expansion below or above min/max
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hex(binwidth=c(2,6))

ggplot(data = OldF, aes(x = NextIntervalMin, y = DurationSec))+
  xlab("Next interval minute") +
  ylab("Duration of eruption") +
  theme_minimal()  + #no expansion below or above min/max
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_point(color="blue", size=2, fill="blue", shape=21, 
             alpha=0.1)

#**CHALLENGE EXERCISE 3: university salaries
#how to represent data that goes outside the bounds of the plot? 
sal <- read_csv(here("Documents","data","beautiful_graphics","UnivSalaries.csv"))