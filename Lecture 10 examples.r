############################################################
#R code for FISH554 Beautiful Graphics in R
#Animation lecture
#School of Aquatic and Fishery Sciences
#University of Washington, USA
#Instructor: Trevor A. Branch, tbranch@uw.edu#
############################################################


#============manual method using gifmaker.me==========================
#Create a sequence of images, e.g. in png format
#then open the webpage "gifmaker.me" to create an animated
#gif. Upload png files, select Create GIF Animation, 
#then select Download the GIF. 
#Note: animated gifs run when embedded in powerpoint and 
#the powerpoint is displayed. 
#===========================================================
set.seed(101)
for (i in 1:20) {
   #create a png with sequential numbering
   #this is sent directory on my hard drive
   #called Gifmaker.me\rnorm 
   norm.data <- tibble(nums=rnorm(n=i^2*10))
   range(norm.data$nums)
   ggplot(data=norm.data, aes(x=nums)) + 
      geom_histogram(binwidth=0.2) + theme_minimal() + 
      xlim(c(-4,4))
   ggsave(filename=paste0("Gifmaker.me\\rnorm\\fig",i,".png"))
}

#==========USING GGANIMATE===================================
library(tidyverse)
library(gganimate)
library(magick)   

#animation based on Species column in iris dataset
library(gganimate)
head(iris)
ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) +
   geom_point(aes(colour = Species)) +
   transition_states(Species,
                     transition_length = 2,
                     state_length = 1)

#transitions using group=1
ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) +
   geom_point(aes(colour = Species, group = 1)) +
   transition_states(Species,
                     transition_length = 2,
                     state_length = 1) +
   theme_minimal()

#control entrance and exit of points with enter_fade() and exit_shrink()
ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) +
   geom_point(aes(colour = Species), size = 2) +
   transition_states(Species,
                     transition_length = 2,
                     state_length = 1) +
   theme_minimal() +
   enter_fade() +
   exit_shrink()

#save as an animated gif
anim_save(filename='Figs\\iris.gif')



#============forest example using gganimate===========================
#Data and original source code by Emilio Vilanova <vilanova@uw.edu>
#Shared on 8 March 2020. Simplified for teaching purposes by
#Trevor Branch tbranch@uw.edu
#Data are trees in a forest plot, with animation in the order in
#which data were collected: species, position (X,Y), diameter
#of tree, and other measurements.
#=====================================================================
library(tidyverse)   #includes ggplot
library(gganimate)   #the package that does the magic
library(magick)      #delete library(gifski)

PackForestData <- read_csv(file=here("Documents", "data","beautiful_graphics","PackForest.csv"))

#need to specify that the data are grouped using the
#seq_along command based on the cnt column (numbers 1:nrow(data))
PackMap3 <- ggplot(PackForestData, aes(x = X, y = Y, colour = CommonName,
                              group = seq_along(cnt))) +
   geom_point(aes(size = Diameter, color = CommonName), alpha=0.6) +
   scale_size_area(breaks = c(10,30,50,70,90,120)) +
   scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07",
         "blue4", "chocolate4", "#E69F00", "#56B4E9", "darkmagenta")) +
   theme_bw() +
   theme(plot.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "gray80"),
      legend.position = "right") +
   scale_y_continuous(breaks = seq(0, 100, 20)) +
   scale_x_continuous(breaks = seq(0, 100, 20)) +

   #make the legend circles a bit bigger
   guides(color = guide_legend(override.aes = list(size=5),
                               title = 'Common name'),
          size = guide_legend(title='Diameter (cm)')) +
   labs(title="Stem map - Marteloscope plot - Pack Forest",
        x = "X (meters)", y = "Y (meters)") +
   #this is the gganimate step: reveals each data point based
   #on the groupings in the "cnt" column
   transition_reveal(cnt)

#can just run   PackMap3  but the code below allows fine-tuning.
#duration determines how long it takes to animate in seconds
#height and width are the fixed size of the resulting figure
#This takes ~60 seconds to render
p1 <- animate(PackMap3, duration = 10, height = 500, 
              width = 700)
p1 #shows the plot in R

anim_save("PackForest.gif", p1)

