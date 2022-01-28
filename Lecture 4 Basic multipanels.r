##################################################################
###Multipanel plots
#Trevor A. Branch, tbranch@uw.edu
#Lecture notes for FISH554, Beautiful Graphics in R
#School of Aquatic and Fishery Sciences, University of Washington
#(With help from Dan Ovando, danovan@uw.edu)
##################################################################

library(tidyverse)

FAOwide <- read_csv("FAOTotalCatch.csv")     #default reading in of data
head(FAOwide)

#it is in wide format, but we need long format. Used to use gather() but now 
#pivot_longer() is preferred. All data bits chained together in one step with 
#lots of %>% (termed "pipes")
FAOlong <- FAOwide %>% 
   #change year columns to long
   pivot_longer("1950":"2007", names_to="Year", values_to="Catch") %>%
   arrange(FAO.area) %>% #sort by FAO.area
   mutate(Year = as.integer(Year)) #was a string, changed to integer
head(FAOlong)

#so we try and make a plot where each area has a different line
ggplot(data=FAOlong, aes(x=Year, y=Catch, color=FAO.names)) + 
   geom_line() 

#turn them into panels! 
ggplot(data=FAOlong, aes(x=Year, y=Catch, color=FAO.names)) + 
   geom_line() + 
   facet_wrap(~FAO.names)

#I don't need them to be different colors now
ggplot(data=FAOlong, aes(x=Year, y=Catch)) + 
   geom_line() + 
   facet_wrap(~FAO.names)

#make it a little prettier using area plots
ggplot(data=FAOlong, aes(x=Year, y=Catch)) + 
   geom_area() + 
   facet_wrap(~FAO.names)
#ggsave("figs\\FAOcatches default.pdf", width=7, height=7)

#get rid of background gray but keep a box around each plot
ggplot(data=FAOlong, aes(x=Year, y=Catch)) + 
   geom_area() + 
   facet_wrap(~FAO.names) + 
   theme(panel.background = element_rect(fill=NA, 
                                         color="gray50"))

#get rid of gridlines (still there, but white)
ggplot(data=FAOlong, aes(x=Year, y=Catch)) + 
   geom_area() + 
   facet_wrap(~FAO.names) + 
   theme(panel.background = element_rect(fill=NA, 
                                         color="gray50"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())

#now get rid of space between min values and axes
ggplot(data=FAOlong, aes(x=Year, y=Catch)) + 
   geom_area() + 
   facet_wrap(~FAO.names) + 
   theme(panel.background = element_rect(fill=NA, 
                                         color="gray50"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()) + 
   scale_x_continuous(expand = c(0, 0)) + 
   scale_y_continuous(expand = c(0, 0))

#now get rid of space between panels
ggplot(data=FAOlong, aes(x=Year, y=Catch)) + 
   geom_area() + 
   facet_wrap(~FAO.names) + 
   theme(panel.background = element_rect(fill=NA, 
                                         color="gray50"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.spacing.x=unit(0, "lines"),
         panel.spacing.y=unit(0, "lines")) + 
   scale_x_continuous(expand = c(0, 0)) + 
   scale_y_continuous(expand = c(0, 0))

#now get rid of gray background for facet labels
#put this first, so that it does not overwrite other bits
ggplot(data=FAOlong, aes(x=Year, y=Catch)) + 
   geom_area() + 
   facet_wrap(~FAO.names) + 
   theme(strip.background = element_rect(fill=NA, color=NA), #label background
         panel.background = element_rect(fill=NA, 
                                         color="gray50"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.spacing.x=unit(0, "lines"),
         panel.spacing.y=unit(0, "lines")) + 
   scale_x_continuous(expand = c(0, 0)) + 
   scale_y_continuous(expand = c(0, 0))

#divide catch by 1 million and change the y label 
ggplot(data=FAOlong, aes(x=Year, y=Catch/1e6)) + 
   geom_area() + 
   facet_wrap(~FAO.names) + 
   theme(strip.background = element_rect(fill=NA, color=NA),
         panel.background = element_rect(fill=NA, 
                                         color="gray50"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.spacing.x=unit(0, "lines"),
         panel.spacing.y=unit(0, "lines")) + 
   scale_x_continuous(expand = c(0, 0)) + 
   scale_y_continuous(expand = c(0, 0)) + 
   ylab("Catch (million metric tons)")

#Fewer labels for years so that they all fit, mess with the colors
ggplot(data=FAOlong, aes(x=Year, y=Catch/1e6)) + 
   geom_area(color="gray50", fill="gray80") + 
   facet_wrap(~FAO.names) + 
   theme(strip.background = element_rect(fill=NA, color=NA),
         panel.background = element_rect(fill=NA, color="gray60"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.spacing.x=unit(0, "lines"),
         panel.spacing.y=unit(0, "lines")) + 
   scale_x_continuous(expand = c(0, 0), 
                      breaks = c(1950, 1970, 1990)) + 
   scale_y_continuous(expand = c(0, 0)) + 
   ylab("Catch (million metric tons)")
#ggsave("figs\\FAOcatches.pdf", width=6, height=6)

#wrap the strip labels across multiple lines
#save the plot to "basicplot"
basicplot <- ggplot(data=FAOlong, aes(x=Year, y=Catch/1e6)) + 
   geom_area(color="gray50", fill="gray80") + 
   facet_wrap(~FAO.names, labeller = label_wrap_gen(width=20)) +  #wrap with width 20
   theme(strip.background = element_rect(fill=NA, color=NA),
         panel.background = element_rect(fill=NA, color="gray60"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.spacing.x=unit(0, "lines"),
         panel.spacing.y=unit(0, "lines")) + 
   scale_x_continuous(expand = c(0, 0), 
                      breaks = c(1950, 1970, 1990)) + 
   scale_y_continuous(expand = c(0, 0)) + 
   ylab("Catch (million metric tons)")
basicplot
#ggsave("figs\\FAOcatches better.pdf", width=7, height=7)

#1. remove the strips altogether just showing the data
#2. expand the upper y by 5%
#3. add labels inside the panels using geom_text()
basicplot <- ggplot(data=FAOlong, aes(x=Year, y=Catch/1e6)) + 
   geom_area(color="gray50", fill="gray80") + 
   facet_wrap(~FAO.names) + 
   theme(strip.background = element_rect(fill=NA, color=NA),
         panel.background = element_rect(fill=NA, 
                                         color="gray60"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.spacing.x=unit(0, "lines"),
         panel.spacing.y=unit(0, "lines"),
         strip.text = element_blank()) +   #remove the strip altogether
   scale_x_continuous(expand = c(0, 0), 
                      breaks = c(1950, 1970, 1990)) + 
   scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + #expand y
   ylab("Catch (million metric tons)")
basicplot

#Now add in the labels at the top. The key is creating a whole new data frame
#that contains the x positions, y positions, and labels to plot
#and then adding as a geom_text plot on top of the other plot
# make a dataframe for just the labels

#new label
label_data <- data.frame(x = 1978, y = 22, 
                         FAO.names = unique(FAOlong$FAO.names))
label_data
basicplot <- ggplot(data=FAOlong, aes(x=Year, y=Catch/1e6)) + 
   geom_area(color="gray50", fill="gray80") + 
   facet_wrap(~FAO.names) + 
   theme(strip.background = element_rect(fill=NA, color=NA),
         panel.background = element_rect(fill=NA, color="gray60"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.spacing.x=unit(0, "lines"),
         panel.spacing.y=unit(0, "lines"),
         strip.text = element_blank()) +  
   scale_x_continuous(expand = c(0, 0), 
                      breaks = c(1950, 1970, 1990)) + 
   scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
   ylab("Catch (million metric tons)") + 
   geom_text(data = label_data, aes(x=x, y=y, label=FAO.names),   #plot text
             color="black", size = 2.5 )
basicplot

#===============Additional useful changes=======================================
#Add a line on top of the bar?
basicplot + geom_line(color="blue")

#one long row of plots? 
basicplot + facet_grid(.~FAO.names)

#one long column of plots? 
basicplot + facet_grid(FAO.names ~ .)

#y axes different for every panel (very common)
basicplot + facet_wrap(~FAO.names, scales="free_y")

#x axes different for every panel (a pretty bad idea generally
#unless you are flipping the axes)
basicplot + facet_wrap(~FAO.names, scales="free_x")

#x and y axes different for every panel
basicplot + facet_wrap(~FAO.names, scales="free")


#=====diamonds!============================================
library(tidyverse)
head(diamonds)  #dataset included in ggplot2

#sample 5000 points at random from the dataset
diamonds5000 <- diamonds[sample(1:nrow(diamonds), size=5000, 
                                replace=FALSE), ]
ggplot(data=diamonds5000, aes(x=carat, y=price)) + 
   geom_point(size=0.5, alpha=0.1) + 
   facet_grid(cut~color)


#the data are more visible without the gray background and gridlines
ggplot(data=diamonds5000, aes(x=carat, y=price)) + 
   geom_point(size=0.5, alpha=0.1) + 
   facet_grid(cut~color) + 
   theme(panel.background = element_rect(fill=NA, color="gray60"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.spacing.x=unit(0, "lines"),
         panel.spacing.y=unit(0, "lines"))

#add color as a function of carat
ggplot(data=diamonds5000, aes(x=carat, y=price, color=carat)) + 
   geom_point(size=0.5, alpha=0.1) + 
   facet_grid(cut~color) + 
   theme(panel.background = element_rect(fill=NA, color="gray60"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.spacing.x=unit(0, "lines"),
         panel.spacing.y=unit(0, "lines"))


#====Code provided for interest================================
#====highlighting one set of lines among many==================
#this code originally from Dan Ovando
library(tidyverse)
library(gapminder)
library(janitor)

#the data
cont_data <- gapminder %>% 
   janitor::clean_names() %>% 
   group_by(continent, year) %>% 
   summarise(gdp_percap = mean(gdp_percap)) %>% 
   ungroup()
cont_data
#create a duplicated version of the data for each continent
background_lines <- tibble(continent = unique(cont_data$continent), 
   data = list(cont_data %>% rename(c2 = continent))) %>% 
   tidyr::unnest(cols = data)

#create a plot of just the background lines
background_plot <- background_lines %>% 
   ggplot() + 
   geom_line(aes(year, gdp_percap,group = c2), color = "lightgrey") + 
   facet_wrap(~continent) + 
   theme_minimal() + 
   theme(panel.grid.minor = element_blank())
background_plot

#create a plot of the highlighted lines
line_plot <- geom_line(data = cont_data,
                       aes(year, gdp_percap, color = continent),
                       show.legend = FALSE, size=1.5)
line_plot

#add the two plots together
background_plot + line_plot + 
   ylab("Gross domestic product per capita") +
   xlab("Year")


#or using the gghighlight package
# https://cran.r-project.org/web/packages/gghighlight/vignettes/gghighlight.html
library(gghighlight)
ggplot(background_lines) +
   geom_line(aes(year, gdp_percap, color = c2), 
             show.legend = FALSE) +
   gghighlight(continent == c2, use_direct_label = FALSE, 
               use_group_by = FALSE) +
   theme_minimal() +
   facet_wrap(~ continent)
