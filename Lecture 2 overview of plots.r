#=============================================================================
#Lecture 2 FISH554: ggplot types of plots, using themes
#Trevor A. Branch tbranch@uw.edu January 2021
#data files needed: 
#    WCcatches.csv: catches of west coast fish species over time
#    BlueLengths.csv: latitude and length of sample of 
#           mature female blue whales in Southern Hemisphere
#=============================================================================

#libraries required here
library(tidyverse)  #includes packages readr and ggplot2
library(hexbin)
library(here)

#data files used: WCcatches.csv

#get some data to look at
WC <- read_csv(file=here("Documents","data","beautiful_graphics","WCcatches.csv"))   #read_csv creates tibble
print(WC)
#side note that these data are in wide format not long format

#======simple plots with year vs. catches
#area plots
ggplot(data=WC, aes(x=Year, y=Sablefish)) + 
    geom_area()

#bar plots
ggplot(data=WC, aes(x=Year, y=Sablefish)) + 
    geom_col()

#line plots
ggplot(data=WC, aes(x=Year, y=Sablefish)) + 
    geom_line()

#point plots
ggplot(data=WC, aes(x=Year, y=Sablefish)) + 
    geom_point()

#step plots (skyscraper plots)
ggplot(data=WC, aes(x=Year, y=Sablefish)) + 
    geom_step()

#to plot one of the columns with spaces in the title, use the `` symbol
#(top left on keyboards)
ggplot(data=WC, aes(x=Year, y=`Bocaccio rockfish`)) + 
    geom_step()

#=======changing the order of data=======
#reordering ensures that both sets of data are reordered
WC <- read_csv(file="WCcatches.csv")   #read_csv is in readr, creates tibble
WC1990s <- filter(WC, Year >= 1990)    #get just data since 1990
ggplot(data=WC1990s, aes(x=Year, y=Sablefish)) + 
    geom_col()

#order by sablefish catches
ggplot(data=WC1990s, aes(x=reorder(Year, Sablefish), y=Sablefish)) + 
    geom_col(width=0.9)

#order by reverse year
ggplot(data=WC1990s, aes(x=reorder(Year, rev(Year)), y=Sablefish)) + 
    geom_col()

#==========Frequencies and histograms=======================================
#create 10000 random numbers from a normal distribution
#tinker with the mean and sd values to see how the plots change
#change data into tibble (data frame) format
normdata <- as_tibble(x=rnorm(n=10000, mean=55, sd=10))
print(normdata) #note that as.tibble() automatically called the data "value"

#histogram
ggplot(data=normdata, aes(x=value)) + 
    geom_histogram()
ggplot(data=normdata, aes(x=value)) + 
    geom_histogram(binwidth=1)  #change default binwidth
ggplot(data=normdata, aes(x=value)) + 
    geom_histogram(binwidth=5)

#polygon of frequency
ggplot(data=normdata, aes(x=value)) + 
    geom_freqpoly(binwidth=1)

#areaplot with bins
ggplot(data=normdata, aes(x=value)) + 
    geom_area(stat="bin", binwidth=1)

#smoothed curve fitted to frequencies
ggplot(data=normdata, aes(x=value)) + 
    geom_density()

ggplot(data=normdata, aes(x=value)) + 
    geom_density(kernel="gaussian")

#bar plot of discrete values (round to whole numbers)
normintegers <- round(normdata,0) #round values to whole numbers
print(normintegers)
ggplot(data=normintegers, aes(x=value)) + 
    geom_bar()

#============points, jitter, heatmap
#Blue whale catches (year, latitude, length from 30-80E, 25S to 80S, 1950-1973)
library(tidyverse)  #includes packages readr and ggplot2
bluedata <- read_csv(file=here("Documents","data","beautiful_graphics","BlueLengths.csv"))
print(bluedata)

#point plots: do you see any interesting patterns? 
ggplot(data=bluedata, aes(x=Lat, y=LenFt)) +
    geom_point()

#jitter plot
#for overlapping points, add a little bit of "jitter", also helps with 
#rounded data, and here many latitudes; lengths rounded to whole numbers
ggplot(data=bluedata, aes(x=Lat, y=LenFt)) +
    geom_jitter(width=0.2, height=0.2)

ggplot(data=bluedata, aes(x=Lat, y=LenFt)) +
    geom_jitter(width=0.5, height=0.5)  #greater jitter amount

#add a smooth fitted line using locally estimated smoothers
ggplot(data=bluedata, aes(x=Lat, y=LenFt)) +
    geom_jitter(width=0.5, height=0.5) +  
    geom_smooth(method="loess", span=0.5)  #add a local smoother

#heatmap / 2D bin
ggplot(data=bluedata, aes(x=Lat, y=LenFt)) +
    geom_bin2d()
ggplot(data=bluedata, aes(x=Lat, y=LenFt)) +
    geom_bin2d(binwidth=c(1,1)) #with much smaller bins

#hex plot
library(hexbin)
ggplot(data=bluedata, aes(x=Lat, y=LenFt)) +
    geom_hex(binwidth=c(1,1))

#contour plot of two normal distributions
twonorms <- as_tibble(cbind(x=rnorm(10000), y=rnorm(10000)))
ggplot(data=twonorms, aes(x=x, y=y)) +
    geom_density2d()

#===============Making prettier plots: barplots=================================
library(tidyverse)
WC <- read_csv(file="WCcatches.csv")   #read_csv is in readr, creates tibble
ggplot(data=WC, aes(x=Year, y=Sablefish)) + 
    geom_col() +
    xlab("Year") +   #add appropriate labels for axes
    ylab("Sablefish catches (metric tons)") +
    theme_classic()  #change the default theme of the plot

ggplot(data=WC, aes(x=Year, y=Sablefish)) + 
    geom_col() +
    xlab("Year") +
    ylab("Sablefish catches (metric tons)") +
    theme_minimal() +
    scale_x_continuous(expand = c(0, 0)) + #no expansion below or above min/max
    scale_y_continuous(expand = c(0, 0))  #no expansion below or above min/max

#======coordinate systems: flip, equal, log-transform etc.==========
LW <- read_csv(file=here("Documents","data","beautiful_graphics","LengthWeight.csv"))
print(LW)
ggplot(data=LW, aes(x=LengthCM, y=WeightKG)) + 
    theme_classic() + 
    geom_point()

#x and y axes have equal sizes between units
#since coord_fixed does not make sense for these data
#I first multiplied weight by 20
ggplot(data=LW, aes(x=LengthCM, y=WeightKG*20)) + 
    theme_classic() + 
    geom_point() + coord_fixed() 

#flip axes, so x data vertical and y data horizontal 
ggplot(data=LW, aes(x=LengthCM, y=WeightKG)) + theme_classic() + 
    geom_point() + coord_flip() 

#log-x and log-y (natural logarithm)
ggplot(data=LW, aes(x=LengthCM, y=WeightKG)) + theme_classic() + 
    geom_point() + 
    coord_trans(x="log", y="log")

#===barplots with an error bar=======
WC <- read_csv(file="WCcatches.csv")   
ggplot(data=WC, aes(x=Year, y=Sablefish)) + 
    geom_col() + xlab("Year") + 
    ylab("Sablefish catches (metric tons)") +
    theme_minimal() +
    #add error bar (20% below, 20% above)
    geom_errorbar(aes(ymax=Sablefish*1.2, ymin=Sablefish*0.8)) +
    scale_x_continuous(expand = c(0, 0))+ #no expansion below or above min/max
    scale_y_continuous(expand = c(0, 0))  #no expansion below or above min/max
    
ggplot(data=WC, aes(x=Year, y=Sablefish)) + 
    geom_col() + xlab("Year") + ylab("Sablefish catches (metric tons)") +
    theme_minimal() +
    #only the upper error bar, arbitrarily 20% higher
    geom_errorbar(aes(ymax=Sablefish*1.2, ymin=Sablefish)) +
    scale_x_continuous(expand = c(0, 0))+ #no expansion below or above min/max
    scale_y_continuous(expand = c(0, 0)) #no expansion below or above min/max

#====points with an error line around them (no error bar)=======================
#geom_pointrange() is more elegant than bar plots for many 
#uses, and less data ink than a formal error 
#bar that has horizontal lines above and below.
ggplot(data=WC, aes(x=Year, y=Sablefish)) + 
    geom_pointrange(aes(ymax=Sablefish*1.2, ymin=Sablefish*0.8)) + 
    xlab("Year") + ylab("Sablefish catches (metric tons)") +
    theme_minimal() +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) #no expansion below or above min/max

ggplot(data=WC, aes(x=Year, y=Sablefish)) + 
    geom_pointrange(aes(ymax=Sablefish*1.2, ymin=Sablefish*0.8)) + 
    xlab("Year") + ylab("Sablefish catches (metric tons)") +
    theme_minimal() +
    scale_x_continuous(expand = c(0.01, 0.01))+ #add 1% expansion for points
    scale_y_continuous(expand = c(0, 0)) + 
    #how to set limits on the x axis or y axis (does not clip values outside lims)
    coord_cartesian(ylim=c(0,33000)) 
    #ylim(0,33000) + xlim(1950,2005)  #another way of setting limits

#=============adding a fitted trendline to a plot of points=====================
x <- sort(runif(20,1,10)) #create some random data
correl.data <- as_tibble(cbind(x = x,   #generate 20 values from a uniform between 1 and 10
                         y = x + rnorm(20)))         #add some normal random numbers to the x values
print(correl.data)
ggplot(data=correl.data, aes(x=x, y=y)) +
    geom_point() + theme_minimal() + 
    geom_smooth(method="lm", level=0.95) #add a linear prediction 95% CI

#========point sizes, symbols, colors, transparency=====
#make the points bigger
correl.data <- as_tibble(cbind(x = x,   #generate 20 values from a uniform between 1 and 10
                               y = x + rnorm(20)))         #add some normal random numbers to the x values
ggplot(data=correl.data, aes(x=x, y=y)) +
    geom_point(size=1) + theme_minimal() 
ggplot(data=correl.data, aes(x=x, y=y)) +
    geom_point(size=3) + theme_minimal() 
ggplot(data=correl.data, aes(x=x, y=y)) +
    geom_point(size=1:20) + theme_minimal()  #different size for each point

#different shapes
ggplot(data=correl.data, aes(x=x, y=y)) +
    geom_point(shape=1:20, size=5) + theme_minimal() 

#can specify anything as the shape, but "." is one pixel
#here is an enormous dataset
twonorms <- as_tibble(cbind(x=rnorm(100000), y=rnorm(100000)))

#might be slow to plot since 100,000 data points
ggplot(data=twonorms, aes(x=x, y=y)) +
    geom_point(shape=".") + theme_minimal()

#different colors (more on colors in the color lecture)
ggplot(data=correl.data, aes(x=x, y=y)) +
    geom_point(color="red", size=5) + theme_minimal() 
ggplot(data=correl.data, aes(x=x, y=y)) +
    geom_point(color=c(rep("red",10), rep("blue",10)), size=5) + 
    theme_minimal() 

#rainbow() is a built-in palette that returns a suits of colors
#later I will explain why rainbow palettes are very bad... 
ggplot(data=correl.data, aes(x=x, y=y)) +
    geom_point(color=rainbow(20), size=5) + 
    theme_minimal() 

#shapes 21:25 have a fill color but the others do not
ggplot(data=correl.data, aes(x=x, y=y)) +
    geom_point(color="black", size=5, fill="green", shape=21) + 
    theme_minimal() 
ggplot(data=correl.data, aes(x=x, y=y)) +
    geom_point(color="black", size=5, fill="green", shape=19) + 
    theme_minimal() 

#transparency in color, applies to both outline and fill
#seq = give 20 values between 0 (transparent) and 1 (opaque)
ggplot(data=correl.data, aes(x=x, y=y)) +
    geom_point(color="black", size=5, fill="green", shape=21, 
               alpha=seq(0,1,length.out=20)) + 
    theme_minimal() 

#========line width, type===================================
#different line types: 1=solid, 2=dashed, 3=dot, 4=dot-dash
#generally looks terrible to distinguish between lines with 
#different line types, so this should be avoided
WC <- read_csv(file="WCcatches.csv")  
ggplot(data=WC, aes(x=Year, y=Sablefish)) + theme_minimal() + 
    geom_line(linetype=1) +
    xlab("Year") + ylab("Sablefish catches (metric tons)")
ggplot(data=WC, aes(x=Year, y=Sablefish)) + theme_minimal() + 
    geom_line(linetype=2) +
    xlab("Year") + ylab("Sablefish catches (metric tons)")
ggplot(data=WC, aes(x=Year, y=Sablefish)) + theme_minimal() + 
    geom_line(linetype=3) +
    xlab("Year") + ylab("Sablefish catches (metric tons)")
ggplot(data=WC, aes(x=Year, y=Sablefish)) + theme_minimal() + 
    geom_line(linetype=4) +
    xlab("Year") + ylab("Sablefish catches (metric tons)")

#line widths
ggplot(data=WC, aes(x=Year, y=Sablefish)) + theme_minimal() + 
    geom_line(linetype=1, size=3) +
    xlab("Year") + ylab("Sablefish catches (metric tons)")
ggplot(data=WC, aes(x=Year, y=Sablefish)) + theme_minimal() + 
    geom_line(linetype=1, size=1:56/14) +
    xlab("Year") + ylab("Sablefish catches (metric tons)")

#========adding vertical/horizontal lines
ggplot(data=WC, aes(x=Year, y=Sablefish)) + theme_minimal() + 
    geom_line(linetype=1, size=1:56/14) +
    xlab("Year") + ylab("Sablefish catches (metric tons)") +
    geom_hline(yintercept=10000, linetype="dashed", 
               color = "gray50", size=1)
ggplot(data=WC, aes(x=Year, y=Sablefish)) + 
    theme_minimal() + 
    geom_line(linetype=1, size=1:56/14) +
    xlab("Year") + ylab("Sablefish catches (metric tons)") +
    geom_vline(xintercept=1974, linetype="dashed", 
               color = "gray50", size=1)

#there is also geom_ribbon, geom_polygon, geom_segment, 
#geom_rect for other ways of adding objects to plots


#========EXERCISES===============================================
#**DO EXERCISE 1: accident data from xkcd cartoon
#Try to get ordered data with horizontal barplots
acc <- read_csv("Accidents.csv")     #read in data

#**DO EXERCISE 2: Old Faithful eruptions at Yellowstone N.P.
#explore different plotting types to see which represents the data the best
OldF <- read_csv("OldFaithfulDuration.csv")

#**CHALLENGE EXERCISE 3: university salaries
#how to represent data that goes outside the bounds of the plot? 
sal <- read.csv("UnivSalaries.csv")
