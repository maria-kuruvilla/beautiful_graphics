####################################
#Inserting images, text annotations, mathematical expressions
#legends, custom axes
#Trevor Branch, FISH 554
####################################

#===Inserting photos into plots=======================================
#create a random plot and add a photo. 
#Pic is lilac-breasted roller, by Trevor Branch, 2019, Kruger N.P.
#Find free silhouettes of virtually any animal from phylopic.org
library(ggplot2)
library(cowplot)
library(magick)

p <- ggplot(mtcars, aes(disp, mpg)) +
   geom_point(size = 1.5, color = 'blue') +
   theme_cowplot(font_size=12)
p
ggdraw(p) + 
   draw_image(image='bird.png', x = 0.55, y = 0.55, 
              width=0.4, height=0.4)
#ggsave('Figs\\Lilacbreasted.pdf', width=7, height=4.5)


#=====ANNOTATING POINTS WITH TEXT=====================================
library(tidyverse)
#create random dataset with labels from built-in list of US states
some.data <- tibble(x=5:14, y=x+rnorm(10), z=y^2, 
                    label=state.name[1:10])
some.data

#make a simple plot
ggplot(data=some.data, aes(x=x, y=y)) + 
   geom_point(size=4) + 
   theme_minimal()

#replace the points with the labels, note label added to aes
ggplot(data=some.data, aes(x=x, y=y, label=label)) + 
   theme_minimal() + 
   geom_text()

#points in red, text in light gray
#move points up and right, ensure that they are left justified
ggplot(data=some.data, aes(x=x, y=y, label=label)) + 
   geom_point(size=4, color='red') + theme_minimal() + 
   geom_text(hjust=0, nudge_x=0.2, nudge_y=0.02, 
             size=4, color='gray50')

#overlapping text
ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) + 
   geom_point(size=2, color='red') + theme_minimal() + 
   geom_text(vjust=0, nudge_x=0, nudge_y=0.4)

#check_overlap removes labels that overlap
ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) + 
   geom_point(size=2, color='red') + theme_minimal() + 
   geom_text(vjust=0, nudge_x=0, nudge_y=0.4, 
             check_overlap=TRUE, color='gray50')

#much better solution from the ggrepel library that does a great job 
#at keeping all the labels visible and moving the labels around so 
#they can all be seen, and don't disappear off the side of the plot
library(ggrepel)
ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) + 
   geom_point(size=2, color='red') + theme_minimal() + 
   geom_text_repel(color='gray60')

#additional notes: 
#Use geom_label() instead of geom_text() for labels with background colors
#hjust='inward' puts labels on the inner side of the plot    

#====ADDING CUSTOM TEXT ANYWHERE ON PLOT==============================
ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) + 
   geom_point(size=2, color='red') + theme_minimal() + 
   geom_text(vjust=0, nudge_x=0, nudge_y=0.4, check_overlap=TRUE) + 
   annotate(geom='text', x=4, y=33.75, 
            label='Plot of weight vs. mpg',
            size=7, color='blue')

#can change any text to 'bold' or 'italic' using fontface
ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) + 
   geom_point(size=2, color='red') + theme_minimal() + 
   geom_text(vjust=0, nudge_x=0, nudge_y=0.4, check_overlap=TRUE) + 
   annotate(geom='text', x=4, y=33.75, label='Plot of weight vs. mpg',
            size=7, color='blue', fontface='italic')

#adding text in common places using labs()
ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) + 
   geom_point(size=2, color='gray20') + theme_minimal() + 
   geom_text(vjust=0, nudge_x=0, nudge_y=0.4, check_overlap=TRUE, 
             color='gray40') + 
   labs(x='x label', y='y label',
        title='Title', subtitle='Subtitle', caption='Caption',
        tag='Tag')
   
#Change colors etc. using theme(). Title changes all, there is also 
#plot.title, axis.title, axis.title.x, axis.title.y, plot.caption,
#plot.subtitle, and plot.tag to change each element.
ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) + 
   geom_point(size=2, color='gray20') + theme_minimal() + 
   geom_text(vjust=0, nudge_x=0, nudge_y=0.4, check_overlap=TRUE, 
             color='gray40') + 
   theme(plot.caption=element_text(color='red', size=10)) +
   labs(x='Weight of car', y='Miles per gallon',
        title='Plot of weight vs. miles per gallon', 
        subtitle='From the mtcars dataset', 
        caption='Produced for FISH554',
        tag='a)')

#=====MATHEMATICAL EXPRESSIONS========================================
#for adding degree symbols etc. 
library(graphics)
demo(graphics::plotmath)  

#the \u2030 refers to unicode characters in hex numbers
#for a full list see: https://unicode-table.com/en/#greek-extended 
library(ggplot2)
ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) + 
   geom_point() + theme_minimal() + 
   labs(x=expression('Concentration of'~CO[2]),
        y=expression('Mean catch'~(10^6 ~'t')),
        title=expression('Carbonate'~CO[3]^'2-'), 
        subtitle=expression('Calcium ion'~Ca^'2+'), 
        caption=expression('Temperature'~(degree*C)~'in 2003'),
        tag=expression(paste(delta^{13}, 'C (\u2030)')))

#more complicated mathematical equations
ggplot(mtcars, aes(wt, mpg)) + geom_point(size=2) + theme_minimal() + 
   annotate(geom='text', x=3, y=33.75, size=6,
            label=expression(bar(x)==sum(frac(x[i],n),i==1,n))) + 
   annotate(geom='text', x=3, y=31, size=6,
            label=expression(hat(beta)==(X^t*X)^{-1}*X^t*y)) +
   annotate(geom='text', x=3, y=28, size=6,
         label=expression(z[i]==sqrt(x[i]^2+y[i]^2)))

#then add axes labels with degree symbols for temperature and longitude
#can use degree, minute and second for latitudes and longitudes
ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_minimal() + 
   scale_x_continuous(breaks=seq(2,5,1),  #where to place the values
                      labels=c(expression(2*degree*E), 
                               expression(3*degree*E),
                               expression(4*degree*E), 
                               expression(5*degree*E))) +
   scale_y_continuous(breaks=seq(10, 30, 10),
                      labels=c(expression(10*degree*C), 
                               expression(20*degree*C),
                               expression(30*degree*C))) + 
   labs(x='Longitude', y='Temperature', title='Not actual data')

#If you can find the degree symbol and paste it into R this 
#works usually, depending on how text is encoded in your editor
#alt-numeric-key-248 in Windows and shift-command-8 on Macs or 
#insert-symbol in MS Word
tickplace <- seq(from=2, to=5, by=1)  #vector of numbers from 2 to 5
ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_minimal() + 
   scale_x_continuous(breaks=tickplace,  #where to place the values
                      labels=paste0(tickplace,'°C'))


#====LOGARITHMIC AXES=================================================
#Don't plot the log of the value on the tickmarks. Instead, add labels
#1,2,5,10,20,50,100, etc. at the place of log(1), log(2), log(5), 
#etc. Or possibly 10^0, 10^1, 10^2 at log(1), log(10), log(100) etc.

#create a random dataset using the lognormal distribution
#that has some extreme large and small values
set.seed(104) #for repeatable plot
lognormaldata <- tibble(x=1:100, y=rlnorm(n=100, meanlog=3, sdlog=2))
lognormaldata

#clearly not the right way of plotting
ggplot(data=lognormaldata, aes(x=x, y=y)) + geom_point() +
   theme_minimal()

#change to log scale on y axis, but I don't like scientific notation
#with the e-01 etc. 
ggplot(data=lognormaldata, aes(x=x, y=y)) + geom_point() +
   theme_minimal() + 
   scale_y_log10()

#write out values on the tick marks instead
ggplot(data=lognormaldata, aes(x=x, y=y)) + geom_point() +
   theme_minimal() + 
   scale_y_log10(breaks=c(0.1,1,10,100,1000),
                 labels=c(0.1,1,10,100,1000))

#or use powers of 10
ggplot(data=lognormaldata, aes(x=x, y=y)) + geom_point() +
   theme_minimal() + 
   scale_y_log10(breaks=c(0.1,1,10,100,1000),
                 labels=c(expression(10^-1),expression(10^0),
                          expression(10^1),expression(10^2),
                          expression(10^3)))

#===LEGENDS===========================================================
#For more, see http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
#ggplot automatically adds legends
plotGrowth <- ggplot(data=PlantGrowth, 
                     aes(x=group, y=weight, fill=group)) +
   geom_boxplot() + theme_minimal()
plotGrowth

#remove all legend titles
plotGrowth + theme(legend.title=element_blank())

#remove all legends
plotGrowth + theme(legend.position='none')

#move legend to the top of the figure
plotGrowth + theme(legend.position='top')

# Position legend in graph, where x,y is 0,0 (left bottom) to 1,1 
#(right top) of the entire plot including captions and labels
plotGrowth + theme(legend.position=c(0.8, 0.2))

#change the names in the legends. The \n means go to new line
plotGrowth + scale_fill_discrete(name='Experimental\ncondition',
                         breaks=c('ctrl', 'trt1', 'trt2'),
                         labels=c('Control', 'Treatment 1', 'Treatment 2'))

#change the default colors: manual means manually setting stuff
#the colors are specified as hex colors
#Note: all scales are in the form of scale_xxx_yyy where 
#xxx could be color, fill, alpha, linetype, shape, or size
#yyy could be hue, manual, gradient, gray, discrete, or continuous
plotGrowth + scale_fill_manual(values=c('#999999', '#E69F00', 
                                        '#56B4E9'))


#=======ELEGANT SOLUTION TO LATITUDE AND LONGITUDE======
#Slick function from Ben Bolker on how to automatically get 
#latitude and longitude working properly for any plot.
#The key is creating functions that will take the axis label numbers
#and convert them into appropriate labels depending on whether
#they are latitude or longitude. 
#https://gist.github.com/bbolker/5ba6a37d64b06a176e320b2b696b6733
#=======================================================
library(ggplot2)

long_label <- function(x) {
   s <- sprintf('%d*degree*%s',
                x,ifelse(x==0,'',
                         ifelse(x<0,'W','E')))
   return(parse(text=s))
}

lat_label <- function(x) {
   s <- sprintf('%d*degree*%s',
                x,ifelse(x==0,'',
                         ifelse(x<0,'S','N')))
   return(parse(text=s))
}
ggplot(mtcars,aes(wt,mpg)) + geom_point()

#note the passing of a function to transform the default labels into 
#longitude and latitude
ggplot(mtcars,aes(wt,mpg)) + geom_point() +
   scale_x_continuous(labels=long_label) +
   scale_y_continuous(labels=lat_label) +
   labs(x='Longitude',y='Latitude')
