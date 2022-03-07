#=============ADVANCED MULTIPANEL PLOTS============================
#Fish 554, Beautiful Graphics in R.
#Advanced multipanel plots: grouping different plots together,
#arranging them, inserting small subplots into bigger plots.
#Using patchwork and cowplot.
#By Trevor A. Branch tbranch@uw.edu 
#Cowplot code borrows from vignettes by Claus Wilke: 
#https://wilkelab.org/cowplot/articles/index.html
#==================================================================

library(tidyverse)
library(cowplot)
library(gridExtra)
library(patchwork)
library(png)

#======================
#=======PATCHWORK======
#aligns all the axes nicely, has clean simple syntax
#======================
library(patchwork)
library(ggplot2)

p1 <- ggplot(mtcars, aes(disp, mpg)) + geom_point()
p2 <- ggplot(mtcars, aes(qsec, mpg)) + geom_point()
p3 <- ggplot(mtcars, aes(x = qsec, y = disp)) + 
   geom_point() + facet_wrap(~gear)

p1+p2  #+ means add to the plots
p1/p2  #/ means add a plot below
(p1+p2)/p3  #complex expressions allow complex plots
(p1|p2)/p3  #complex expressions allow complex plots

#more complicated designs are possible using text strings
#to denote where each plot will be, with each letter denoting 
#which space one of the plots will fill
layout_string <- 'ABB
                  CCD'
p1+p2+p3+p1 + plot_layout(design=layout_string)

#use # for spaces with missing plots
layout_string <- 'A#BB
                  CCC#'
p1 + p2 + p3 + plot_layout(design=layout_string)

#also choose widths and heights allocated to each row and column of plots
p1 + p2 + p3 + 
   plot_layout(design=layout_string, 
               widths=c(3,1,5,6),
               heights=c(5,3))

#for inset plots, use the inset_element() call
#experiment with align_to to determine relative placement
p1 + inset_element(p2, 0.6, 0.6, 1, 1,  
                   align_to='full')
p1 + inset_element(p2, 0.6, 0.6, 1, 1,  
                   align_to='panel')
p1 + inset_element(p2, 0.6, 0.6, 1, 1,  
                   align_to='plot')

#you can also use inset_element to insert shapes
library(grid)
p1 + inset_element(circleGrob(), 0.4, 0.4, 0.6, 0.6)

#and even to insert pictures
library(png)
puppy <- readPNG(here("Documents","data","beautiful_graphics" ,"puppy.png"), native=TRUE) 
p1 + inset_element(puppy, 0.5, 0.5, 0.74, 0.9, align_to = 'full')

#To add an x axis label for those suite of plots, use the 
#plot_annotation() command, e.g. below
p1 + p2 +
   plot_annotation(caption = "x-axis title for whole figure", 
   theme = theme(plot.caption = element_text(size=12,
                 hjust = 0.5, vjust = 1)))
p1/p2  +  plot_annotation(caption = "x-axis title for whole figure", 
                          theme = theme(plot.caption = element_text(size=12,
                                                                    hjust = 0.5, vjust = 1)))

#for joint y-axis here is a kludge: make the y-label of one plot
#lower using hjust, then remove the y-label for the other plot
#the blank background for the second plot ensures it does not overlap
library(patchwork)
p5 <- p1 + theme(axis.title.y=element_text(hjust=-0.4))
p6 <- p2 + labs(y='') + theme(plot.background=element_blank())
p5/p6

#====================
#=======COWPLOT======
#====================
library(tidyverse)
library(cowplot)

#a variety of publication-ready themes
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
   geom_point() + 
   theme_cowplot(font_size=12)

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
   geom_point() + 
   theme_minimal_grid(12)

#===combining two plots into one==============================
#arranging two plots into one
p1 <- ggplot(mtcars, aes(disp, mpg)) +
   geom_point() + theme_cowplot(font_size=12)
p2 <- ggplot(mtcars, aes(qsec, hp)) +
   geom_point() + theme_cowplot(font_size=12)
#two plots
plot_grid(p1, p2, labels = c('A', 'B'))

#four plots
plot_grid(p1, p2, p1, p2, labels = LETTERS[1:4])

#tweaking plot
plot_grid(p1, p2, labels = c('A', 'B'), 
   label_fontface='plain',  #1=plain, 2=bold, 3=italic, 4=bold-italic
   label_size = 12,  #size of label text
   nrow=2) #specify the number of rows

#========aligning plots with different widths and heights=====
#align plots: v=vertical, h=horizontal, vh=align both
#align the axes: b=bottom, t=top, bt=top and bottom, 
#                     l=left, r=right, btlr = all sides
plot_grid(p1, p2, nrow=2, align = "vh", axis="bt") 

#======making one plot bigger than another============================
#use the rel_widths() and rel_heights() to change plot sizes
#labels = "AUTO" adds A, B, ... while "auto" adds a, b, ...
plot_grid(p1, p2, labels = "AUTO", rel_widths = c(1, 2))

#=======missing plots=================================================
#to signify missing data, one plot could be missing
plot_grid(p1, NULL, NULL, p2,
          labels = c("A","","","B"), ncol = 2)

#========more complex arrangements of plots===========================
#can create a row of plots using plot_grid, and then 
#add another plot as an entire row to itself
p1 <- ggplot(mtcars, aes(disp, mpg)) + geom_point()
p2 <- ggplot(mtcars, aes(qsec, mpg)) + geom_point()
p3 <- ggplot(mtcars, aes(x = qsec, y = disp)) + 
   geom_point() + facet_wrap(~gear)

bottom_row <- plot_grid(p1, p2, labels = c('B', 'C'), label_size = 12)
plot_grid(p3, bottom_row, labels = c('A', ''), label_size = 12, ncol = 1)

#===adding a background "DRAFT" watermark to plot=====================
p <- ggplot(mpg, aes(displ, cty)) + geom_point() + 
   theme_half_open()   #need empty background color
ggdraw() + 
   draw_label("Draft", color = "gray50", size = 100, angle = 45, alpha=0.3) +
   draw_plot(p)

#===adding a second plot as an inset to the first plot================
inset <- ggplot(mpg, aes(drv)) + 
   geom_bar(fill = "skyblue2", alpha = 0.7) + 
   scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
   theme_minimal_hgrid(font_size=11)

ggdraw(p) +
   #specify the lower-left corner of the plot and how big it will be
   draw_plot(inset, x=0.45, y=0.45, width=0.5, height=0.5) +
   #specify where the labels for the two plots will be 
   draw_plot_label(label=c("A", "B"), 
                   x=c(0, 0.45), y=c(1, 0.95), size=12)





#===Just for interest===================
#=======gridExtra=======================
#=======================================
#For more complex arrangements, see the vignette at
#https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html

library(gridExtra)
library(grid)
library(ggplot2)
#library(lattice)

p1 <- ggplot(mtcars, aes(disp, mpg)) + geom_point()
p2 <- ggplot(mtcars, aes(disp, mpg)) + geom_point() +
   facet_wrap(~gear)
p3 <- ggplot(mtcars, aes(qsec, mpg)) + geom_point()
p4 <- ggplot(mtcars, aes(qsec, mpg)) + geom_point() +
   facet_wrap(~gear)

grid.arrange(p1, p2, p3, p4)

#create a matrix of numbers, each number must be in a rectangle
#shape. 1=plot 1, 2=plot 2, etc. Add plots to a list. 
gs <- list(p1,p2,p3,p4)
layout_str <- rbind(c(1,1,2,2,2),
                    c(3,3,4,4,4))
grid.arrange(grobs=gs, layout_matrix=layout_str)

#adding NA to a location creates a hole
layout_str <- rbind(c(1,1,NA,2,2,2),
                    c(3,3,NA,4,4,4))
grid.arrange(grobs=gs, layout_matrix=layout_str)

