#Goal - produce one figure with multiple panels to plot average number of wild 
# and hatchery salmon out migrating as a function of time of year. One sub figure for
# river and species.

library(RODBC)
library(here)
library(tidyverse)
library(tidyquant)
library(RColorBrewer)
library(patchwork)
library(magick)
library(cowplot)

access_file <- function(file_name){
  out <- tryCatch(
    {
      # Just to highlight: if you want to use more than one 
      # R expression in the "try" part then you'll have to 
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression 
      # in case the "try" part was completed successfully
      
      message("This is the 'try' part")
      
      channel <- odbcConnectAccess2007(here("Documents","data","pied_piper",
                                            file_name))
      df <- sqlFetch(channel, 'qry_AllCatch')
      # The return value of `readLines()` is the actual value 
      # that will be returned in case there is no condition 
      # (e.g. warning or error). 
      # You don't need to state the return value via `return()` as code 
      # in the "try" part is not wrapped inside a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error=function(cond) {
      message(paste("File does not exist"),file_name)
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    },
    warning=function(cond) {
      message("Opening access file caused a warning:",file_name)
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    },
    finally={
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you 
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>' 
      #message(paste("File successfully opened:", url))
      message("File successfully opened",file_name)
    }
  )    
  return(out)
  odbcClose(channel)
}

extract_species_data_error <- function(year, species,type,river,file_type){
  if(species == "Steelhead"){
    data_filter = paste(species,"smolt",type)
    file = paste0(river,file_type)
    string_year = paste(toString(year),file)
    print(string_year)
    year0 = year-1
    df <- access_file(string_year)
    df$StartDate_OG <- df$StartDate
    df_species_all_years <- df[((df$WSPEName==data_filter)
                                & df$CaptureType==1),]
    df_species_all_years$StartDate_OG <- df_species_all_years$StartDate
  }
  else {
    data_filter0 = paste(species,"0+",type)
    data_filter = paste(species,"1+",type)
    file = paste0(river,file_type)
    year0 = year-1
    string_year = paste(toString(year),file)
    string_year0 = paste(toString(year0),file)
    print(string_year)
    print(string_year0)
    
    df <- access_file(string_year)
    #print(unique(df$WSPEName))
    df_species <- df[((df$WSPEName==data_filter)
                      & df$CaptureType==1),]
    df_species$StartDate_OG <- df_species$StartDate
    
    
    df0 <- access_file(string_year0)
    df_species0 <- df0[((df0$WSPEName==data_filter0)
                        & df0$CaptureType==1),]
    df_species0$StartDate_OG <- df_species0$StartDate
    
    
    df_species_all_years <- rbind(df_species0,df_species)
  }
  
  df_species_all_years$day_of_year <- format(as.POSIXct(df_species_all_years$StartDate_OG), format = "%m/%d")
  df_species_all_years$year <- format(as.POSIXct(df_species_all_years$StartDate_OG), format = "%Y")
  df_species_all_years$start_datetime <- as.POSIXct(paste(df_species_all_years$StartDate_OG, 
                                                          format(df_species_all_years$StartTime, format = "%H:%M:%S")))
  
  df_species_all_years$start_time <- format(as.POSIXct(df_species_all_years$start_datetime), format = "%H:%M:%S")
  
  
  df_species_all_years$end_datetime <- as.POSIXct(paste(df_species_all_years$EndDate, 
                                                        format(df_species_all_years$EndTime, format = "%H:%M:%S")))
  
  df_species_all_years$end_time <- format(as.POSIXct(df_species_all_years$end_datetime), format = "%H:%M:%S")
  
  df_species_all_years$fish_per_hour <- (df_species_all_years$NumCaught
                                         /as.numeric(difftime(df_species_all_years$end_datetime,
                                                              df_species_all_years$start_datetime, units = "hours")))
  
  year(df_species_all_years$StartDate[year(df_species_all_years$StartDate) == year0]) <- 2000
  year(df_species_all_years$StartDate[year(df_species_all_years$StartDate) == year]) <- 2001
  return(df_species_all_years)
  
  
}

sum_species_data <- function(year, species,type,river,file_type){
  df <- extract_species_data_error(year, species,type,river,file_type)
  df_new_summarize <- df %>%
    group_by(WSPEName, StartDate, day_of_year, year) %>%
    summarise(sum_num_caught = sum(fish_per_hour))
  return(df_new_summarize)
}

moving_average <- function(species,type,river,file_type){
  if(species == "Steelhead"){
    last_year = 2020
  }
  else {
    last_year = 2021
  }
  for(i in 2016:last_year){
    if(i==2016){
      df_combine <- sum_species_data(i, species,type,river,file_type)
    }
    else {
      df <- sum_species_data(i, species,type,river,file_type)
      df_combine <- rbind(df_combine,df)
    }
    
  }
  my_data_frame <- df_combine %>%
    group_by(StartDate) %>%
    summarise(avg_num_caught = mean(sum_num_caught), sd = sd(sum_num_caught))
  f21 <- rep(1/10,10)
  my_data_frame$moving_average <- stats::filter(my_data_frame$avg_num_caught,f21,  sides = 2)
  my_data_frame$moving_average[is.na(my_data_frame$moving_average)] <- 
    my_data_frame$avg_num_caught[is.na(my_data_frame$moving_average)]
  return(my_data_frame)
}

plot_all_years <- function(plot_empty, species,type,river,file_type){
  if(type == "H"){
    color_brewer = "#66c2a5"
  }
  else {
    color_brewer = "#fc8d62"
  }
  if(species == "Steelhead"){
    last_year = 2020
  }
  else {
    last_year = 2021
  }
  for(i in 2016:2021){
    df <- sum_species_data(i, species,type,river,file_type)
    plot_empty <- plot_empty + 
      geom_line(df, mapping = aes(StartDate, sum_num_caught), alpha = 0.3, color = color_brewer)
  }
  return(plot_empty)
}

# try (p1 + p2 + p3)/p4 + p5 + p6)

species = "Chinook"
type = "W"
river = "Dungeness"
ma_w_chinook <- moving_average(species,type,river, ".accdb")
p1_w <- ggplot() + theme_minimal() + 
  geom_line(ma_w_chinook, mapping = aes(StartDate, moving_average), size = 1, color = "#fc8d62") +
  xlab("Time of Year") + ylab("Number of fish caught per hour") +
  scale_x_datetime(expand = c(0,0),
                   limits = c(as.POSIXct("2000-01-02"),as.POSIXct("2001-08-19")), 
                   date_labels="%b") 

p1_w

species = "Chinook"
type = "H"
river = "Dungeness"
ma_h_chinook <- moving_average(species,type,river, ".accdb")
p1 <- p1_w + geom_line(ma_h_chinook, mapping = aes(StartDate, moving_average), size = 1, color = "#66c2a5") +
  xlab("Time of Year") + ylab("Number of fish caught per hour")+
  labs(title = river, subtitle = paste(species))+
  theme(axis.title.x=element_blank())


p1


species = "Coho"
type = "W"
river = "Dungeness"
ma_w_coho <- moving_average(species,type,river, ".accdb")
p2_w <- ggplot() + theme_minimal() + 
  geom_line(ma_w_coho, mapping = aes(StartDate, moving_average), size = 1, color = "#fc8d62") +
  xlab("Time of Year") + ylab("Number of fish caught per hour") +
  scale_x_datetime(expand = c(0,0),
                   limits = c(as.POSIXct("2000-01-02"),as.POSIXct("2001-08-19")), 
                   date_labels="%b")  + 
  labs(subtitle = paste(species))+
  theme(axis.title.y=element_blank(),axis.title.x=element_blank())

p2_w

species = "Coho"
type = "H"
river = "Dungeness"
ma_h_coho <- moving_average(species,type,river, ".accdb")
p2 <- p2_w + geom_line(ma_h_coho, mapping = aes(StartDate, moving_average), size = 1, color = "#66c2a5") +
  xlab("Time of Year") + ylab("Number of fish caught per hour") +
  labs(subtitle = paste(species))+
  theme(axis.title.y=element_blank(),axis.title.x=element_blank())

p2

p1+p2



species = "Steelhead"
type = "W"
river = "Dungeness"
ma_w_steel <- moving_average(species,type,river, ".accdb")
p3_w <- ggplot() + theme_minimal() + 
  geom_line(ma_w_steel, mapping = aes(StartDate, moving_average, color = "Wild"), size = 1) +
  xlab("Time of Year") + ylab("Number of fish caught per hour") +
  scale_x_datetime(expand = c(0,0),
                   limits = c(as.POSIXct("2000-01-02"),as.POSIXct("2001-08-19")), 
                   date_labels="%b")  +
  labs(subtitle = paste(species))+
  scale_color_manual(name = "Fish type", values = c("Wild" = "#fc8d62", "Hatchery" = "#66c2a5"))+
  theme(axis.title.y=element_blank(),axis.title.x=element_blank())


p3_w

species = "Steelhead"
type = "H"
river = "Dungeness"
ma_h_steel <- moving_average(species,type,river, ".accdb")
p3 <- p3_w + geom_line(ma_h_steel, mapping = aes(StartDate, moving_average, color = "Hatchery"), size = 1) +
  xlab("Time of Year") + ylab("Number of fish caught per hour") +
  labs(subtitle = paste(species))+
  scale_color_manual(name = "Fish type", values = c("Wild" = "#fc8d62", "Hatchery" = "#66c2a5"))+
  theme(axis.title.y=element_blank())


p3

n <- p1+p2+p3

n

#nisqually 

species = "Chinook"
type = "W"
river = "Nisqually"
ma_w_chinook_n <- moving_average(species,type,river, ".accdb")
p4_w <- ggplot() + theme_minimal() + 
  geom_line(ma_w_chinook_n, mapping = aes(StartDate, moving_average), size = 1, color = "#fc8d62") +
  xlab("Time of Year") + ylab("Number of fish caught per hour") +
  scale_x_datetime(expand = c(0,0),
                   limits = c(as.POSIXct("2000-01-02"),as.POSIXct("2001-08-19")), 
                   date_labels="%b") +
  annotate("text", x = as.POSIXct("2000-07-02"), y = 7.2, label = "year 0")+
  annotate("text", x = as.POSIXct("2001-07-02"), y = 7.2, label = "year 1")+
  labs(title = "Nisqually")+
  theme(axis.title.x=element_blank())

p4 <- p4_w 
p4

species = "Coho"
type = "W"
river = "Nisqually"
ma_w_coho_n <- moving_average(species,type,river, ".accdb")
p5 <- ggplot() + theme_minimal() + 
  geom_line(ma_w_coho_n, mapping = aes(StartDate, moving_average), size = 1, color = "#fc8d62") +
  xlab("Time of Year") + ylab("Number of fish caught per hour") +
  scale_x_datetime(expand = c(0,0),
                   limits = c(as.POSIXct("2000-01-02"),as.POSIXct("2001-08-19")), 
                   date_labels="%b")  +
  annotate("text", x = as.POSIXct("2000-07-02"), y = 7.2, label = "year 0")+
  annotate("text", x = as.POSIXct("2001-07-02"), y = 7.2, label = "year 1")+
  theme(axis.title.y=element_blank(),axis.title.x=element_blank())

p5

species = "Steelhead"
type = "W"
river = "Nisqually"
ma_w_steel_n <- moving_average(species,type,river, ".accdb")
p6 <- ggplot() + theme_minimal() + 
  geom_line(ma_w_steel_n, mapping = aes(StartDate, moving_average, color = "Wild"), size = 1) +
  xlab("Time of Year") + ylab("Number of fish caught per hour") +
  scale_x_datetime(expand = c(0,0),
                   limits = c(as.POSIXct("2000-01-02"),as.POSIXct("2001-08-19")), 
                   date_labels="%b")  +
  scale_color_manual(name = "Fish type", values = c("Wild" = "#fc8d62", "Hatchery" = "#66c2a5"))+
  theme(axis.title.y=element_blank(),axis.title.x=element_blank())

p6

d <- p4+p5+p6
n/d + plot_layout(guides = 'collect') +
  draw_image(image=here("Documents","data","beautiful_graphics","Chinook_salmon_female.jpg"), x = 0.55, y = 0.55, 
                                                 width=0.4, height=0.4)

fig2 <- n/d
file_name <- "time_of_year.pdf"
ggsave(here("Documents","output","pied_piper",
            file_name), width=12, height=7)


p1_pic <- ggdraw(p1) + draw_image(image=here("Documents","data","beautiful_graphics","Chinook_salmon_female.png"), x = 0.36, y = 0.55, 
                width=0.4, height=0.4)
p2_pic <- ggdraw(p2) + draw_image(image=here("Documents","data","beautiful_graphics","coho_salmon_bright.png"), x = 0.35, y = 0.55, 
                        width=0.4, height=0.4)

p3_pic <- ggdraw(p3) + draw_image(image=here("Documents","data","beautiful_graphics","Steelhead_hen_big.png"), x = 0.35, y = 0.55, 
                        width=0.4, height=0.4)

fig <- (p1_pic + p2_pic + p3_pic)/(p4+p5+p6) + plot_layout(guides = 'collect')
fig_wo_pic <- n/d

fig_w_pic<-ggdraw(fig_wo_pic) + draw_image(image=here("Documents","data","beautiful_graphics","Chinook_salmon_female.png"), x = 0.12, y = 0.88, 
                                     width=0.15, height=0.15) + 
  draw_image(image=here("Documents","data","beautiful_graphics","coho_salmon_bright.png"), x = 0.4, y = 0.88, 
             width=0.15, height=0.15)+ 
  draw_image(image=here("Documents","data","beautiful_graphics","Steelhead_hen_big.png"), x = 0.7, y = 0.88, 
                                                  width=0.15, height=0.15)
fig_w_pic
file_name <- "time_of_year.png"
ggsave(here("Documents","output","pied_piper",
            file_name), width=12, height=7)
