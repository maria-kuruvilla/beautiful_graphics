#Goal - produce one figure with multiple panels to plot average number of wild 
# and hatchery salmon out migrating as a function of time of day. One sub figure for
# river and species.

library(RODBC)
library(here)
library(tidyverse)
library(tidyquant)
library(RColorBrewer)
library(patchwork)
library(scales)

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

extract_species_both_data_error <- function(year, species,river,file_type){
  if(species == "Steelhead"){
    data_filter_w = paste(species,"smolt","W")
    data_filter_h = paste(species,"smolt", "H")
    file = paste0(river,file_type)
    string_year = paste(toString(year),file)
    print(string_year)
    year0 = year-1
    df <- access_file(string_year)
    df$StartDate_OG <- df$StartDate
    df_species_all_years <- df[(((df$WSPEName==data_filter_w) | (df$WSPEName==data_filter_h))
                                & df$CaptureType==1),]
    df_species_all_years$StartDate_OG <- df_species_all_years$StartDate
  }
  else {
    data_filter0_w = paste(species,"0+", "W")
    data_filter0_h = paste(species,"0+", "H")
    data_filter_w = paste(species,"1+", "W")
    data_filter_h = paste(species,"1+", "H")
    file = paste0(river,file_type)
    year0 = year-1
    string_year = paste(toString(year),file)
    string_year0 = paste(toString(year0),file)
    print(string_year)
    print(string_year0)
    
    df <- access_file(string_year)
    #print(unique(df$WSPEName))
    df_species <- df[(((df$WSPEName==data_filter_w) | (df$WSPEName==data_filter_h))
                      & df$CaptureType==1),]
    df_species$StartDate_OG <- df_species$StartDate
    
    
    df0 <- access_file(string_year0)
    df_species0 <- df0[(((df0$WSPEName==data_filter0_w) | (df0$WSPEName==data_filter0_h))
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

sum_species_both_data <- function(year, species,river,file_type){
  df <- extract_species_both_data_error(year, species,river,file_type)
  df_new_summarize <- df %>%
    group_by(WSPEName, StartDate, day_of_year, year, StartDate_OG, start_time, start_datetime) %>%
    summarise(sum_num_caught = sum(fish_per_hour))
  return(df_new_summarize)
}

average_start_time <- function(species,river,file_type){
  if(species == "Steelhead"){
    last_year = 2020
  }
  else {
    last_year = 2021
  }
  for(i in 2016:last_year){
    if(i==2016){
      df_combine <- sum_species_both_data(i, species,river,file_type)
    }
    else {
      df <- sum_species_both_data(i, species,river,file_type)
      df_combine <- rbind(df_combine,df)
    }
    
  }
  my_data_frame <- df_combine %>%
    group_by(WSPEName, start_time) %>%
    summarise(sum_num_caught = sum(sum_num_caught))
  
  return(my_data_frame)
}

species = "Chinook"
river = "Dungeness"
file_type = ".accdb"

chinook_time <- average_start_time(species,river,file_type)
chinook_time$hour <- hour(as.POSIXct(chinook_time$start_time, format = "%H:%M:%S"))
chinook_time_subset <- chinook_time[(chinook_time$WSPEName == "Chinook 0+ H") | (chinook_time$WSPEName == "Chinook 0+ W"),]

p1 <- ggplot(data = chinook_time, 
             aes(x = as.POSIXct(start_time, format = "%H:%M:%S"), y = moving_average, color = WSPEName)) +
  theme_minimal() + geom_line() + 
  scale_x_datetime(breaks = date_breaks("12 hour"))

p1

p1 <- ggplot(data = chinook_time_subset, 
             aes(x = hour, y = sum_num_caught, fill = WSPEName)) +
  theme_minimal() + geom_col(width = 5,position = position_dodge(preserve = "single")) +
  scale_fill_manual(values = c("Chinook 0+ H" = "#66c2a5", "Chinook 0+ W" = "#fc8d62"),
                    guide = FALSE) + 
  xlab("Hour of Day") + ylab("Number of fish caught per hour in Dungeness")+
  labs(title = river, subtitle = paste(species)) +
  theme(axis.title.x=element_blank())

p1


#coho dunge

species = "Coho"
river = "Dungeness"
file_type = ".accdb"

coho_time <- average_start_time(species,river,file_type)
coho_time$hour <- hour(as.POSIXct(coho_time$start_time, format = "%H:%M:%S"))
coho_time_subset <- coho_time[(coho_time$WSPEName == "Coho 1+ H") | (coho_time$WSPEName == "Coho 1+ W"),]


p2 <- ggplot(data = coho_time_subset, 
             aes(x = hour, y = sum_num_caught, fill = WSPEName)) +
  theme_minimal() + geom_col(width = 5,position = position_dodge(preserve = "single")) +
  scale_fill_manual(values = c("Coho 1+ H" = "#66c2a5", "Coho 1+ W" = "#fc8d62"),
                    guide = FALSE) + 
  xlab("Hour of Day") + ylab("Number of fish caught per hour")+
  labs(subtitle = paste(species)) +
  theme(axis.title.y=element_blank())

p2

#Steelhead dunge

species = "Steelhead"
river = "Dungeness"
file_type = ".accdb"

steel_time <- average_start_time(species,river,file_type)
steel_time$hour <- hour(as.POSIXct(steel_time$start_time, format = "%H:%M:%S"))
#steel_time_subset <- steel_time[(steel_time$WSPEName == "Coho 1+ H") | (steel_time$WSPEName == "Coho 1+ W"),]


p3 <- ggplot(data = steel_time, 
             aes(x = hour, y = sum_num_caught, fill = WSPEName)) +
  theme_minimal() + geom_col(width = 5,position = position_dodge(preserve = "single")) +
  scale_fill_manual(name = "Fish origin",
                    values = c("Steelhead smolt H" = "#66c2a5", "Steelhead smolt W" = "#fc8d62"),
                    labels = c("Hatchery","Wild")) + 
  xlab("Hour of Day") + ylab("Number of fish caught per hour")+
  labs(subtitle = paste(species)) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank()) + 
  scale_x_continuous(limits = c(0,24))


p3



###############
## chinook nis

species = "Chinook"
river = "Nisqually"
file_type = ".accdb"

chinook_time_n <- average_start_time(species,river,file_type)
chinook_time_n$hour <- hour(as.POSIXct(chinook_time_n$start_time, format = "%H:%M:%S"))
chinook_time_subset_n <- chinook_time[(chinook_time$WSPEName == "Chinook 0+ W"),]


p4 <- ggplot(data = chinook_time_subset_n, 
             aes(x = hour, y = sum_num_caught, fill = WSPEName)) +
  theme_minimal() + geom_col(width = 5,position = position_dodge(preserve = "single")) +
  scale_fill_manual(values = c("Chinook 0+ W" = "#fc8d62"),
                    guide = FALSE) + 
  xlab("Hour of Day") + ylab("Number of fish caught per hour in Nisqually")+
  labs(title = river, subtitle = paste(species)) +
  theme(axis.title.x=element_blank())

p4


#coho nisqually
species = "Coho"
river = "Nisqually"
file_type = ".accdb"

coho_time_n <- average_start_time(species,river,file_type)
coho_time_n$hour <- hour(as.POSIXct(coho_time_n$start_time, format = "%H:%M:%S"))
coho_time_subset_n <- coho_time[(coho_time$WSPEName == "Coho 1+ W"),]


p5 <- ggplot(data = coho_time_subset_n, 
             aes(x = hour, y = sum_num_caught, fill = WSPEName)) +
  theme_minimal() + geom_col(width = 5,position = position_dodge(preserve = "single")) +
  scale_fill_manual(values = c("Coho 1+ W" = "#fc8d62"),
                    guide = FALSE) + 
  xlab("Hour of Day") + ylab("Number of fish caught per hour")+
  labs(subtitle = paste(species)) +
  theme(axis.title.y=element_blank())

p5


#Steel nisqually
species = "Steelhead"
river = "Nisqually"
file_type = ".accdb"

steel_time_n <- average_start_time(species,river,file_type)
steel_time_n$hour <- hour(as.POSIXct(steel_time_n$start_time, format = "%H:%M:%S"))
#steel_time_subset_n <- coho_time[(coho_time$WSPEName == "Steelhea 1+ W"),]


p6 <- ggplot(data = steel_time_n, 
             aes(x = hour, y = sum_num_caught, fill = WSPEName)) +
  theme_minimal() + geom_col(width = 5,position = position_dodge(preserve = "single")) +
  scale_fill_manual(values = c("Steelhead smolt W" = "#fc8d62"),
                    guide = FALSE) + 
  xlab("Hour of Day") + ylab("Number of fish caught per hour")+
  labs(subtitle = paste(species)) +
  theme(axis.title.y=element_blank(),axis.title.x=element_blank()) + 
  scale_x_continuous(limits = c(0,24))

p6

combined <- (p1 + p2 + p3)/ (p4 + p5 + p6) #+ plot_annotation(caption = "Outmigration of juvenile salmon (both hatchery and wild) during different hours of the day in the Dungeness river (top row) and Nisqually river (bottom row)",theme = theme(plot.caption = element_text(hjust = 0)))
combined + plot_layout(guides = 'collect') 

file_name <- "project_fig4.png"
ggsave(here("Documents","output","beautiful_graphics",
            file_name), width=12, height=7)
