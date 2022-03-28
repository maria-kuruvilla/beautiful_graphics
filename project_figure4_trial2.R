#Goal - produce one figure with multiple panels to plot average number of wild 
# and hatchery salmon out migrating as a function of time of day. One sub figure for
# river and species.
# edit - to have one dataframe and then use facet wrap

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

extract_all_data <- function(year, river, file_type){
  file = paste0(river,file_type)
  string_year = paste(toString(year),file)
  df <- access_file(string_year)
  df_species_all_years <- df[df$CaptureType==1,]
  df_species_all_years$StartDate_OG <- df_species_all_years$StartDate
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
  df_species_all_years$location <- river
  return(df_species_all_years)
}

bind_data <- function(river,year, file_type){
  for(i in 1:length(river)){
    for(j in 1:length(year)){
      
      
      if(year[j]==2016 && river[i] == "Nisqually"){
        
        df_river_year <- extract_all_data(year[j], river[i], file_type) 
        
      }
      else {
        df_one <- extract_all_data(year[j], river[i], file_type)
        df_river_year <- rbind(df_one,df_river_year)
      }
    }
  }
  return(df_river_year)
}

sum_start_time <- function(df_combine){
  
  my_data_frame <- df_combine %>%
    group_by(WSPEName, start_datetime,start_time, location) %>%
    summarise(sum_num_caught = sum(fish_per_hour))
  
  return(my_data_frame)
}

avg_start_time <- function(df){
  my_data_frame <- df %>%
    group_by(WSPEName,start_time, location) %>%
    summarise(avg_num_caught = mean(sum_num_caught))
  
  return(my_data_frame)
}



rivers = c("Nisqually","Dungeness")
years = c(2016,2017,2018,2019,2020)
file_type = ".accdb"

df_all <- bind_data(rivers, years, file_type)

sum <- sum_start_time(df_all)

avg <- avg_start_time(sum)
avg$hour <- hour(as.POSIXct(avg$start_time, format = "%H:%M:%S"))

ggplot(avg, aes(x = hour, y = avg_num_caught)) +
  geom_col() + 
  facet_wrap(vars(location,WSPEName), scales = "free_y")

# have a column describing hatchery or wild

