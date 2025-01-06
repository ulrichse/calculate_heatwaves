library(future)
library(furrr)
library(tidyverse)
library(arrow)
library(humidity)

setwd("C:/Users/ulric/OneDrive - University of North Carolina at Chapel Hill/Documents - Extreme Heat Events/")

#==============================================================================#
# Read in and set up data
#==============================================================================#

Temperature <- left_join(open_dataset("shared_data/raw/PRISM/NC_PRISM_ZIP_2008_2021.parquet") %>% # Create a df of the temeprature data and rename the columns
                           mutate(month = month(Date)) %>%
                           filter(year(Date) >= 2010 & year(Date) <= 2020)%>%
                           #filter(month(Date) >= 5 & month(Date) <= 9) %>%
                           collect(),
                         open_dataset("shared_data/raw/PRISM/NC_PRISM_ZIP_2008_2021.parquet") %>% # Create a df for extreme percentile of temperatures
                           mutate(month = month(Date)) %>%
                           filter(year(Date) >= 2010 & year(Date) <= 2020)%>%
                           #filter(month(Date) >= 5 & month(Date) <= 9) %>%
                           collect() %>%
                           group_by(Zip, month) %>%
                           do(data.frame(t(quantile(.$tmean, probs = c(0.95, 0.97, 0.99))))) %>%
                           rename(H95 = X95.,
                                  H97 = X97.,
                                  H99 = X99.),
                         by = c("Zip", "month")) %>%
  arrange(Date) %>%
  rename(Tdmean = tdmean,
         TMIN = tmin,
         TMAX = tmax,
         TAVG = tmean) %>%
  na.omit(.) # Remove any rows that contain missing values

#==============================================================================#
# Create Heatwave Functions
#==============================================================================#

# Calculate the EHI acclimation 
Three_day_average <- function(Data){
  
  # Initialize the start and end indices of the window
  start_index <- 31
  
  # Initialize a new column called is_larger
  Data[, "three_day_average"] <- 0
  
  # Loop through the dataset row by row with a 3 row window
  while ((start_index+2) <= nrow(Data)) {
    
    # Get the current window of rows
    current_window <- Data[(start_index-2):(start_index), ]
    
    # Get the mean value of the current window
    current_window_mean <- mean(current_window$TAVG)
    
    # Check if the current window mean value is greater than the previous 30 rows mean value
    Data$three_day_average[start_index] <- current_window_mean 
    
    # Increment the start and end indices of the window
    start_index <- start_index + 1
    
  }
  return(Data)
}

# Calculate the EHI acclimation 
Three_day_95th_percentile <- function(Data){
  
  # Initialize the start and end indices of the window
  start_index <- 31
  
  # Initialize a new column called is_larger
  Data[, "three_day_95th_percentile"] <- 0
  
  # Loop through the dataset row by row with a 3 row window
  while ((start_index+2) <= nrow(Data)) {
    
    # Get the current window of rows
    current_window <- Data[(start_index-2):(start_index), ]
    
    # Get the mean value of the current window
    current_window_mean <- mean(current_window$H95)
    
    # Check if the current window mean value is greater than the previous 30 rows mean value
    Data$three_day_95th_percentile[start_index] <- current_window_mean 
    
    # Increment the start and end indices of the window
    start_index <- start_index + 1
    
  }
  return(Data)
}

# Calculate the EHI acclimation 
Previous_30_day_mean <- function(Data){
  
  # Initialize the start and end indices of the window
  start_index <- 31
  
  # Initialize a new column called is_larger
  Data[, "Previous_30_day_mean"] <- 0
  
  # Loop through the dataset row by row with a 3 row window
  while ((start_index+2) <= nrow(Data)) {
    
    # Get the previous 30 rows
    prev_30_rows <- Data[(start_index-30):(start_index-1), ]
    
    # Get the mean value of the previous 30 rows
    prev_30_rows_mean <- mean(prev_30_rows$TAVG)
    
    # Check if the current window mean value is greater than the previous 30 rows mean value
    Data$Previous_30_day_mean[start_index] <- prev_30_rows_mean 
    
    # Increment the start and end indices of the window
    start_index <- start_index + 1
    
  }
  return(Data)
}

Calculate <- function(Data) {
  
  Data %>%
    Three_day_average() %>%
    Three_day_95th_percentile() %>%
    Previous_30_day_mean()
}  

#==============================================================================#
# Calculate Heat Wave
#==============================================================================#

EHF_pipeline <- function(dataset) {
  
  Test <- dataset %>%
    select(Zip, Date, TAVG, H95) %>%
    nest(data = c(-Zip)) %>%
    mutate(calculate = future_map(data, Calculate)) %>%
    select(-data) %>% 
    unnest(cols = c(calculate), names_repair = "minimal") %>% #unnests the nested data
    mutate(EHI_sig = three_day_average - three_day_95th_percentile,
           EHI_accl = three_day_average - Previous_30_day_mean,
           EHI_sig = if_else(EHI_sig < 0, 0, EHI_sig),
           EHI_accl = if_else(EHI_accl < 1, 1, EHI_accl),
           EHF = EHI_sig * EHI_accl) %>%
    select(-three_day_95th_percentile, -three_day_average, -Previous_30_day_mean)
}

# Run in parallel
plan(multisession, workers = (availableCores() - 1))
start <- now()
Temperature_1 <- EHF_pipeline(Temperature)
end <- now()
end-start

Temperature <- left_join(Temperature, Temperature_1 %>% select(-TAVG, -H95), by = c("Zip", "Date")) 

write_parquet(Temperature, "NC_EHF_Heatwave_2010_2020_ZCTA_M-S.parquet")

#==============================================================================#
# Calculate the 85th percentile EHF
#==============================================================================#

Heatwave <- left_join(open_dataset("NC_EHF_Heatwave_2010_2020_ZCTA_M-S.parquet") %>%
                        mutate(Severe_occurance = ifelse(EHF > 0, 1, 0),
                               daynum = 1) %>%
                        collect() %>%
                        drop_na(.),
                      open_dataset("NC_EHF_Heatwave_2010_2020_ZCTA_M-S.parquet") %>% 
                        select(Zip, EHF) %>%
                        group_by(Zip) %>% 
                        collect() %>%
                        filter(EHF > 0) %>%
                        do(data.frame(t(quantile(.$EHF, probs = c(0.85), na.rm=T)))) %>%
                        rename(EHF85 = X85.),
                      by = c("Zip"))

#==============================================================================#
# Calculate the daily information 
#==============================================================================#

Heat_Calc <- Heatwave %>%
  group_by(Zip) %>%
  mutate(Severe_Heatwaves = (ifelse(EHF >= EHF85, 1, 0)),
         Extreme_Heatwaves = (ifelse(EHF >= (3 * EHF85), 1, 0)),
         low_intensity = (ifelse(EHF > 0 & EHF < 1, 1, 0)),
         moderate_intensity = (ifelse(EHF >= 1 & EHF < 2, 1, 0)),  
         high_intensity = (ifelse(EHF >= 2, 1, 0)),
         Above_95th = fifelse(TAVG > H95, 1, 0),
         Above_97th  = fifelse(TAVG > H97, 1, 0),
         Above_99th = fifelse(TAVG > H99, 1, 0),
         RH = round(RH(TAVG, Tdmean, isK = FALSE), 2),
         Heatwave = if_else(low_intensity > 0 | moderate_intensity > 0 | high_intensity > 0, 1, 0)) %>%
  select(Date, Zip, TAVG, TMIN, TMAX, RH, EHF, EHI_sig, EHI_accl, 
         Severe_Heatwaves, Extreme_Heatwaves, Above_95th, Above_97th, Above_99th, Heatwave,
         low_intensity, moderate_intensity, high_intensity)

#==============================================================================#
# Write to .parquet
#==============================================================================#

write_parquet(Heat_Calc, "EHF_Heatwave_Metrics_NC_ZCTA_2010_2020_M-S.parquet")

