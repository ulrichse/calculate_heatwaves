

# Heat Index calculations, North Carolina 2010-2020

library(lubridate)
library(dplyr)
library(arrow)
library(tidyverse)
library(weathermetrics)

setwd("C:/Users/ulric/OneDrive - University of North Carolina at Chapel Hill/Documents - Extreme Heat Events/")

# File listing ZCTAs and corresponding CONUS Climate Division
clim_region <- read.csv("project_files/calculate_metrics/NC_ZCTA_Climate_Divisions.csv")

clim_region <- clim_region %>%
  filter(!(grepl("^VA", clim_div) | grepl("^SC", clim_div)))

# Read the daily relative humnidity, mean, max, and average temperature time series from PRISM
# and merge with climate region file
df <- read_parquet("shared_data/raw/PRISM/Heatwave_Metrics.parquet")%>%
  filter(year(Date) >= 2010 & year(Date) <= 2020)%>%
  left_join(clim_region, by=c('Zip'))

# Calculate Heat Index

df$HI <- heat.index(t = df$TAVG, rh = df$RH, temperature.metric = "celsius")

# Calculate heatwaves based on Heat Index definiton

calculate_heatwave <- function(df, region_column, temperature_columns, percentiles = c(0.75, 0.85, 0.90, 0.95, 0.975, 0.99)) {
  
  # Filter for the months of interest
  df <- df %>%
    filter(month(Date) >= 5 & month(Date) <= 9) %>%
    group_by(across(all_of(region_column)))
  
  # Iterate over temperature columns
  for (temp_col in temperature_columns) {
    
    # Calculate quantiles for each percentile
    for (p in percentiles) {
      pct_col_name <- paste0(temp_col, "_pct_", p * 100)
      above_col_name <- paste0("above_", temp_col, "_pct_", p * 100)
      
      # Calculate percentile and check if value exceeds it
      df <- df %>%
        mutate(
          !!pct_col_name := quantile(!!sym(temp_col), p, na.rm = TRUE),
          !!above_col_name := as.numeric(!!sym(temp_col) > !!sym(pct_col_name))
        )
    }
  }
  
  # Ungroup and return the final dataframe
  df <- df %>% ungroup()
  
  return(df)
}

# Calculate heatwave and store the result in a new dataframe
df_hi_heatwave <- calculate_heatwave(df, region_column = "clim_div", temperature_column = c("HI"), percentiles = c(0.75, 0.85, 0.90, 0.95, 0.975, 0.99))

percentiles <- c("75", "85", "90", "95", "975", "99")

for (p in percentiles) {
  df_hi_heatwave <- df_hi_heatwave %>%
    group_by(clim_div) %>%
    arrange(Date) %>%
    mutate(
      across(
        c(starts_with(paste0("above_HI_pct_", p))),
        list(
          two_days = ~ ifelse(. == 1 & lag(.) == 1 & lead(.) == 0, 1, 0),
          three_days = ~ ifelse(. == 1 & lag(., 2) == 1 & lag(.) == 1 & lead(.) == 0, 1, 0),
          four_days = ~ ifelse(. == 1 & lag(., 3) == 1 & lag(., 2) == 1 & lag(.) == 1 & lead(.) == 0, 1, 0)
        ),
        .names = "{col}_{fn}"
      )
    ) %>%
    ungroup()
}

write_parquet(df_hi_heatwave, "shared_data/extreme_heat_metrics/heat-index/heatwaves_heat_index_nc_zcta_2010_2020_v3.parquet")
