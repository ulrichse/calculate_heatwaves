
library(data.table)
library(lubridate)
library(dplyr)
library(arrow)
library(lubridate)
library(ggpubr)
library(tidyverse)
library(zoo)

setwd("C:/Users/ulric/OneDrive - University of North Carolina at Chapel Hill/Documents - Extreme Heat Events/")

# File listing ZCTAs and corresponding CONUS Climate Division
clim_region <- read.csv("project_files/calculate_metrics/NC_ZCTA_Climate_Divisions.csv")

clim_region <- clim_region %>%
  filter(!(grepl("^VA", clim_div) | grepl("^SC", clim_div)))

# Read the daily mean, max, and average temperature time series from PRISM
# and merge with climate region file
df <- read_parquet("shared_data/raw/PRISM/NC_PRISM_ZIP_2008_2021.parquet")%>%
  filter(year(Date) >= 2010 & year(Date) <= 2020)%>%
  left_join(clim_region, by=c('Zip'))%>%
  filter(!is.na(clim_div))

df_filter <- df %>%
  filter(clim_div == "NC SOUTHERN COASTAL PLAIN")

# Convert data to a time series object and then to a 'zoo' object 
df.ts <- zoo(df_filter[, -1], order.by = df_filter$Date)
head(df.ts)

plot(df_ts)

df_aggregated <- df_filter %>%
  group_by(Date) %>%
  summarize(
    tmin = mean(tmin, na.rm = TRUE),
    tmax = mean(tmax, na.rm = TRUE),
    tmean = mean(tmean, na.rm = TRUE),
    tdmean = mean(tdmean, na.rm = TRUE),
    X = mean(X, na.rm = TRUE)
  )

# Convert to zoo object
df_zoo <- zoo(df_aggregated[, -1], order.by = df_aggregated$Date)

plot(df_zoo[, c("tmin", "tmax")], main = "Minimum and Maximum Temperature over Time")

diff.ts <- diff(df_zoo)
plot(diff.ts[, c("tmin", "tmax")], main = "Minimum and Maximum Temperature over Time (Detrended)")




