library(data.table)
library(dplyr)
library(arrow)
library(humidity)

#==============================================================================#
#                            Heatwaves                                      ----
#==============================================================================#

setwd("~/Heatwave/Data/Zip")

#==============================================================================#
# Calculate the 85th percentile EHF
#==============================================================================#

Heatwave <- left_join(open_dataset("NC_Heatwave.parquet") %>%
                        mutate(Severe_occurance = ifelse(EHF > 0, 1, 0),
                               daynum = 1) %>%
                        collect() %>%
                        drop_na(.),
                      open_dataset("NC_Heatwave.parquet") %>% 
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

write_parquet(Heat_Calc, "Heatwave_Metrics.parquet")

#==============================================================================#
#                            Coldwaves                                      ----
#==============================================================================#

setwd("~/Heatwave/Data/Zip")

#==============================================================================#
# Combine .csv and clean the data
#==============================================================================#

Coldwave <- left_join(open_dataset("NC_Coldwave.parquet") %>%
                        mutate(Severe_occurance = ifelse(ECF > 0, 1, 0),
                               daynum = 1) %>%
                        collect() %>%
                        drop_na(.),
                      open_dataset("NC_Coldwave.parquet") %>% # Calculate the 85th percentile ECF
                        select(Zip, ECF) %>%
                        group_by(Zip) %>% 
                        collect() %>%
                        filter(ECF > 0) %>%
                        do(data.frame(t(quantile(.$ECF, probs = c(0.85), na.rm=T)))) %>%
                        rename(ECF85 = X85.),
                      by = c("Zip"))

#==============================================================================#
# Calculate the daily data
#==============================================================================#

Cold_Calc <- Coldwave %>%
  group_by(Zip)%>%
  mutate(Severe_Coldwaves = (ifelse(ECF >= ECF85, 1, 0)),
         Extreme_Coldwaves = (ifelse(ECF >= (3 * ECF85), 1, 0)),
         Below_5th = fifelse(TAVG > C5, 1, 0),
         Below_3rd  = fifelse(TAVG > C3, 1, 0),
         Below_1st = fifelse(TAVG > C1, 1, 0),
         low_intensity = (ifelse(ECF > 0 & ECF < 1, 1, 0)),
         moderate_intensity = (ifelse(ECF >= 1 & ECF < 2, 1, 0)),
         high_intensity = (ifelse(ECF >= 2, 1, 0)),
         RH = round(RH(TAVG, Tdmean, isK = FALSE), 2),
         Coldwave = if_else(low_intensity > 0 | moderate_intensity > 0 | high_intensity > 0, 1, 0))%>%
  dplyr::select(Date, Zip, TAVG, TMIN, TMAX, RH, ECF, ECI_sig, ECI_accl,
                Severe_Coldwaves, Extreme_Coldwaves, Below_5th, Below_3rd, Below_1st, Coldwave, 
                low_intensity, moderate_intensity, high_intensity)

#==============================================================================#
# write to .csv
#==============================================================================#

write_parquet(Cold_Calc, "Coldwave.parquet")















