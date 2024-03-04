
# Code to calculate heat events for 2001-2019 time period

library(data.table)
library(dplyr)
library(lubridate)
library(readxl)
library(tmap)
library(sf)

# Read in temp and climate region data
temp <- read.csv("S:/Projects/External_PI/BirthsToxics/Data/avgTemp_NC.csv")%>%
  mutate(Date=as.Date(Date, format="%Y-%m-%d"))%>%
  filter(Variable=="TAVG")%>%
  filter(year(Date) < 2020)%>%
  rename(geoid20=region_code)

regions <- read_excel("S:/Projects/External_PI/BirthsToxics/Data/temperature/heatwaves climate region/nc_climate_regions_2010_tracts.xlsx")%>%
  mutate(geoid10=as.numeric(geoid10))%>%
  rename(region=CD_NEW)%>%
  select(geoid10, NAME, region)

cw <- read_excel("S:/Projects/External_PI/BirthsToxics/Data/census/tract_relationships_2010_2020.xlsx") %>%
  mutate(geoid20=as.numeric(GEOID_TRACT_20),
         geoid10=as.numeric(GEOID_TRACT_10))%>%
  select(geoid10, geoid20)

# Crosswalk the temp data to 2010 census tracts

temp_cw <- temp %>%
  left_join(cw, by=c('geoid20')) %>%
  distinct(geoid10, Date, .keep_all=TRUE)

# Merge temp data with climate regions

temp_regions_cw <- temp_cw %>%
  left_join(regions, by=c('geoid10'))

# Calculate temperature thresholds using just May-September (Definition 1)

temp_def1 <- temp_regions_cw %>%
  filter(month(Date) >= 5 & month(Date) <= 9)%>% # Filter for May-September
  group_by(region)%>%
  mutate(
    pct_90 = quantile(Temperature, 0.90, na.rm=TRUE),
    pct_95 = quantile(Temperature, 0.95, na.rm=TRUE),
    pct_975 = quantile(Temperature, 0.975, na.rm=TRUE),
    pct_99 = quantile(Temperature, 0.99, na.rm=TRUE),
    above_pct_90 = as.numeric(Temperature > pct_90),
    above_pct_95 = as.numeric(Temperature > pct_95),
    above_pct_975 = as.numeric(Temperature > pct_975),
    above_pct_99 = as.numeric(Temperature > pct_99),
  ) %>%
  ungroup ()

# Check temps

table(temp_def1$above_pct_90)
table(temp_def1$pct_90)
table(temp_def1$above_pct_99)
table(temp_def1$pct_99)

#### Heatwaves based on May-September threshold ####

temp_heatwave1 <- temp_def1 %>%
  group_by(geoid10) %>%
  arrange(Date) %>%
  mutate(
    two_days_90 = ifelse(above_pct_90 == 1 & lag(above_pct_90) ==1 & lead(above_pct_90) == 0, 1, 0),
    three_days_90 = ifelse(above_pct_90 == 1 & lag(above_pct_90, 2) == 1 & lag(above_pct_90) == 1 & lead(above_pct_90)== 0, 1, 0),
    four_days_90 = ifelse(above_pct_90 == 1 & lag(above_pct_90, 3) == 1 & lag(above_pct_90, 2) == 1 & lag(above_pct_90) == 1 & lead(above_pct_90) == 0, 1, 0)
  )%>%
  ungroup()

temp_heatwave1 <- temp_heatwave1 %>%
  group_by(geoid10) %>%
  arrange(Date) %>%
  mutate(
    two_days_95 = ifelse(above_pct_95 == 1 & lag(above_pct_95) ==1 & lead(above_pct_95) == 0, 1, 0),
    three_days_95 = ifelse(above_pct_95 == 1 & lag(above_pct_95, 2) == 1 & lag(above_pct_95) == 1 & lead(above_pct_95)== 0, 1, 0),
    four_days_95 = ifelse(above_pct_95 == 1 & lag(above_pct_95, 3) == 1 & lag(above_pct_95, 2) == 1 & lag(above_pct_95) == 1 & lead(above_pct_95) == 0, 1, 0)
  )%>%
  ungroup()

temp_heatwave1 <- temp_heatwave1 %>%
  group_by(geoid10) %>%
  arrange(Date) %>%
  mutate(
    two_days_975 = ifelse(above_pct_975 == 1 & lag(above_pct_975) ==1 & lead(above_pct_975) == 0, 1, 0),
    three_days_975 = ifelse(above_pct_975 == 1 & lag(above_pct_975, 2) == 1 & lag(above_pct_975) == 1 & lead(above_pct_975)== 0, 1, 0),
    four_days_975 = ifelse(above_pct_975 == 1 & lag(above_pct_975, 3) == 1 & lag(above_pct_975, 2) == 1 & lag(above_pct_975) == 1 & lead(above_pct_975) == 0, 1, 0)
  )%>%
  ungroup()

temp_heatwave1 <- temp_heatwave1 %>%
  group_by(geoid10) %>%
  arrange(Date) %>%
  mutate(
    two_days_99 = ifelse(above_pct_99 == 1 & lag(above_pct_99) ==1 & lead(above_pct_99) == 0, 1, 0),
    three_days_99 = ifelse(above_pct_99 == 1 & lag(above_pct_99, 2) == 1 & lag(above_pct_99) == 1 & lead(above_pct_99)== 0, 1, 0),
    four_days_99 = ifelse(above_pct_99 == 1 & lag(above_pct_99, 3) == 1 & lag(above_pct_99, 2) == 1 & lag(above_pct_99) == 1 & lead(above_pct_99) == 0, 1, 0)
  )%>%
  ungroup()

temp_heatwave1 <- temp_heatwave1 %>%
  select(Date, geoid10, pct_90, pct_95, pct_975, pct_99, above_pct_90, above_pct_95, above_pct_975, above_pct_99, 
         two_days_90, three_days_90, four_days_90, two_days_95, three_days_95, four_days_95,
         two_days_975, three_days_975, four_days_975, two_days_99, three_days_99, four_days_99)

temp_heatwave1 <- temp_regions_cw %>%
  select(geoid10, Date, region, Temperature)%>%
  left_join(temp_heatwave1, by=c('Date', 'geoid10'))
temp_heatwave1[is.na(temp_heatwave1)] <- 0
table(temp_heatwave1$above_pct_99)


# Absolute value calculations

state_90_may_sep = quantile(temp_def1$Temperature, 0.90, na.rm=TRUE)
state_95_may_sep = quantile(temp_def1$Temperature, 0.95, na.rm=TRUE)
state_99_may_sep = quantile(temp_def1$Temperature, 0.99, na.rm=TRUE)

temp_heatwave1 <- temp_heatwave1 %>%
  mutate(state_90=ifelse(Temperature > 26.5, 1, 0),
         state_95=ifelse(Temperature > 27.5, 1, 0),
         state_99=ifelse(Temperature > 29.2, 1, 0),
  )

temp1_filter <- temp_heatwave1%>%
  filter(geoid10==37063000101 & month(Date)==7 & year(Date)==2011) #There should be heatwaves for this census tract in Durham during July 2011

write.csv(temp_heatwave1, "S:/Projects/External_PI/BirthsToxics/Data/temperature/heatwaves climate region/heat_events_2001_2019.csv")

# Map and QC

tracts <- read_sf("S:/GeoData/NC/Census_Boundaries/2010/Tract/2010_Census_trct.shp")
tracts <- tracts %>%
  rename(geoid10=GEOID10)%>%
  select(geoid10, geometry)
tracts$geoid10 <- as.numeric(tracts$geoid10)

temp <- temp_heatwave1 %>%
  group_by(geoid10)%>%
  summarize(count90=sum(above_pct_90),
            count902=sum(two_days_90),
            count903=sum(three_days_90), 
            count904=sum(four_days_90),
            count95=sum(above_pct_95),
            count952=sum(two_days_95),
            count953=sum(three_days_95), 
            count954=sum(four_days_95),
            count975=sum(above_pct_975),
            count9752=sum(two_days_975),
            count9753=sum(three_days_975), 
            count9754=sum(four_days_975),
            count99=sum(above_pct_99),
            count992=sum(two_days_99),
            count993=sum(three_days_99), 
            count994=sum(four_days_99),
            state90ct=sum(state_90),
            state95ct=sum(state_95),
            state99ct=sum(state_99),
            state90ctwm=sum(state_90_may_sep),
            state95ctwm=sum(state_95_may_sep),
            state99ctwm=sum(state_99_may_sep)
  )

temp <- tracts %>%
  left_join(temp, by=c('geoid10'))

tm_shape(temp)+
  tm_polygons(
    col="count994",
    border.col = NULL, 
    style= "jenks", 
    n=5
  )

# Write off shapefiles for mapping

write_sf(temp, "S:/Projects/External_PI/BirthsToxics/Data/temperature/heatwaves climate region/heat_events_2001_2019.shp")


