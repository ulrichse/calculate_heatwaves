
library(dplyr)

temp <- read.csv("S:/Projects/External_PI/BirthsToxics/Data/temperature/heatwaves climate region/climate_region_heatwaves_may_sept_2011_2019_w_absolute.csv")%>%
  mutate(Date=as.Date(Date),
         location=geoid10)

#temp <- read.csv("S:/Projects/External_PI/BirthsToxics/Data/temperature/heatwaves climate region/heat_events_2001_2019.csv")%>%
  #mutate(Date=as.Date(Date),
         #location=geoid10)


table(temp$above_pct_90)
table(temp$above_pct_95)
table(temp$two_days_90)
table(temp$three_days_90)
table(temp$four_days_90)
table(temp$two_days_95)
table(temp$three_days_95)
table(temp$four_days_95)
table(temp$two_days_975)
table(temp$three_days_975)
table(temp$four_days_975)
table(temp$two_days_99)
table(temp$three_days_99)
table(temp$four_days_99)
