# Explore spatial distribution

library(arrow)
library(dplyr)
library(sf)
library(tigris)
library(mapview)
library(tmap)
library(lubridate)
library(tidyverse)
library(ggpubr)

setwd("C:/Users/ulric/OneDrive - University of North Carolina at Chapel Hill/Documents - Extreme Heat Events/")


df_heatwave <- read_parquet("shared_data/extreme_heat_metrics/relative-threshold/NC_ZCTA_Heatwaves_2010_2020_v3.parquet")
heat_index <- read_parquet("shared_data/extreme_heat_metrics/heat-index/heatwaves_heat_index_nc_zcta_2010_2020_v3.parquet")%>%
  dplyr::select(Date, Zip, starts_with(c("HI", "above_HI")))
ehf <- read_parquet("shared_data/extreme_heat_metrics/ehf/EHF_Heatwave_Metrics_NC_ZCTA_2010_2020_M-S.parquet")%>%
  dplyr::select(-TAVG, -TMIN, -TMAX, -RH)

df_heatwave <- df_heatwave %>%
  left_join(heat_index, by=c('Zip', 'Date'))%>%
  left_join(ehf, by=c('Zip', 'Date'))

write_parquet(df_heatwave, "shared_data/extreme_heat_metrics/time_series_all_definitions.parquet")

temp_thresholds <- df_heatwave %>%
  filter(!is.na(clim_div))%>%
  summarize(across(c(tmean_pct_90, tmean_pct_95, tmean_pct_99, 
                     tmin_pct_90, tmin_pct_95, tmin_pct_99, 
                     tmax_pct_90, tmax_pct_95, tmax_pct_99), mean, na.rm = TRUE))


hi_thresholds <- heat_index %>%
  filter(!is.na(clim_div)) %>%
  summarize(across(c(HI_pct_90, HI_pct_95, HI_pct_99), mean, na.rm = TRUE))
  
df_biv <- df_heatwave %>%
  rename(biv275=above_tmin_tmax_75_two_days,
         biv375=above_tmin_tmax_75_three_days,
         biv475=above_tmin_tmax_75_four_days,
         biv285=above_tmin_tmax_85_two_days,
         biv385=above_tmin_tmax_85_three_days,
         biv485=above_tmin_tmax_85_four_days,
         biv290=above_tmin_tmax_90_two_days,
         biv390=above_tmin_tmax_90_three_days,
         biv490=above_tmin_tmax_90_four_days,
         biv295=above_tmin_tmax_95_two_days,
         biv395=above_tmin_tmax_95_three_days,
         biv495=above_tmin_tmax_95_four_days,
         biv2975=above_tmin_tmax_97.5_two_days,
         biv3975=above_tmin_tmax_97.5_three_days,
         biv4975=above_tmin_tmax_97.5_four_days,
         biv299=above_tmin_tmax_99_two_days,
         biv399=above_tmin_tmax_99_three_days,
         biv499=above_tmin_tmax_99_four_days
  )%>%
  dplyr::select(Zip, 
                Date, 
                tmin_pct_75, 
                tmax_pct_75, 
                tmin_pct_85, 
                tmax_pct_85, 
                tmin_pct_90, 
                tmax_pct_90, 
                tmin_pct_95, 
                tmax_pct_95, 
                tmin_pct_97.5, 
                tmax_pct_97.5,
                tmin_pct_99, 
                tmax_pct_99,
                clim_div,
                biv275,
                biv375,
                biv475,
                biv285,
                biv385,
                biv485,
                biv290,
                biv390,
                biv490,
                biv295,
                biv395,
                biv495,
                biv2975,
                biv3975,
                biv4975,
                biv299,
                biv399,
                biv499
                )

df_threshold <- df_heatwave %>%
  dplyr::select(Zip, 
                Date, 
                tmean_pct_75, 
                tmean_pct_85, 
                tmean_pct_90, 
                tmean_pct_95, 
                tmean_pct_97.5,
                tmean_pct_99, 
                clim_div,
                above_tmean_pct_75_two_days,
                above_tmean_pct_75_three_days,
                above_tmean_pct_75_four_days,
                above_tmean_pct_85_two_days,
                above_tmean_pct_85_three_days,
                above_tmean_pct_85_four_days,
                above_tmean_pct_90_two_days,
                above_tmean_pct_90_three_days,
                above_tmean_pct_90_four_days,
                above_tmean_pct_95_two_days,
                above_tmean_pct_95_three_days,
                above_tmean_pct_95_four_days,
                above_tmean_pct_97.5_two_days,
                above_tmean_pct_97.5_three_days,
                above_tmean_pct_97.5_four_days,
                above_tmean_pct_99_two_days,
                above_tmean_pct_99_three_days,
                above_tmean_pct_99_four_days
  )%>%
  rename(tmean275=above_tmean_pct_75_two_days,
         tmean375=above_tmean_pct_75_three_days,
         tmean475=above_tmean_pct_75_four_days,
         tmean285=above_tmean_pct_85_two_days,
         tmean385=above_tmean_pct_85_three_days,
         tmean485=above_tmean_pct_85_four_days,
         tmean290=above_tmean_pct_90_two_days,
         tmean390=above_tmean_pct_90_three_days,
         tmean490=above_tmean_pct_90_four_days,
         tmean295=above_tmean_pct_95_two_days,
         tmean395=above_tmean_pct_95_three_days,
         tmean495=above_tmean_pct_95_four_days,
         tmean2975=above_tmean_pct_97.5_two_days,
         tmean3975=above_tmean_pct_97.5_three_days,
         tmean4975=above_tmean_pct_97.5_four_days,
         tmean299=above_tmean_pct_99_two_days,
         tmean399=above_tmean_pct_99_three_days,
         tmean499=above_tmean_pct_99_four_days)

heat_index <- heat_index %>%
  rename(hi275=above_HI_pct_75_two_days,
         hi375=above_HI_pct_75_three_days,
         hi475=above_HI_pct_75_four_days,
         hi285=above_HI_pct_85_two_days,
         hi385=above_HI_pct_85_three_days,
         hi485=above_HI_pct_85_four_days,
         hi290=above_HI_pct_90_two_days,
         hi390=above_HI_pct_90_three_days,
         hi490=above_HI_pct_90_four_days,
         hi295=above_HI_pct_95_two_days,
         hi395=above_HI_pct_95_three_days,
         hi495=above_HI_pct_95_four_days,
         hi299=above_HI_pct_99_two_days,
         hi399=above_HI_pct_99_three_days,
         hi499=above_HI_pct_99_four_days)%>%
  select(Zip, 
         Date, 
         HI_pct_75,
         HI_pct_85, 
         HI_pct_90, 
         HI_pct_95, 
         HI_pct_99,
         hi275,
         hi375, 
         hi475,
         hi285,
         hi385, 
         hi485,
         hi290,
         hi390, 
         hi490,
         hi295,
         hi395, 
         hi495,
         hi299,
         hi399, 
         hi499)
              
ehf <- ehf %>%
  filter(month(Date) >= 5 & month(Date) <= 9)%>%
  rename(ehfsev=Severe_Heatwaves, 
         ehfextr=Extreme_Heatwaves,
         ehfhw=Heatwave,
         ehflow=low_intensity,
         ehfmod=moderate_intensity,
         ehfhigh=high_intensity)
  
df_heatwave_agg <- df_threshold %>%
  left_join(df_biv, by=c('Zip', 'Date', 'clim_div'))%>%
  left_join(heat_index, by=c('Zip', 'Date'))%>%
  left_join(ehf, by=c('Zip', 'Date'))
  
df_heatwave_agg_year <- df_heatwave_agg %>%
  mutate(Year = year(Date)) %>%
  group_by(Year, Zip) %>%
  summarize(across(c(biv275,
                     biv375,
                     biv475,
                     biv285,
                     biv385,
                     biv485,
                     biv290,
                     biv390,
                     biv490,
                     biv295,
                     biv395,
                     biv495,
                     biv2975,
                     biv3975,
                     biv4975,
                     biv299,
                     biv399,
                     biv499,
                     hi275,
                     hi375, 
                     hi475,
                     hi285,
                     hi385, 
                     hi485,
                     hi290,
                     hi390, 
                     hi490,
                     hi295,
                     hi395, 
                     hi495,
                     hi299,
                     hi399, 
                     hi499,
                     tmean275,
                     tmean375,
                     tmean475,
                     tmean285,
                     tmean385,
                     tmean485,
                     tmean290,
                     tmean390,
                     tmean490,
                     tmean295,
                     tmean395,
                     tmean495,
                     tmean2975,
                     tmean3975,
                     tmean4975,
                     tmean299,
                     tmean399,
                     tmean499,
                     ehfsev, 
                     ehfextr,
                     ehfhw,
                     ehflow,
                     ehfmod,
                     ehfhigh), sum, .names = "{.col}"))

df_heatwave_agg_year_clim_region <- df_heatwave_agg %>%
  mutate(Year = year(Date)) %>%
  group_by(Year, Zip, clim_div) %>%
  summarize(across(c(biv275,
                     biv375,
                     biv475,
                     biv285,
                     biv385,
                     biv485,
                     biv290,
                     biv390,
                     biv490,
                     biv295,
                     biv395,
                     biv495,
                     biv2975,
                     biv3975,
                     biv4975,
                     biv299,
                     biv399,
                     biv499,
                     hi275,
                     hi375, 
                     hi475,
                     hi285,
                     hi385, 
                     hi485,
                     hi290,
                     hi390, 
                     hi490,
                     hi295,
                     hi395, 
                     hi495,
                     hi299,
                     hi399, 
                     hi499,
                     tmean275,
                     tmean375,
                     tmean475,
                     tmean285,
                     tmean385,
                     tmean485,
                     tmean290,
                     tmean390,
                     tmean490,
                     tmean295,
                     tmean395,
                     tmean495,
                     tmean2975,
                     tmean3975,
                     tmean4975,
                     tmean299,
                     tmean399,
                     tmean499,
                     ehfsev, 
                     ehfextr,
                     ehfhw,
                     ehflow,
                     ehfmod,
                     ehfhigh), sum, .names = "{.col}"))

df_heatwave_agg <- df_heatwave_agg %>%
  group_by(Zip)%>%
  summarize(across(c(biv275,
                     biv375,
                     biv475,
                     biv285,
                     biv385,
                     biv485,
                     biv290,
                     biv390,
                     biv490,
                     biv295,
                     biv395,
                     biv495,
                     biv2975,
                     biv3975,
                     biv4975,
                     biv299,
                     biv399,
                     biv499,
                     hi275,
                     hi375, 
                     hi475,
                     hi285,
                     hi385, 
                     hi485,
                     hi290,
                     hi390, 
                     hi490,
                     hi295,
                     hi395, 
                     hi495,
                     hi299,
                     hi399, 
                     hi499,
                     tmean275,
                     tmean375,
                     tmean475,
                     tmean285,
                     tmean385,
                     tmean485,
                     tmean290,
                     tmean390,
                     tmean490,
                     tmean295,
                     tmean395,
                     tmean495,
                     tmean2975,
                     tmean3975,
                     tmean4975,
                     tmean299,
                     tmean399,
                     tmean499,
                     ehfsev, 
                     ehfextr,
                     ehfhw,
                     ehflow,
                     ehfmod,
                     ehfhigh), sum, .names = "{.col}"))

df_heatwave_sum <- df_heatwave_agg %>%
  summarize(across(c(biv275,
                     biv375,
                     biv475,
                     biv285,
                     biv385,
                     biv485,
                     biv290,
                     biv390,
                     biv490,
                     biv295,
                     biv395,
                     biv495,
                     biv2975,
                     biv3975,
                     biv4975,
                     biv299,
                     biv399,
                     biv499,
                     hi275,
                     hi375, 
                     hi475,
                     hi285,
                     hi385, 
                     hi485,
                     hi290,
                     hi390, 
                     hi490,
                     hi295,
                     hi395, 
                     hi495,
                     hi299,
                     hi399, 
                     hi499,
                     tmean275,
                     tmean375,
                     tmean475,
                     tmean285,
                     tmean385,
                     tmean485,
                     tmean290,
                     tmean390,
                     tmean490,
                     tmean295,
                     tmean395,
                     tmean495,
                     tmean2975,
                     tmean3975,
                     tmean4975,
                     tmean299,
                     tmean399,
                     tmean499,
                     ehfsev, 
                     ehfextr,
                     ehfhw,
                     ehflow,
                     ehfmod,
                     ehfhigh), sum, na.rm = TRUE, .names = "{.col}"))

nc_zctas <- zctas(state = "NC",
                  year = 2010, 
                  cb = FALSE)

nc_zctas <- nc_zctas %>%
  mutate(Zip=as.numeric(ZCTA5CE10))%>%
  left_join(df_heatwave_agg, by=c('Zip'))

write_sf(nc_zctas, "shared_data/extreme_heat_metrics/shapefiles/NC_ZCTA_Heatwaves_All_Definitions_v2.shp")

#### Event count by year ####


#### Temporal analysis of events ####

#### TMEAN ####
tmean_names <- c("tmean290","tmean390","tmean490","tmean295","tmean395","tmean495","tmean299","tmean399","tmean499")
tmean_colors <- c(
  "tmean290" = "lightblue",
  "tmean390" = "blue",
  "tmean490" = "darkblue",
  "tmean295" = "lightgreen",
  "tmean395" = "green",
  "tmean495" = "darkgreen",
  "tmean299" = "pink",
  "tmean399" = "red",
  "tmean499" = "darkred"
)
tmean_labels = c(
  "tmean290" = "2-Day 90th Percentile",
  "tmean390" = "3-Day 90th Percentile",
  "tmean490" = "4-Day 90th Percentile",
  "tmean295" = "2-Day 95th Percentile",
  "tmean395" = "3-Day 95th Percentile",
  "tmean495" = "4-Day 95th Percentile",
  "tmean299" = "2-Day 99th Percentile",
  "tmean399" = "3-Day 99th Percentile",
  "tmean499" = "4-Day 99th Percentile")
tmean_long <- df_heatwave_agg_year %>%
  group_by(Year)%>%
  summarize(across(all_of(tmean_names), sum, .names = "{.col}"))%>%
  pivot_longer(cols = c("tmean290", "tmean390", "tmean490", "tmean295", "tmean395", "tmean495", "tmean299", "tmean399", "tmean499"), names_to = "Variable", values_to = "Value")
tmean_long$Variable <- factor(tmean_long$Variable, levels = c("tmean290", "tmean390", "tmean490", "tmean295", "tmean395", "tmean495", "tmean299", "tmean399", "tmean499"))

tmean_plot <- ggplot(tmean_long, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Count of Avg. Temp Heatwave Events by Year", x = "Year", y = "Value") +
  scale_color_manual(values = tmean_colors,
                     labels = tmean_labels
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()
plot(tmean_plot)

# by climate region

tmean_long <- df_heatwave_agg_year_clim_region %>%
  group_by(Year, clim_div)%>%
  summarize(across(all_of(tmean_names), sum, na.rm = TRUE, .names = "{.col}"))%>%
  pivot_longer(cols = c("tmean290","tmean390","tmean490","tmean295","tmean395","tmean495","tmean299","tmean399","tmean499"), names_to = "Variable", values_to = "Value")

df_plot <- tmean_long %>%
  filter(clim_div == "NC CENTRAL COASTAL PLAIN")
central_coastal_plain <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Central Coastal Plain (n=123)", x = "Year", y = "Value") +
  scale_color_manual(values = tmean_colors,
                     labels = tmean_labels) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal() +
  theme(legend.position = "none")

df_plot <- tmean_long %>%
  filter(clim_div == "NC CENTRAL PIEDMONT")
central_piedmont <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Central Piedmont (n=85)", x = "Year", y = "Value") +
  scale_color_manual(values = tmean_colors,
                     labels = tmean_labels) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal() +
  theme(legend.position = "none")

df_plot <- tmean_long %>%
  filter(clim_div == "NC NORTHERN COASTAL PLAIN")
northern_coastal_plain <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Northern Coastal Plain (n=76)", x = "Year", y = "Value") +
  scale_color_manual(values = tmean_colors,
                     labels = tmean_labels)+
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()+
  theme(legend.position = "none")

df_plot <- tmean_long %>%
  filter(clim_div == "NC NORTHERN MOUNTAINS")
northern_mountains <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Northern Mountains (n=57)", x = "Year", y = "Value") +
  scale_color_manual(values = tmean_colors,
                     labels = tmean_labels) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()+
  theme(legend.position = "none")

df_plot <- tmean_long %>%
  filter(clim_div == "NC NORTHERN PIEDMONT")
northern_piedmont <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Northern Piedmont (n=84)", x = "Year", y = "Value") +
  scale_color_manual(values = tmean_colors,
                     labels = tmean_labels
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()+
  theme(legend.position = "none")

df_plot <- tmean_long %>%
  filter(clim_div == "NC SOUTHERN COASTAL PLAIN")
southern_coastal_plain <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Southern Coastal Plain (n=118)", x = "Year", y = "Value") +
  scale_color_manual(values = tmean_colors,
                     labels = tmean_labels
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()+
  theme(legend.position = "none")

df_plot <- tmean_long %>%
  filter(clim_div == "NC SOUTHERN MOUNTAINS")
southern_mountains <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Southern Mountains (n=93)", x = "Year", y = "Value") +
  scale_color_manual(values = tmean_colors,
                     labels = tmean_labels
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()+
  theme(legend.position = "none")

df_plot <- tmean_long %>%
  filter(clim_div == "NC SOUTHERN PIEDMONT")
southern_piedmont <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Southern Piedmont (n=134)", x = "Year", y = "Value") +
  scale_color_manual(values = tmean_colors,
                     labels = tmean_labels
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()+
  theme(legend.position = "bottom") +    # Set legend position to bottom
  guides(color = guide_legend(ncol = 3))
legend <- get_legend(southern_piedmont)
southern_piedmont <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Southern Piedmont (n=134)", x = "Year", y = "Value") +
  scale_color_manual(values = tmean_colors,
                     labels = tmean_labels
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()+
  theme(legend.position = "none")

combined_plot <- ggarrange(
  ggarrange(northern_coastal_plain, central_coastal_plain, southern_coastal_plain, ncol = 3),
  ggarrange(northern_piedmont, central_piedmont, southern_piedmont, ncol = 3),
  ggarrange(northern_mountains, southern_mountains, legend, ncol = 3),
  nrow = 3  # Specify that there are 3 rows in total
)

annotated_plot <- annotate_figure(
  combined_plot,
  top = text_grob("Count of TMEAN Heatwave Events by NC Climate Region, 2010-2020", size = 16, face = "bold")
)

print(annotated_plot)

#### BIV ####
biv_names <- c("biv290","biv390","biv490","biv295","biv395","biv495","biv299","biv399","biv499")
biv_colors <- c(
  "biv290" = "lightblue",
  "biv390" = "blue",
  "biv490" = "darkblue",
  "biv295" = "lightgreen",
  "biv395" = "green",
  "biv495" = "darkgreen",
  "biv299" = "pink",
  "biv399" = "red",
  "biv499" = "darkred"
)
biv_labels <- c(
  "biv290" = "2-Day 90th Percentile",
  "biv390" = "3-Day 90th Percentile",
  "biv490" = "4-Day 90th Percentile",
  "biv295" = "2-Day 95th Percentile",
  "biv395" = "3-Day 95th Percentile",
  "biv495" = "4-Day 95th Percentile",
  "biv299" = "2-Day 99th Percentile",
  "biv399" = "3-Day 99th Percentile",
  "biv499" = "4-Day 99th Percentile")
biv_long <- df_heatwave_agg_year %>%
  group_by(Year)%>%
  summarize(across(all_of(biv_names), sum, .names = "{.col}"))%>%
  pivot_longer(cols = c("biv290","biv390","biv490","biv295","biv395","biv495","biv299","biv399","biv499"), names_to = "Variable", values_to = "Value")
biv_long$Variable <- factor(biv_long$Variable, levels = c("biv290","biv390","biv490","biv295","biv395","biv495","biv299","biv399","biv499"))
biv_plot <- ggplot(biv_long, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Count of Bivariate Heatwave Events by Year", x = "Year", y = "Value") +
  scale_color_manual(values = biv_colors,
                     labels = biv_labels
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()
plot(biv_plot)

# By climate region

biv_long <- df_heatwave_agg_year_clim_region %>%
  group_by(Year, clim_div)%>%
  summarize(across(all_of(biv_names), sum, na.rm = TRUE, .names = "{.col}"))%>%
  pivot_longer(cols = c("biv290","biv390","biv490","biv295","biv395","biv495","biv299","biv399","biv499"), names_to = "Variable", values_to = "Value")

df_plot <- biv_long %>%
  filter(clim_div == "NC CENTRAL COASTAL PLAIN")
central_coastal_plain <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Central Coastal Plain (n=123)", x = "Year", y = "Value") +
  scale_color_manual(values = biv_colors,
                     labels = biv_labels) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal() +
  theme(legend.position = "none")

df_plot <- biv_long %>%
  filter(clim_div == "NC CENTRAL PIEDMONT")
central_piedmont <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Central Piedmont (n=85)", x = "Year", y = "Value") +
  scale_color_manual(values = biv_colors,
                     labels = biv_labels) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal() +
  theme(legend.position = "none")

df_plot <- biv_long %>%
  filter(clim_div == "NC NORTHERN COASTAL PLAIN")
northern_coastal_plain <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Northern Coastal Plain (n=76)", x = "Year", y = "Value") +
  scale_color_manual(values = biv_colors,
                     labels = biv_labels)+
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()+
  theme(legend.position = "none")

df_plot <- biv_long %>%
  filter(clim_div == "NC NORTHERN MOUNTAINS")
northern_mountains <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Northern Mountains (n=57)", x = "Year", y = "Value") +
  scale_color_manual(values = biv_colors,
                     labels = biv_labels) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()+
  theme(legend.position = "none")

df_plot <- biv_long %>%
  filter(clim_div == "NC NORTHERN PIEDMONT")
northern_piedmont <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Northern Piedmont (n=84)", x = "Year", y = "Value") +
  scale_color_manual(values = biv_colors,
                     labels = biv_labels
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()+
  theme(legend.position = "none")

df_plot <- biv_long %>%
  filter(clim_div == "NC SOUTHERN COASTAL PLAIN")
southern_coastal_plain <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Southern Coastal Plain (n=118)", x = "Year", y = "Value") +
  scale_color_manual(values = biv_colors,
                     labels = biv_labels
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()+
  theme(legend.position = "none")

df_plot <- biv_long %>%
  filter(clim_div == "NC SOUTHERN MOUNTAINS")
southern_mountains <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Southern Mountains (n=93)", x = "Year", y = "Value") +
  scale_color_manual(values = biv_colors,
                     labels = biv_labels
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()+
  theme(legend.position = "none")

df_plot <- biv_long %>%
  filter(clim_div == "NC SOUTHERN PIEDMONT")
southern_piedmont <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Southern Piedmont (n=134)", x = "Year", y = "Value") +
  scale_color_manual(values = biv_colors,
                     labels = biv_labels
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()+
  theme(legend.position = "bottom") +    # Set legend position to bottom
  guides(color = guide_legend(ncol = 3))
legend <- get_legend(southern_piedmont)
southern_piedmont <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Southern Piedmont (n=134)", x = "Year", y = "Value") +
  scale_color_manual(values = biv_colors,
                     labels = biv_labels
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()+
  theme(legend.position = "none")

combined_plot <- ggarrange(
  ggarrange(northern_coastal_plain, central_coastal_plain, southern_coastal_plain, ncol = 3),
  ggarrange(northern_piedmont, central_piedmont, southern_piedmont, ncol = 3),
  ggarrange(northern_mountains, southern_mountains, legend, ncol = 3),
  nrow = 3  # Specify that there are 3 rows in total
)

annotated_plot <- annotate_figure(
  combined_plot,
  top = text_grob("Count of Bivariate Heatwave Events by NC Climate Region, 2010-2020", size = 16, face = "bold")
)

print(annotated_plot)

#### HI ####
hi_names <- c("hi290","hi390","hi490","hi295","hi395","hi495","hi299","hi399","hi499")
hi_colors <- c(
  "hi290" = "lightblue",
  "hi390" = "blue",
  "hi490" = "darkblue",
  "hi295" = "lightgreen",
  "hi395" = "green",
  "hi495" = "darkgreen",
  "hi299" = "pink",
  "hi399" = "red",
  "hi499" = "darkred"
)
hi_labels = c(
  "hi290" = "2-Day 90th Percentile",
  "hi390" = "3-Day 90th Percentile",
  "hi490" = "4-Day 90th Percentile",
  "hi295" = "2-Day 95th Percentile",
  "hi395" = "3-Day 95th Percentile",
  "hi495" = "4-Day 95th Percentile",
  "hi299" = "2-Day 99th Percentile",
  "hi399" = "3-Day 99th Percentile",
  "hi499" = "4-Day 99th Percentile")
hi_long <- heat_index %>%
  mutate(Year=year(Date))%>%
  group_by(Year)%>%
  summarize(across(all_of(hi_names), sum, na.rm = TRUE, .names = "{.col}"))%>%
  pivot_longer(cols = c("hi290","hi390","hi490","hi295","hi395","hi495","hi299","hi399","hi499"), names_to = "Variable", values_to = "Value")
hi_long$Variable <- factor(hi_long$Variable, levels = c("hi290","hi390","hi490","hi295","hi395","hi495","hi299","hi399","hi499"))

hi_plot <- ggplot(hi_long, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Count of Heat Index Heatwave Events by Year", x = "Year", y = "Value") +
  scale_color_manual(values = hi_colors,
                     labels = hi_labels
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()
plot(hi_plot)

# By climate region 

hi_long <- df_heatwave_agg_year_clim_region %>%
  group_by(Year, clim_div)%>%
  summarize(across(all_of(hi_names), sum, na.rm = TRUE, .names = "{.col}"))%>%
  pivot_longer(cols = c("hi290","hi390","hi490","hi295","hi395","hi495","hi299","hi399","hi499"), names_to = "Variable", values_to = "Value")

df_plot <- hi_long %>%
  filter(clim_div == "NC CENTRAL COASTAL PLAIN")
central_coastal_plain <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Central Coastal Plain (n=123)", x = "Year", y = "Value") +
  scale_color_manual(values = hi_colors,
                     labels = hi_labels) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal() +
  theme(legend.position = "none")

df_plot <- hi_long %>%
  filter(clim_div == "NC CENTRAL PIEDMONT")
central_piedmont <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Central Piedmont (n=85)", x = "Year", y = "Value") +
  scale_color_manual(values = hi_colors,
                     labels = hi_labels) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal() +
  theme(legend.position = "none")

df_plot <- hi_long %>%
  filter(clim_div == "NC NORTHERN COASTAL PLAIN")
northern_coastal_plain <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Northern Coastal Plain (n=76)", x = "Year", y = "Value") +
  scale_color_manual(values = hi_colors,
                     labels = hi_labels)+
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()+
  theme(legend.position = "none")

df_plot <- hi_long %>%
  filter(clim_div == "NC NORTHERN MOUNTAINS")
northern_mountains <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Northern Mountains (n=57)", x = "Year", y = "Value") +
  scale_color_manual(values = hi_colors,
                     labels = hi_labels) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()+
  theme(legend.position = "none")

df_plot <- hi_long %>%
  filter(clim_div == "NC NORTHERN PIEDMONT")
northern_piedmont <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Northern Piedmont (n=84)", x = "Year", y = "Value") +
  scale_color_manual(values = hi_colors,
                     labels = hi_labels
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()+
  theme(legend.position = "none")

df_plot <- hi_long %>%
  filter(clim_div == "NC SOUTHERN COASTAL PLAIN")
southern_coastal_plain <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Southern Coastal Plain (n=118)", x = "Year", y = "Value") +
  scale_color_manual(values = hi_colors,
                     labels = hi_labels
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()+
  theme(legend.position = "none")

df_plot <- hi_long %>%
  filter(clim_div == "NC SOUTHERN MOUNTAINS")
southern_mountains <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Southern Mountains (n=93)", x = "Year", y = "Value") +
  scale_color_manual(values = hi_colors,
                     labels = hi_labels
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()+
  theme(legend.position = "none")

df_plot <- hi_long %>%
  filter(clim_div == "NC SOUTHERN PIEDMONT")
southern_piedmont <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Southern Piedmont (n=134)", x = "Year", y = "Value") +
  scale_color_manual(values = hi_colors,
                     labels = hi_labels
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()+
  theme(legend.position = "bottom") +    # Set legend position to bottom
  guides(color = guide_legend(ncol = 3))
legend <- get_legend(southern_piedmont)
southern_piedmont <- ggplot(df_plot, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Southern Piedmont (n=134)", x = "Year", y = "Value") +
  scale_color_manual(values = hi_colors,
                     labels = hi_labels
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()+
  theme(legend.position = "none")

combined_plot <- ggarrange(
  ggarrange(northern_coastal_plain, central_coastal_plain, southern_coastal_plain, ncol = 3),
  ggarrange(northern_piedmont, central_piedmont, southern_piedmont, ncol = 3),
  ggarrange(northern_mountains, southern_mountains, legend, ncol = 3),
  nrow = 3  # Specify that there are 3 rows in total
)

annotated_plot <- annotate_figure(
  combined_plot,
  top = text_grob("Count of Heat Index Heatwave Events by NC Climate Region, 2010-2020", size = 16, face = "bold")
)
print(annotated_plot)

#### EHF ####

ehf_names <- c("ehflow", "ehfmod", "ehfhigh", "ehfsev", "ehfextr", "ehfhw")
ehf_colors <- c(
  "ehflow" = "blue",
  "ehfmod" = "darkgreen",
  "ehfhw" = "brown",
  "ehfhigh" = "orange",
  "ehfsev" = "pink",
  "ehfextr" = "red"
)
ehf_long <- ehf %>%
  mutate(Year=year(Date))%>%
  group_by(Year)%>%
  summarize(across(all_of(ehf_names), sum, na.rm = TRUE, .names = "{.col}"))%>%
  pivot_longer(cols = c("ehflow", "ehfmod", "ehfhigh", "ehfsev", "ehfextr", "ehfhw"), names_to = "Variable", values_to = "Value")
ehf_long$Variable <- factor(ehf_long$Variable, levels = c("ehflow", "ehfmod", "ehfhigh", "ehfsev", "ehfextr", "ehfhw"))

ehf_plot <- ggplot(ehf_long, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +        # Draw lines
  geom_point() +               # Add points at each data value
  labs(title = "Count of EHF Heatwave Events by Year", x = "Year", y = "Value") +
  scale_color_manual(values = ehf_colors,
                     labels = c(
                       "ehflow" = "Low Intensity",
                       "ehfmod" = "Moderate Intensity",
                       "ehfhigh" = "High Intensity",
                       "ehfsev" = "Severe",
                       "ehfextr" = "Extreme",
                       "ehfhw" = "All EHF Heatwaves")
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Set x-axis to show every year from 2010 to 2020
  theme_minimal()
plot(ehf_plot)

ehf_sum <- ehf_sum %>%
  summarize(across(starts_with("ehf"), sum, na.rm = TRUE))

#### By RUCA, geo region, clim div etc. ####

dat <- read.csv("C:/Users/ulric/OneDrive - University of North Carolina at Chapel Hill/Data/Regional_Vars.csv")
clim_region <- read.csv("project_files/calculate_metrics/NC_ZCTA_Climate_Divisions.csv")%>%
  filter(!(grepl("^VA", clim_div) | grepl("^SC", clim_div)))

hw <- read_sf("shared_data/extreme_heat_metrics/shapefiles/NC_ZCTA_Heatwaves_All_Definitions_v2.shp")%>%
  select(-ALAND10, -AWATER10, -ehfextr)%>% # Remove extreme EHF since there were none during this period
  select_if(is.numeric)

nc_zctas <- hw %>%
  left_join(dat, by=c('Zip'='zip'))%>%
  left_join(clim_region, by=c('Zip'))

#write_sf(nc_zctas, "shared_data/extreme_heat_metrics/shapefiles/NC_ZCTA_Heatwaves_w_regional_vars.shp")

geo_grp <- nc_zctas %>%
  group_by(geo_region) %>%
  summarize(
    across(
      c(ends_with("90"), ends_with("95"), ends_with("99"), starts_with("ehf")),
      ~ sum(.x, na.rm = TRUE)  # Sum function applied to each variable
    ),
    .groups = 'drop'  # Prevents warning about ungrouping
  )

ruca_grp <- nc_zctas %>%
  group_by(RUCA_Cat) %>%
  summarize(
    across(
      c(ends_with("90"), ends_with("95"), ends_with("99"), starts_with("ehf")),
      ~ sum(.x, na.rm = TRUE)  # Sum function applied to each variable
    ),
    .groups = 'drop'  # Prevents warning about ungrouping
  )

climdiv_grp <- nc_zctas %>%
  group_by(clim_div) %>%
  summarize(
    across(
      c(ends_with("90"), ends_with("95"), ends_with("99"), starts_with("ehf")),
      ~ sum(.x, na.rm = TRUE)  # Sum function applied to each variable
    ),
    .groups = 'drop'  # Prevents warning about ungrouping
  )


geo_count <- nc_zctas %>%
  group_by(clim_div)%>%
  summarize(n=n())

nc_grp <- pivot_longer(nc_grp, cols = c(ends_with("90"), ends_with("95"), ends_with("99"), starts_with("ehf")))
nc_grp <- pivot_wider(nc_grp, names_from = clim_div)

write.csv(nc_grp, "climdiv_sum.csv")


nc_zctas <- nc_zctas %>%
  left_join(clim_region, b=c('Zip'))

nc_grp <- nc_zctas %>%
  group_by(clim_div) %>%
  summarize(
    across(
      c(ends_with("90"), ends_with("95"), ends_with("99"), starts_with("ehf")),
      ~ sum(.x, na.rm = TRUE)  # Sum function applied to each variable
    ),
    .groups = 'drop'  # Prevents warning about ungrouping
  )

# Plots

dat <- read.csv("shared_data/extreme_heat_metrics/tables/hw_data_for_scatterplots.csv") %>%
  rename(metric = Metric.) %>%
  mutate(
    Intensity. = gsub("[^0-9.-]", "", Intensity.),  # Remove non-numeric characters
    Duration. = gsub("[^0-9.-]", "", Duration.),
    adjusted_count = gsub("[^0-9.-]", "", adjusted_count)
  ) %>%
  mutate(
    intensity = as.numeric(Intensity.),  # Convert to numeric
    duration = as.numeric(Duration.),
    adjusted_ct = as.numeric(adjusted_count)
  )

plotdat <- dat %>%
  filter(region_type == "clim_div" & grepl("^Bivariate", metric))

ggplot(plotdat, aes(x = intensity, y = adjusted_ct, color = Region)) +
  geom_point(size = 3) +  # Adjust size as needed
  labs(
    x = "Duration",
    y = "Intensity",
    title = "Scatterplot of Intensity vs. Duration by Region"
  ) +
  theme_minimal()


