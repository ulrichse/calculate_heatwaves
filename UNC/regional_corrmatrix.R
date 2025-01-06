

library(sf)
library(tidyverse)
library(corrr)
library(GGally)
library(corrplot)
library(gridExtra)
library(spdep)
setwd("C:/Users/ulric/OneDrive - University of North Carolina at Chapel Hill/Documents - Extreme Heat Events/")

hw <- read_sf("shared_data/extreme_heat_metrics/shapefiles/NC_ZCTA_Heatwaves_All_Definitions_v2.shp")%>%
  select(-ends_with(c("75", "85")))%>%
  st_drop_geometry()%>%
  select(-ALAND10, -AWATER10, -ehfextr)%>% # Remove extreme EHF since there were none during this period
  select_if(is.numeric)

hw[is.na(hw)] <- 0

spearman_corr <- cor(hw, method = "spearman")
corrplot(spearman_corr, method = "color", type = "full", tl.col = "black", tl.cex = 0.8)

clim_region <- read.csv("project_files/calculate_metrics/NC_ZCTA_Climate_Divisions.csv")
clim_region <- clim_region %>%
  filter(!(grepl("^VA", clim_div) | grepl("^SC", clim_div)))

hw <- hw %>%
  left_join(clim_region, by=c('Zip'))

# Plot all climate divisions together

hw_list <- split(hw, hw$clim_div)

filtered_list <- list()

hw_list_filter <- hw_list[c("NC SOUTHERN COASTAL PLAIN", "NC CENTRAL COASTAL PLAIN", "NC NORTHERN COASTAL PLAIN")]

for (val in names(hw_list_filter)) {
  
  df_current <- hw_list[[val]]
  
  filtered_df <- df_current %>%
    select(ends_with("90"), ends_with("95"), ends_with("99"), starts_with("ehf")) %>%
    select(starts_with("biv"), starts_with("tmean"), starts_with("hi"), starts_with("ehf"))

  filtered_list[[paste0("filtered_", val)]] <- filtered_df
}


par(mfrow = c(2,2))  # Adjust margins if needed (right margin increased)


for (val in names(filtered_list)) {

  df_current <- filtered_list[[val]]

  corr_matrix <- cor(df_current, method = "spearman")

  corrplot(corr_matrix, method = "color", type = "full", 
           tl.col = "black", tl.cex = 1.2, tl.srt = 45)
http://127.0.0.1:19165/graphics/08a47369-fd24-4490-b4a8-68d64cc55b6a.png
  title_name <- sub("filtered_", "", val)  # Remove "filtered_" prefix

  mtext(title_name, side = 2, line = 2, cex = 1.2, font = 2)  # Adjust 'line' and 'cex' as needed
}


# Individual plots by climate region

dev.off()

par(mar = c(4, 4, 4, 4) + 0.1)
hw_filter <- hw%>%
  filter(clim_div=="NC NORTHERN MOUNTAINS")%>%
  select(ends_with("90"), ends_with("95"), ends_with("99"), starts_with("ehf")) %>%
  select(starts_with("biv"), starts_with("tmean"), starts_with("hi"), starts_with("ehflow"), starts_with("ehfhigh"), starts_with("ehfmod"))
spearman_corr <- cor(hw_filter, method = "spearman")
nccp <- corrplot(spearman_corr, method = "color", type = "full", 
                 tl.col = "black", tl.cex = 1, tl.srt = 45)
mtext("Northern Mountains", side = 2, line = 2, cex = 1.5, font = 2)



