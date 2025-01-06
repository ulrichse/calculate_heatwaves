
# Explore correlation among heatwave metrics

library(sf)
library(tidyverse)
library(corrr)
library(GGally)
library(corrplot)
library(gridExtra)
library(spdep)
setwd("C:/Users/ulric/OneDrive - University of North Carolina at Chapel Hill/Documents - Extreme Heat Events/")

hw <- read_sf("shared_data/extreme_heat_metrics/shapefiles/NC_ZCTA_Heatwaves_All_Definitions_v2.shp")%>%
  st_drop_geometry()%>%
  select(-ALAND10, -AWATER10, -ehfextr)%>% # Remove extreme EHF since there were none during this period
  select_if(is.numeric)

hw[is.na(hw)] <- 0

spearman_corr <- cor(hw, method = "spearman")
corrplot(spearman_corr, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8)

biv_hi <- hw %>%
  select(Zip, starts_with("biv"), starts_with("hi"))
spearman_corr <- cor(biv_hi, method = "spearman")
corrplot(spearman_corr, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8)

biv_tmean <- hw %>%
  select(Zip, starts_with("biv"), starts_with("tmean"))
spearman_corr <- cor(biv_tmean, method = "spearman")
corrplot(spearman_corr, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8)

hi_tmean <- hw %>%
  select(Zip, starts_with("hi"), starts_with("tmean"))
spearman_corr <- cor(hi_tmean, method = "spearman")
corrplot(spearman_corr, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8)

hi_ehf <- hw %>%
  select(Zip, starts_with("hi"), starts_with("ehf"))
spearman_corr <- cor(hi_ehf, method = "spearman")
plot_hi_ehf <- corrplot(spearman_corr, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8)

biv_ehf <- hw %>%
  select(Zip, starts_with("biv"), starts_with("ehf"))
spearman_corr <- cor(biv_ehf, method = "spearman")
plot_biv_ehf <- corrplot(spearman_corr, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8)

tmean_ehf <- hw %>%
  select(Zip, starts_with("tmean"), starts_with("ehf"))
spearman_corr <- cor(tmean_ehf, method = "spearman")
plot_tmean_ehf <- corrplot(spearman_corr, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8)

spearman_corr <- cor(hw, method = "spearman")
plot_hw <- corrplot(spearman_corr, method = "color", type = "full", tl.col = "black", tl.cex = 0.8, tl.srt = 45)
  
hw_filter <- hw %>%
  select(starts_with("biv"), starts_with("tmean"), starts_with("hi"), starts_with("ehf"))%>%
  select(ends_with("290"), ends_with("295"), ends_with("299"),
         ends_with("390"), ends_with("395"), ends_with("399"),
         ends_with("490"), ends_with("495"), ends_with("499"),
         starts_with("ehflow"), starts_with("ehfmod"), starts_with("ehfhigh")) 
spearman_corr <- cor(hw_filter, method = "spearman")
plot_hw <- corrplot(spearman_corr, method = "color", type = "full", tl.col = "black", tl.cex = 0.8, tl.srt = 45)

rural_filter <- hw %>%
  filter(RUCA_Cat == "Rural")%>%
  select(ends_with("390"), ends_with("395"), ends_with("399"), starts_with("ehf"))%>%
  select(starts_with("biv"), starts_with("tmean"), starts_with("hi"), starts_with("ehf"))

urban_filter <- hw %>%
  filter(RUCA_Cat == "Urban")%>%
  select(ends_with("390"), ends_with("395"), ends_with("399"), starts_with("ehf"))%>%
  select(starts_with("biv"), starts_with("tmean"), starts_with("hi"), starts_with("ehf"))

suburban_filter <- hw %>%
  filter(RUCA_Cat == "Suburban")%>%
  select(ends_with("390"), ends_with("395"), ends_with("399"), starts_with("ehf"))%>%
  select(starts_with("biv"), starts_with("tmean"), starts_with("hi"), starts_with("ehf"))

mtn_filter <- hw %>%
  filter(geo_region == "Western")%>%
  select(ends_with("390"), ends_with("395"), ends_with("399"), starts_with("ehf"))%>%
  select(starts_with("biv"), starts_with("tmean"), starts_with("hi"), starts_with("ehf"))

pmt_filter <- hw %>%
  filter(geo_region == "Piedmont")%>%
  select(ends_with("390"), ends_with("395"), ends_with("399"), starts_with("ehf"))%>%
  select(starts_with("biv"), starts_with("tmean"), starts_with("hi"), starts_with("ehf"))

coast_filter <- hw %>%
  filter(geo_region == "Eastern")%>%
  select(ends_with("390"), ends_with("395"), ends_with("399"), starts_with("ehf"))%>%
  select(starts_with("biv"), starts_with("tmean"), starts_with("hi"), starts_with("ehf"))

par(mfrow = c(2, 3), mar = c(2, 2, 5, 2))  # Increase top margin to 5 for title space

# Plot for urban filter
corrplot(cor(urban_filter, method = "spearman"), method = "color", type = "full", 
         tl.col = "black", tl.cex = 1.2, tl.srt = 45)
mtext("Urban", side = 3, line = 3, cex = 1.5, font = 2)  # Title moved further above plot

# Plot for suburban filter
corrplot(cor(suburban_filter, method = "spearman"), method = "color", type = "full", 
         tl.col = "black", tl.cex = 1.2, tl.srt = 45)
mtext("Suburban", side = 3, line = 3, cex = 1.5, font = 2)

# Plot for rural filter
corrplot(cor(rural_filter, method = "spearman"), method = "color", type = "full", 
         tl.col = "black", tl.cex = 1.2, tl.srt = 45)
mtext("Rural", side = 3, line = 3, cex = 1.5, font = 2)

# Plot for mountain filter
corrplot(cor(mtn_filter, method = "spearman"), method = "color", type = "full", 
         tl.col = "black", tl.cex = 1.2, tl.srt = 45)
mtext("Mountain", side = 3, line = 3, cex = 1.5, font = 2)

# Plot for piedmont filter
corrplot(cor(pmt_filter, method = "spearman"), method = "color", type = "full", 
         tl.col = "black", tl.cex = 1.2, tl.srt = 45)
mtext("Piedmont", side = 3, line = 3, cex = 1.5, font = 2)

# Plot for coast filter
corrplot(cor(coast_filter, method = "spearman"), method = "color", type = "full", 
         tl.col = "black", tl.cex = 1.2, tl.srt = 45)
mtext("Coast", side = 3, line = 3, cex = 1.5, font = 2)

# Plot the three correlation matrices
corrplot(cor(hi_ehf, method = "spearman"), method = "color", type = "upper", tl.col = "black", tl.cex = 0.8)
corrplot(cor(biv_ehf, method = "spearman"), method = "color", type = "upper", tl.col = "black", tl.cex = 0.8)
corrplot(cor(tmean_ehf, method = "spearman"), method = "color", type = "upper", tl.col = "black", tl.cex = 0.8)

# Explore by climate division

clim_region <- read.csv("project_files/calculate_metrics/NC_ZCTA_Climate_Divisions.csv")

clim_region <- clim_region %>%
  filter(!(grepl("^VA", clim_div) | grepl("^SC", clim_div)))

hw <- hw %>%
  left_join(clim_region, by=c('Zip'))

hw_list <- split(hw, hw$clim_div)

# Create an empty list to store the filtered dataframes
filtered_list <- list()

# Loop through each dataframe in hw_list (instead of using 'unique_values')
for (val in names(hw_list)) {
  
  # Get the dataframe for the current category (using the name of the list element)
  df_current <- hw_list[[val]]
  
  # Filter and select columns based on your conditions
  filtered_df <- df_current %>%
    select(ends_with("390"), ends_with("395"), ends_with("399"), starts_with("ehf")) %>%
    select(starts_with("biv"), starts_with("tmean"), starts_with("hi"), starts_with("ehf"))
  
  # Store the filtered dataframe in the list with a key name based on the category
  filtered_list[[paste0("filtered_", val)]] <- filtered_df
}


# Set up the plotting layout for a 4x2 grid
par(mfrow = c(4, 2), mar = c(4, 4, 4, 4))  # Adjust margins if needed (right margin increased)

# Loop through each dataframe in filtered_list
for (val in names(filtered_list)) {
  
  # Get the current dataframe from filtered_list
  df_current <- filtered_list[[val]]
  
  # Calculate the correlation matrix (Spearman method)
  corr_matrix <- cor(df_current, method = "spearman", use = "complete.obs")
  
  # Create the correlation plot
  corrplot(corr_matrix, method = "color", type = "full", 
           tl.col = "black", tl.cex = 1.2, tl.srt = 45)
  
  # Remove "filtered_" from the title using sub() and add it to the plot
  title_name <- sub("filtered_", "", val)  # Remove "filtered_" prefix
  
  # Move the title to the right side and make it smaller
  mtext(title_name, side = 2, line = 2, cex = 0.8, font = 2)  # Adjust 'line' and 'cex' as needed
}


# Bivariate regression models

hw <- read_sf("shared_data/extreme_heat_metrics/shapefiles/NC_ZCTA_Heatwaves_All_Definitions.shp")%>%
  select(-ALAND10, -AWATER10, -ehfextr)
hw[is.na(hw)] <- 0

model <- lm(biv395 ~ hi395, data = hw)
hw$residuals <- residuals(model)

ggplot(hw) +
  geom_sf(aes(fill = residuals)) +
  scale_fill_gradient2(low = "darkblue", high = "darkred", midpoint = 0) +
  theme_minimal() +
  labs(title = "Map of Regression Residuals", fill = "Residuals")

neighbors <- poly2nb(hw)  # Adjust if needed
lw <- nb2listw(neighbors)

# Test for spatial autocorrelation in residuals
moran.test(hw$residuals, lw)



hw_filter <- hw %>%
  select("biv290", "tmean290", "hi290", "biv390", "tmean390", "hi390", "biv490", "tmean490", "hi490")
spearman_corr <- cor(hw_filter, method = "spearman")
plot_hw_90 <- corrplot(spearman_corr, method = "color", type = "full", tl.col = "black", tl.cex = 0.8, tl.srt = 45)

hw_filter <- hw %>%
  select("biv295", "tmean295", "hi295", "biv395", "tmean395", "hi395", "biv495", "tmean495", "hi495")
spearman_corr <- cor(hw_filter, method = "spearman")
plot_hw_95 <- corrplot(spearman_corr, method = "color", type = "full", tl.col = "black", tl.cex = 0.8, tl.srt = 45)

hw_filter <- hw %>%
  select("biv299", "tmean299", "hi299", "biv399", "tmean399", "hi399", "biv499", "tmean499", "hi499")
spearman_corr <- cor(hw_filter, method = "spearman")
plot_hw_99 <- corrplot(spearman_corr, method = "color", type = "full", tl.col = "black", tl.cex = 0.8, tl.srt = 45)

hw_filter <- hw %>%
  select("biv290", "tmean290", "hi290", "biv295", "tmean295", "hi295", "biv299", "tmean299", "hi299")
spearman_corr <- cor(hw_filter, method = "spearman")
plot_hw_2day <- corrplot(spearman_corr, method = "color", type = "full", tl.col = "black", tl.cex = 0.8, tl.srt = 45)

hw_filter <- hw %>%
  select("biv390", "tmean390", "hi390", "biv395", "tmean395", "hi395", "biv399", "tmean399", "hi399")
spearman_corr <- cor(hw_filter, method = "spearman")
plot_hw_3day <- corrplot(spearman_corr, method = "color", type = "full", tl.col = "black", tl.cex = 0.8, tl.srt = 45)

hw_filter <- hw %>%
  select("biv490", "tmean490", "hi490", "biv495", "tmean495", "hi495", "biv499", "tmean499", "hi499")
spearman_corr <- cor(hw_filter, method = "spearman")
plot_hw_4day <- corrplot(spearman_corr, method = "color", type = "full", tl.col = "black", tl.cex = 0.8, tl.srt = 45)

# Set up the plotting area to display 3 plots per row, 2 rows
par(mfrow = c(2, 3))

# Generate each plot in sequence
corrplot(cor(hw %>% select("biv290", "tmean290", "hi290", "biv390", "tmean390", "hi390", "biv490", "tmean490", "hi490"), method = "spearman"), method = "color", type = "full", tl.col = "black", tl.cex = 0.8, tl.srt = 45)
mtext("90%", side = 3, line = 2, cex = 1, font = 2)
corrplot(cor(hw %>% select("biv295", "tmean295", "hi295", "biv395", "tmean395", "hi395", "biv495", "tmean495", "hi495"), method = "spearman"), method = "color", type = "full", tl.col = "black", tl.cex = 0.8, tl.srt = 45)
mtext("95%", side = 3, line = 2, cex = 1, font = 2)
corrplot(cor(hw %>% select("biv299", "tmean299", "hi299", "biv399", "tmean399", "hi399", "biv499", "tmean499", "hi499"), method = "spearman"), method = "color", type = "full", tl.col = "black", tl.cex = 0.8, tl.srt = 45)
mtext("99%", side = 3, line = 2, cex = 1, font = 2)
corrplot(cor(hw %>% select("biv290", "tmean290", "hi290", "biv295", "tmean295", "hi295", "biv299", "tmean299", "hi299"), method = "spearman"), method = "color", type = "full", tl.col = "black", tl.cex = 0.8, tl.srt = 45)
mtext("2 Days", side = 3, line = 2, cex = 1, font = 2)
corrplot(cor(hw %>% select("biv390", "tmean390", "hi390", "biv395", "tmean395", "hi395", "biv399", "tmean399", "hi399"), method = "spearman"), method = "color", type = "full", tl.col = "black", tl.cex = 0.8, tl.srt = 45)
mtext("3 Days", side = 3, line = 2, cex = 1, font = 2)
corrplot(cor(hw %>% select("biv490", "tmean490", "hi490", "biv495", "tmean495", "hi495", "biv499", "tmean499", "hi499"), method = "spearman"), method = "color", type = "full", tl.col = "black", tl.cex = 0.8, tl.srt = 45)
mtext("4 Days", side = 3, line = 2, cex = 1, font = 2)
