#Scottish A&E Covid Choropleth

#install.packages("tidyverse")
#install.packages("here")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("sf")
#install.packages("sjPlot")

library(tidyverse)
library(here)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)
library(scales)
library(sjPlot)

# Path to the downloaded shapefile
shapefile_path <- "/Users/hui_p/Documents/AandE_test/AandE_test/Rawdata/SG_NHS_HealthBoards_2019/SG_NHS_HealthBoards_2019.shp"

# Read the shapefile
scottish_health_boards <- st_read(shapefile_path)
labels_data <- scottish_health_boards %>% 
  mutate(geometry = st_centroid(geometry))

Covid_monthlyae_popest2022 <- Covid_monthlyae_popest %>% 
  filter(Year == "2022")

