#Scottish A&E data - Choropleth

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

##CHLOROPETH

#Creating a map of the HBs with population estimates for 2022

# Path to the downloaded shapefile
shapefile_path <- "/Users/hui_p/Documents/AandE_test/AandE_test/Rawdata/SG_NHS_HealthBoards_2019/SG_NHS_HealthBoards_2019.shp"

# Read the shapefile
scottish_health_boards <- st_read(shapefile_path)
labels_data <- scottish_health_boards %>% 
  mutate(geometry = st_centroid(geometry))

HBUpdatespopulation_estimate_HBname2022 <- HBUpdatespopulation_estimate_HBname %>% 
  filter(Year == "2022")

# Assuming your health boards' shapefile has an ID or name column called 'HBName'
HBUpdatespopulation_estimate_HBname2022_data <- data.frame(
  HBName = c("Ayrshire and Arran", "Borders", "Dumfries and Galloway", "Forth Valley", "Grampian", "Highland", "Lothian", "Orkney", "Shetland", "Western Isles", "Fife", "Tayside", "Greater Glasgow and Clyde", "Lanarkshire"),
  Population2022 = c(368837, 116520, 148892, 306927, 588868, 325310, 925939, 22731, 22968, 26606, 376267, 419000, 1192485, 666129)
)

# Ensure the merge key exists in both datasets
scottish_health_boards_pop2022 <- scottish_health_boards %>%
  left_join(HBUpdatespopulation_estimate_HBname2022_data, by = "HBName")  # 'by' specifies the column to join by

# Now, you can plot your data

ScottishMapHBPopulation2022 <- ggplot(data = scottish_health_boards_pop2022) + 
  geom_sf(aes(fill = Population2022)) +  # Plot the health board regions with fill mapped to your data
  geom_sf_text(data = labels_data, aes(label = HBName), size = 3) +  # Add labels
  #scale_fill_viridis_c() +  # Optional: Use a nice color scale
  scale_fill_continuous(labels = label_comma(), type="viridis", direction = -1, guide_legend(title = "Population \nestimate")) +
  labs(
    #title = "Population estimate in 2022 by Scottish Health Board", 
    fill = "AllAges") +
  #theme_minimal()
  theme_void() 
save_plot("Output/ScottishMapHBPopulation2022.svg", fig = ScottishMapHBPopulation2022, width = 12, height = 14)

#Creating a map of the HBs in 2022 with percent within 4 hours

#didn't work when I left_join with scottish_health_boards, had to manually input the data
HBwaiting4Hr2022 <- ae_4HwaitingbyHB_2022 %>% select(NHSBoardName, percent4Hr)
colnames(HBwaiting4Hr2022)<- c("HBName","percent4Hr")

#had to manually input the data for it to wokr
aeHBwaiting4Hr2022 <- data.frame(
  HBName = c("Ayrshire and Arran", "Borders", "Dumfries and Galloway", "Fife","Forth Valley", "Grampian", "Greater Glasgow and Clyde", "Highland", "Lanarkshire", "Lothian", "Orkney", "Shetland", "Tayside", "Western Isles"),
  percent4Hr = c(69.5, 64.5, 79.7, 73.0, 64.7, 70.9, 71.1, 85.4, 58.2, 63.4, 91.4, 94.0, 90.4, 97.8))

scottish_health_boards4Hwaiting2022 <- scottish_health_boards %>%
  left_join(aeHBwaiting4Hr2022, by="HBName")

#Chloropeth of Scottish 4 hour waiting in 2022
png(file = "Output/ScottishMapHB4Hwaiting2022.png")
ggplot(data = scottish_health_boards4Hwaiting2022) + 
  geom_sf(aes(fill = percent4Hr)) +  # Plot the health board regions with fill mapped to your data
  geom_sf_text(data = labels_data, aes(label = HBName), size = 3) +  # Add labels
  #scale_fill_viridis_c() +  # Optional: Use a nice color scale
  scale_fill_continuous(labels = label_comma(), type="viridis", 
                        direction = -1, 
                        limits=c(55, 100),
                        guide_legend(title = "% Patients seen \nwithin 4 hours")) +
  labs(title = "% Patients seen within 4 hours in 2022 by Scottish Health Board", fill = "percent4Hr") +
  #theme_minimal()
  theme_void()
dev.off()

#Saving as svg Chloropeth of Scottish 4 hour waiting in 2022

ScottishMapHB4Hwaiting2022 <- ggplot(data = scottish_health_boards4Hwaiting2022) + 
  geom_sf(aes(fill = percent4Hr)) +  # Plot the health board regions with fill mapped to your data
  geom_sf_text(data = labels_data, aes(label = HBName), size = 3) +  # Add labels
  #scale_fill_viridis_c() +  # Optional: Use a nice color scale
  scale_fill_continuous(labels = label_comma(), type="viridis", 
                        direction = -1, 
                        limits=c(55, 100),
                        guide_legend(title = "% Patients seen \nwithin 4 hours")) +
  labs(
    #title = "% Patients seen within 4 hours in 2022 \nby Scottish Health Board", 
    fill = "percent4Hr") +
  #theme_minimal()
  theme_void()
save_plot("Output/ScottishMapHB4Hwaiting2022.svg", fig = ScottishMapHB4Hwaiting2022, width = 12, height = 14)

#Creating a map of the HBs in 2012 with percent within 4 hours

#Creating a dataframe with 2012 percent within 4 hours data

ae_4HwaitingbyHB_2012 <-ae_4HwaitingbyHB %>% 
  filter(Year=="2012") 

ae_4HwaitingbyHB_2012<- ae_4HwaitingbyHB_2012 %>% 
  select(NHSBoardName, NumberOfAttendancesAll, NumberWithin4HoursAll) %>% 
  group_by(NHSBoardName)%>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll),
            NumberWithin4HoursAll=sum(NumberWithin4HoursAll))

#counting the percentages for within 4 hours
ae_4HwaitingbyHB_2012$percent4Hr<-ae_4HwaitingbyHB_2012$NumberWithin4HoursAll/ae_4HwaitingbyHB_2012$NumberOfAttendancesAll
ae_4HwaitingbyHB_2012$percent4Hr<-ae_4HwaitingbyHB_2012$percent4Hr*100

#rounding up percent4Hr to 1 decimal point
ae_4HwaitingbyHB_2012$percent4Hr<-round(ae_4HwaitingbyHB_2012$percent4Hr,digits = 3)

#Creating the dataframe with percent within 4 hours by HB
aeHBwaiting4Hr2012 <- data.frame(
  HBName = c("Ayrshire and Arran", "Borders", "Dumfries and Galloway", "Fife","Forth Valley", "Grampian", "Greater Glasgow and Clyde", "Highland", "Lanarkshire", "Lothian", "Orkney", "Shetland", "Tayside", "Western Isles"),
  percent4Hr = c(93.4, 96.7, 94.2, 94.6, 92.8, 96.5, 94.4, 97.9, 93.0, 91.5, 98.6, 99.4, 98.9, 98.1))

scottish_health_boards4Hwaiting2012 <- scottish_health_boards %>%
  left_join(aeHBwaiting4Hr2012, by="HBName")

#Chloropeth of Scottish 4 hour waiting in 2012
png(file = "Output/ScottishMapHB4Hwaiting2012.png")
ggplot(data = scottish_health_boards4Hwaiting2012) + 
  geom_sf(aes(fill = percent4Hr)) +  # Plot the health board regions with fill mapped to your data
  geom_sf_text(data = labels_data, aes(label = HBName), size = 3) +  # Add labels
  #scale_fill_viridis_c() +  # Optional: Use a nice color scale
  scale_fill_continuous(labels = label_comma(), type="viridis", 
                        direction = -1, 
                        limits=c(55, 100),
                        guide_legend(title = "% Patients seen \nwithin 4 hours")) +
  labs(title = "% Patients seen within 4 hours in 2012 by Scottish Health Board", fill = "percent4Hr") +
  #theme_minimal()
  theme_void()
dev.off()

#Saving as svg Chloropeth of Scottish 4 hour waiting in 2012

ScottishMapHB4Hwaiting2012 <- ggplot(data = scottish_health_boards4Hwaiting2012) + 
  geom_sf(aes(fill = percent4Hr)) +  # Plot the health board regions with fill mapped to your data
  geom_sf_text(data = labels_data, aes(label = HBName), size = 3) +  # Add labels
  #scale_fill_viridis_c() +  # Optional: Use a nice color scale
  scale_fill_continuous(labels = label_comma(), type="viridis", 
                        direction = -1, 
                        limits=c(55, 100),
                        guide_legend(title = "% Patients seen \nwithin 4 hours")) +
  labs(
    #title = "% Patients seen within 4 hours in 2012 \nby Scottish Health Board", 
    fill = "percent4Hr") +
  #theme_minimal()
  theme_void()
save_plot("Output/ScottishMapHB4Hwaiting2012.svg", fig = ScottishMapHB4Hwaiting2012, width = 12, height = 14)


#Chloropeth of Scottish attendance rate per 1000pop in 2022

#Creating a dataframe with 2022 attendance rate data
ae_byboard_Updatedpopulation_2022 <- ae_byboard_Updatedpopulation %>% 
  filter(Year=="2022")

ae_byboard_Updatedpopulation_2022<- ae_byboard_Updatedpopulation_2022 %>% 
  select(HBName, Year, NumberOfAttendancesAll, AllAges) %>% 
  group_by(HBName, Year) %>%
  summarise(NumberOfAttendancesAll = sum(NumberOfAttendancesAll),
            AllAges = sum(AllAges))

#Calculating attendance rate per pop in 2022 using Chris's updated estimate
ae_byboard_Updatedpopulation_2022$attendanceperpop<-ae_byboard_Updatedpopulation_2022$NumberOfAttendancesAll/ae_byboard_Updatedpopulation_2022$AllAges
#Calculating attendance per 1,000 population using Chris's updated population estimate
ae_byboard_Updatedpopulation_2022$attendanceper1000pop<-ae_byboard_Updatedpopulation_2022$attendanceperpop*1000

#rounding up attendanceper1000pop to 1 decimal point
ae_byboard_Updatedpopulation_2022$attendanceper1000pop<-round(ae_byboard_Updatedpopulation_2022$attendanceper1000pop,digits = 3)

#Creating the dataframe with attendanceper1000pop in 2022 by HB
ae_byboard_Updatedpopulation_2022 <- data.frame(
  HBName = c("Ayrshire and Arran", "Borders", "Dumfries and Galloway", "Fife","Forth Valley", "Grampian", "Greater Glasgow and Clyde", "Highland", "Lanarkshire", "Lothian", "Orkney", "Shetland", "Tayside", "Western Isles"),
  attendanceper1000pop = c(21.5, 21.2, 25.7, 18.7, 22.2, 16.7, 27.9, 24.1, 24.7, 24.6, 25.1, 27.3, 18.3, 22.9))

scottish_health_boardsUpdatedattendancerate2022 <- scottish_health_boards %>%
  left_join(ae_byboard_Updatedpopulation_2022, by="HBName")

#Chloropeth of Scottish attendance rate in 2022 using Chris's updated population estimate
png(file = "Output/ScottishMapUpdatedattendancerate2022.png")
ggplot(data = scottish_health_boardsUpdatedattendancerate2022) + 
  geom_sf(aes(fill = attendanceper1000pop)) +  # Plot the health board regions with fill mapped to your data
  geom_sf_text(data = labels_data, aes(label = HBName), size = 3) +  # Add labels
  #scale_fill_viridis_c() +  # Optional: Use a nice color scale
  scale_fill_continuous(labels = label_comma(), type="viridis", 
                        direction = -1, 
                        limits=c(10, 35),
                        guide_legend(title = "Attendance rate \nper 1000 population")) +
  labs(title = "Attendance rate per 1000 population in 2022 by Scottish health board", fill = "attendanceper1000pop") +
  #theme_minimal()
  theme_void()
dev.off()

#Saving as svg Chloropeth of Scottish attendance rate in 2022 using Chris's updated population estimate

ScottishMapUpdatedattendancerate2022 <- ggplot(data = scottish_health_boardsUpdatedattendancerate2022) + 
  geom_sf(aes(fill = attendanceper1000pop)) +  # Plot the health board regions with fill mapped to your data
  geom_sf_text(data = labels_data, aes(label = HBName), size = 3) +  # Add labels
  #scale_fill_viridis_c() +  # Optional: Use a nice color scale
  scale_fill_continuous(labels = label_comma(), type="viridis", 
                        direction = -1, 
                        limits=c(10, 35),
                        guide_legend(title = "Attendance rate \nper 1000 population")) +
  #labs(title = "Attendance rate per 1000 population in 2022 \nby Scottish health board", fill = "attendanceper1000pop") +
  #theme_minimal()
  theme_void()
save_plot("Output/ScottishMapUpdatedattendancerate2022.svg", fig = ScottishMapUpdatedattendancerate2022, width = 12, height = 14)


#Chloropeth of Scottish attendance rate per 1000pop in 2012

#Creating a dataframe with 2012 attendance rate data
ae_byboard_Updatedpopulation_2012 <- ae_byboard_Updatedpopulation %>% 
  filter(Year=="2012")

ae_byboard_Updatedpopulation_2012<- ae_byboard_Updatedpopulation_2012 %>% 
  select(HBName, Year, NumberOfAttendancesAll, AllAges) %>% 
  group_by(HBName, Year) %>%
  summarise(NumberOfAttendancesAll = sum(NumberOfAttendancesAll),
            AllAges = sum(AllAges))

#Calculating attendance rate per pop in 2012 using Chris's updated estimate
ae_byboard_Updatedpopulation_2012$attendanceperpop<-ae_byboard_Updatedpopulation_2012$NumberOfAttendancesAll/ae_byboard_Updatedpopulation_2012$AllAges
#Calculating attendance per 1,000 population using Chris's updated population estimate
ae_byboard_Updatedpopulation_2012$attendanceper1000pop<-ae_byboard_Updatedpopulation_2012$attendanceperpop*1000

#rounding up attendanceper1000pop to 1 decimal point
ae_byboard_Updatedpopulation_2012$attendanceper1000pop<-round(ae_byboard_Updatedpopulation_2012$attendanceper1000pop,digits = 3)

#Creating the dataframe with attendanceper1000pop in 2012 by HB
ae_byboard_Updatedpopulation_2012 <- data.frame(
  HBName = c("Ayrshire and Arran", "Borders", "Dumfries and Galloway", "Fife","Forth Valley", "Grampian", "Greater Glasgow and Clyde", "Highland", "Lanarkshire", "Lothian", "Orkney", "Shetland", "Tayside", "Western Isles"),
  attendanceper1000pop = c(27.2, 19.6, 26.2, 19.9, 20.9, 21.3, 34.0, 25.0, 24.8, 24.8, 12.4, 28.1, 22.7, 25.8))

scottish_health_boardsUpdatedattendancerate2012 <- scottish_health_boards %>%
  left_join(ae_byboard_Updatedpopulation_2012, by="HBName")

##Trying to nudge the label for Orkney for ScottishMapUpdatedattendancerate2012 so it doesn't overlap, code doesn't work. 
#It doesn't like the coord_sf line and if I leave it out, it doesn't do anything to the Orkney label
#scottish_health_boardsUpdatedattendancerate2012nudge <-scottish_health_boardsUpdatedattendancerate2012 %>% 
#  mutate(nudge_x_Orkney=ifelse(HBName=='Orkney', -4,0),
#         nudge_y_Orkney=ifelse(HBName=='Orkney', 2,0))
#       #  ,labels_data = ifelse(HBName=='Orkney', 'Orkney', labels_data))


#ggplot(data = scottish_health_boardsUpdatedattendancerate2012nudge) + 
#  geom_sf(aes(fill = attendanceper1000pop)) +  # Plot the health board regions with fill mapped to your data
#  coord_sf(xlim = c(-10,0), ylim = c(50,62)) +
#  geom_sf_text(data = labels_data, aes(label = HBName), size = 3
#               ,nudge_x=scottish_health_boardsUpdatedattendancerate2012nudge$nudge_x_Orkney, nudge_y=scottish_health_boardsUpdatedattendancerate2012nudge$nudge_y_Orkney
#               ) +  # Add labels
#  #scale_fill_viridis_c() +  # Optional: Use a nice color scale
#  scale_fill_continuous(labels = label_comma(), type="viridis", 
#                        direction = -1, 
#                        limits=c(10, 35),
#                        guide_legend(title = "Attendance rate \nper 1000 population")) +
#  labs(title = "Attendance rate per 1000 population in 2012 by Scottish health board", fill = "attendanceper1000pop") 
#+
#  #theme_minimal()
#  theme_void()

#Chloropeth of Scottish attendance rate in 2012 using Chris's updated population estimate
png(file = "Output/ScottishMapUpdatedattendancerate2012.png")
ggplot(data = scottish_health_boardsUpdatedattendancerate2012) + 
  geom_sf(aes(fill = attendanceper1000pop)) +  # Plot the health board regions with fill mapped to your data
  geom_sf_text(data = labels_data, aes(label = HBName), size = 3) +  # Add labels
  #scale_fill_viridis_c() +  # Optional: Use a nice color scale
  scale_fill_continuous(labels = label_comma(), type="viridis", 
                        direction = -1, 
                        limits=c(10, 35),
                        guide_legend(title = "Attendance rate \nper 1000 population")) +
  labs(title = "Attendance rate per 1000 population in 2012 by Scottish health board", fill = "attendanceper1000pop") +
  #theme_minimal()
  theme_void()
dev.off()

#Saving as svg Chloropeth of Scottish attendance rate in 2012 using Chris's updated population estimate

ScottishMapUpdatedattendancerate2012 <- ggplot(data = scottish_health_boardsUpdatedattendancerate2012) + 
  geom_sf(aes(fill = attendanceper1000pop)) +  # Plot the health board regions with fill mapped to your data
  geom_sf_text(data = labels_data, aes(label = HBName), size = 3) +  # Add labels
  #scale_fill_viridis_c() +  # Optional: Use a nice color scale
  scale_fill_continuous(labels = label_comma(), type="viridis", 
                        direction = -1, 
                        limits=c(10, 35),
                        guide_legend(title = "Attendance rate \nper 1000 population")) +
  #labs(title = "Attendance rate per 1,000 population in 2012 \nby Scottish health board", fill = "attendanceper1000pop") +
  #theme_minimal()
  theme_void()
save_plot("Output/ScottishMapUpdatedattendancerate2012.svg", fig = ScottishMapUpdatedattendancerate2012, width = 12, height = 14)

