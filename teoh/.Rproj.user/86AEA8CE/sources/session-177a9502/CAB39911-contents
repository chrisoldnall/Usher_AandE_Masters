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
shapefile_path <- "/Users/hui_p/Documents/GitHub/Usher_AandE_Masters/teoh/Rawdata/SG_NHS_HealthBoards_2019/SG_NHS_HealthBoards_2019.shp"

# Read the shapefile
scottish_health_boards <- st_read(shapefile_path)
labels_data <- scottish_health_boards %>% 
  mutate(geometry = st_centroid(geometry))

#Attendance rate 2022
#Only getting data for 2022, HBName and attendanceper1000pop column needed for choropleth
Covid_monthlyae_popest2022 <- Covid_monthlyae_popest %>% 
  filter(Year == "2022") %>% 
  select(HBName, attendanceper1000pop)

Covid_monthlyae_popest2022$attendanceper1000pop<-round(Covid_monthlyae_popest2022$attendanceper1000pop,digits = 1)

#Creating the dataframe with attendanceper1000pop in 2022 by HB because it wouldn't draw the diagram unless I manually created the dataframe from the data obtained
Covid_monthlyae_popest2022 <- data.frame(
  HBName = c("Ayrshire and Arran", "Borders", "Dumfries and Galloway", "Fife","Forth Valley", "Grampian", "Greater Glasgow and Clyde", "Highland", "Lanarkshire", "Lothian", "Orkney", "Shetland", "Tayside", "Western Isles"),
  attendanceper1000pop = c(21.7, 21.2, 26.3, 19.0, 22.5, 16.9, 28.2, 24.3, 24.6, 25.1, 26.0, 27.2, 18.5, 23.3))


Covidscottish_health_boardsattendancerate2022 <- scottish_health_boards %>%
  left_join(Covid_monthlyae_popest2022, by="HBName")

#Choropleth attendancerateper1000pop 2022
CovidScottishdattendancerate2022 <- ggplot(data = Covidscottish_health_boardsattendancerate2022) + 
  geom_sf(aes(fill = attendanceper1000pop)) +  # Plot the health board regions with fill mapped to your data
  geom_sf_text(data = labels_data, aes(label = HBName), size = 3) +  # Add labels
  #scale_fill_viridis_c() +  # Optional: Use a nice color scale
  scale_fill_continuous(labels = label_comma(), 
                        type="viridis", 
                        direction = -1, 
                        limits=c(10, 35),
                        #low = "yellow", high = "dodgerblue",
                        guide_legend(title = "Attendance rate \nper 1000 population")) +
  #labs(title = "Attendance rate per 1000 population in 2022 \nby Scottish health board", fill = "attendanceper1000pop") +
  #theme_minimal()
  theme_void()
save_plot("Output/CovidScottishattendancerate2022.svg", fig = CovidScottishdattendancerate2022, width = 12, height = 14)



#Attendance rate 2020
#Only getting data for 2020, HBName and attendanceper1000pop column needed for choropleth
Covid_monthlyae_popest2020 <- Covid_monthlyae_popest %>% 
  filter(Year == "2020") %>% 
  select(HBName, attendanceper1000pop)

Covid_monthlyae_popest2020$attendanceper1000pop<-round(Covid_monthlyae_popest2020$attendanceper1000pop,digits = 1)

#Creating the dataframe with attendanceper1000pop in 2020 by HB because it wouldn't draw the diagram unless I manually created the dataframe from the data obtained
Covid_monthlyae_popest2020 <- data.frame(
  HBName = c("Ayrshire and Arran", "Borders", "Dumfries and Galloway", "Fife","Forth Valley", "Grampian", "Greater Glasgow and Clyde", "Highland", "Lanarkshire", "Lothian", "Orkney", "Shetland", "Tayside", "Western Isles"),
  attendanceper1000pop = c(20.5, 19.2, 21.5, 16.0, 18.7, 14.3, 21.9, 19.9, 21.9, 20.4, 19.8, 20.5, 15.2, 19.0))


Covidscottish_health_boardsattendancerate2020 <- scottish_health_boards %>%
  left_join(Covid_monthlyae_popest2020, by="HBName")

#Choropleth attendancerateper1000pop 2020
CovidScottishdattendancerate2020 <- ggplot(data = Covidscottish_health_boardsattendancerate2020) + 
  geom_sf(aes(fill = attendanceper1000pop)) +  # Plot the health board regions with fill mapped to your data
  geom_sf_text(data = labels_data, aes(label = HBName), size = 3) +  # Add labels
  #scale_fill_viridis_c() +  # Optional: Use a nice color scale
  scale_fill_continuous(labels = label_comma(), type="viridis", 
                        direction = -1, 
                        limits=c(10, 35),
                        guide_legend(title = "Attendance rate \nper 1000 population")) +
  #labs(title = "Attendance rate per 1000 population in 2020 \nby Scottish health board", fill = "attendanceper1000pop") +
  #theme_minimal()
  theme_void()
save_plot("Output/CovidScottishattendancerate2020.svg", fig = CovidScottishdattendancerate2020, width = 12, height = 14)


#Attendance rate 2018
#Only getting data for 2018, HBName and attendanceper1000pop column needed for choropleth
Covid_monthlyae_popest2018 <- Covid_monthlyae_popest %>% 
  filter(Year == "2018") %>% 
  select(HBName, attendanceper1000pop)

Covid_monthlyae_popest2018$attendanceper1000pop<-round(Covid_monthlyae_popest2018$attendanceper1000pop,digits = 1)

#Creating the dataframe with attendanceper1000pop in 2018 by HB because it wouldn't draw the diagram unless I manually created the dataframe from the data obtained
Covid_monthlyae_popest2018 <- data.frame(
  HBName = c("Ayrshire and Arran", "Borders", "Dumfries and Galloway", "Fife","Forth Valley", "Grampian", "Greater Glasgow and Clyde", "Highland", "Lanarkshire", "Lothian", "Orkney", "Shetland", "Tayside", "Western Isles"),
  attendanceper1000pop = c(25.8, 23.5, 26.4, 20.6, 22.8, 19.8, 31.0, 27.6, 26.3, 25.7, 24.6, 27.9, 21.6, 29.0))


Covidscottish_health_boardsattendancerate2018 <- scottish_health_boards %>%
  left_join(Covid_monthlyae_popest2018, by="HBName")

#Choropleth attendancerateper1000pop 2018
CovidScottishdattendancerate2018 <- ggplot(data = Covidscottish_health_boardsattendancerate2018) + 
  geom_sf(aes(fill = attendanceper1000pop)) +  # Plot the health board regions with fill mapped to your data
  geom_sf_text(data = labels_data, aes(label = HBName), size = 3) +  # Add labels
  #scale_fill_viridis_c() +  # Optional: Use a nice color scale
  scale_fill_continuous(labels = label_comma(), type="viridis", 
                        direction = -1, 
                        limits=c(10, 35),
                        guide_legend(title = "Attendance rate \nper 1000 population")) +
  #labs(title = "Attendance rate per 1000 population in 2018 \nby Scottish health board", fill = "attendanceper1000pop") +
  #theme_minimal()
  theme_void()
save_plot("Output/CovidScottishattendancerate2018.svg", fig = CovidScottishdattendancerate2018, width = 12, height = 14)


#Waiting times 2022
#Only getting data for 2022, HBName and PercentageWithin4HoursAll column needed for choropleth
Covid_monthlyae_popest4hour2022 <- Covid_monthlyae_popest %>% 
  filter(Year == "2022") %>% 
  select(HBName, PercentageWithin4HoursAll)

Covid_monthlyae_popest4hour2022$PercentageWithin4HoursAll<-round(Covid_monthlyae_popest4hour2022$PercentageWithin4HoursAll,digits = 1)

#Creating the dataframe with attendanceper1000pop in 2022 by HB because it wouldn't draw the diagram unless I manually created the dataframe from the data obtained
Covid_monthlyae_popest4hour2022 <- data.frame(
  HBName = c("Ayrshire and Arran", "Borders", "Dumfries and Galloway", "Fife","Forth Valley", "Grampian", "Greater Glasgow and Clyde", "Highland", "Lanarkshire", "Lothian", "Orkney", "Shetland", "Tayside", "Western Isles"),
  PercentageWithin4HoursAll = c(86.3, 64.6, 82.3, 91.3, 75.6, 87.3, 79.0, 95.2, 58.2, 67.2, 91.5, 94.2, 95.0, 99.0))


Covidscottish_health_boards4hour2022 <- scottish_health_boards %>%
  left_join(Covid_monthlyae_popest4hour2022, by="HBName")

#Choropleth patients seen within 4 hours 2022 - I increased the width and height compared to attendance rate diagram as the choropleth was too small
CovidScottish4hour2022 <- ggplot(data = Covidscottish_health_boards4hour2022) + 
  geom_sf(aes(fill = PercentageWithin4HoursAll)) +  # Plot the health board regions with fill mapped to your data
  geom_sf_text(data = labels_data, aes(label = HBName), size = 3) +  # Add labels
  #scale_fill_viridis_c() +  # Optional: Use a nice color scale
  scale_fill_continuous(labels = label_comma(), type="viridis", 
                        direction = -1, 
                        limits=c(55, 100),
                        guide_legend(title = "Percentage patients \nseen within 4 hours")) +
  #labs(title = "Percentage patients seen within 4 hours in 2022 \nby Scottish health board", fill = "PercentageWithin4HoursAll") +
  #theme_minimal()
  theme_void()
save_plot("Output/CovidScottish4hour2022.svg", fig = CovidScottish4hour2022, width = 16, height = 18)

##CALCULATED WAITING TIMES
##Instead of using the PercentageWithin4HoursAll data provided, this method is calculating the percentage using the number within 4 hours divided by the number of attendances all
#Loading A&E monthly attendance and waiting times csv file
Covid_monthlyae_activity4hour <- read_csv(here("Rawdata", "monthlyae_activity_202406.csv"))
Covid_monthlyae_activity4hour <- Covid_monthlyae_activity4hour %>% 
  select(Month, HBT, NumberOfAttendancesAll, NumberWithin4HoursAll)

#creating a new column for Year using the first 4 digits of the Month column
Covid_monthlyae_activity4hour$Year <- substr(Covid_monthlyae_activity4hour$Month, 1,4)
Covid_monthlyae_activity4hour$monthnumeric <- substr(Covid_monthlyae_activity4hour$Month, 5,6)

#Calculating waiting time within 4 hours for each month

Covid_monthlyae_activity4hour <- Covid_monthlyae_activity4hour %>% 
  group_by(Year, HBT) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll), 
            NumberWithin4HoursAll=sum(NumberWithin4HoursAll))

Covid_monthlyae_activity4hour$Percentwithin4hr<-(Covid_monthlyae_activity4hour$NumberWithin4HoursAll/Covid_monthlyae_activity4hour$NumberOfAttendancesAll)*100

#Changing the column name from HBT to HB to align the column names in HBnames only dataframe
colnames(Covid_monthlyae_activity4hour)<- c("Year", "HB","NumberOfAttendancesAll","NumberWithin4HoursAll", "Percentwithin4hr")

#Dataframe with the names of the health boards. Note some HB codes have been deprecated and replaced, hence more than 14 entries.
HB_names <- read_csv(here("Rawdata", "HealthBoard_2014_2019_names.csv"))
HB_names_only<-HB_names %>% 
  select(HB, HBName)

#want to merge A&E waiting times with HB names
Covid_monthlyae_activity4hour <- merge(Covid_monthlyae_activity4hour, HB_names_only, by = "HB")

#Only getting data for 2022, HBName and PercentageWithin4HoursAll column needed for choropleth
Covid_monthlyae_activity4hour2022 <- Covid_monthlyae_activity4hour %>% 
  filter(Year == "2022") %>% 
  select(HBName, Percentwithin4hr)


Covid_monthlyae_activity4hour2022$Percentwithin4hr<-round(Covid_monthlyae_activity4hour2022$Percentwithin4hr,digits = 1)

#Creating the dataframe with waiting times in 2022 by HB because it wouldn't draw the diagram unless I manually created the dataframe from the data obtained
Covid_monthlyae_activity4hour2022 <- data.frame(
  HBName = c("Ayrshire and Arran", "Borders", "Dumfries and Galloway", "Fife","Forth Valley", "Grampian", "Greater Glasgow and Clyde", "Highland", "Lanarkshire", "Lothian", "Orkney", "Shetland", "Tayside", "Western Isles"),
  Percentwithin4hr = c(69.5, 64.5, 79.7, 73.0, 64.7, 70.9, 71.1, 85.4, 58.2, 63.4, 91.4, 94.0, 90.4, 97.8))

Covidscottish_health_boards4hour2022calculated <- scottish_health_boards %>%
  left_join(Covid_monthlyae_activity4hour2022, by="HBName")

#Choropleth waiting times calculated 2022 - I increased the width and height compared to attendance rate diagram as the choropleth was too small
CovidScottish4hour2022calculated <- ggplot(data = Covidscottish_health_boards4hour2022calculated) + 
  geom_sf(aes(fill = Percentwithin4hr)) +  # Plot the health board regions with fill mapped to your data
  geom_sf_text(data = labels_data, aes(label = HBName), size = 3) +  # Add labels
  #scale_fill_viridis_c() +  # Optional: Use a nice color scale
  scale_fill_continuous(labels = label_comma(), type="viridis", 
                        direction = -1, 
                        limits=c(55, 100),
                        guide_legend(title = "Percentage patients \nseen within 4 hours")) +
  #labs(title = "Percentage patients seen within 4 hours in 2022 \nby Scottish health board", fill = "PercentageWithin4HoursAll") +
  #theme_minimal()
  theme_void()
save_plot("Output/CovidScottish4hour2022calculated.svg", fig = CovidScottish4hour2022calculated, width = 16, height = 18)



#Only getting data for 2020, HBName and PercentageWithin4HoursAll column needed for choropleth
Covid_monthlyae_popest4hour2020 <- Covid_monthlyae_popest %>% 
  filter(Year == "2020") %>% 
  select(HBName, PercentageWithin4HoursAll)

Covid_monthlyae_popest4hour2020$PercentageWithin4HoursAll<-round(Covid_monthlyae_popest4hour2020$PercentageWithin4HoursAll,digits = 1)

#Creating the dataframe with attendanceper1000pop in 2022 by HB because it wouldn't draw the diagram unless I manually created the dataframe from the data obtained
Covid_monthlyae_popest4hour2020 <- data.frame(
  HBName = c("Ayrshire and Arran", "Borders", "Dumfries and Galloway", "Fife","Forth Valley", "Grampian", "Greater Glasgow and Clyde", "Highland", "Lanarkshire", "Lothian", "Orkney", "Shetland", "Tayside", "Western Isles"),
  PercentageWithin4HoursAll = c(93.9, 91.9, 93.7, 97.7, 95.6, 97.1, 93.6, 98.1, 89.3, 92.5, 96.9, 96.6, 98.5, 99.0))


Covidscottish_health_boards4hour2020 <- scottish_health_boards %>%
  left_join(Covid_monthlyae_popest4hour2020, by="HBName")

#Choropleth waiting times 2020 - I increased the width and height compared to attendance rate diagram as the choropleth was too small
CovidScottish4hour2020 <- ggplot(data = Covidscottish_health_boards4hour2020) + 
  geom_sf(aes(fill = PercentageWithin4HoursAll)) +  # Plot the health board regions with fill mapped to your data
  geom_sf_text(data = labels_data, aes(label = HBName), size = 3) +  # Add labels
  #scale_fill_viridis_c() +  # Optional: Use a nice color scale
  scale_fill_continuous(labels = label_comma(), type="viridis", 
                        direction = -1, 
                        limits=c(55, 100),
                        guide_legend(title = "Percentage patients \nseen within 4 hours")) +
  #labs(title = "Percentage patients seen within 4 hours in 2020 \nby Scottish health board", fill = "PercentageWithin4HoursAll") +
  #theme_minimal()
  theme_void()
save_plot("Output/CovidScottish4hour2020.svg", fig = CovidScottish4hour2020, width = 16, height = 18)

##CALCULATED WAITING TIMES
##Instead of using the PercentageWithin4HoursAll data provided, this method is calculating the percentage using the number within 4 hours divided by the number of attendances all

#Only getting data for 2022, HBName and PercentageWithin4HoursAll column needed for choropleth
Covid_monthlyae_activity4hour2020 <- Covid_monthlyae_activity4hour %>% 
  filter(Year == "2020") %>% 
  select(HBName, Percentwithin4hr)


Covid_monthlyae_activity4hour2020$Percentwithin4hr<-round(Covid_monthlyae_activity4hour2020$Percentwithin4hr,digits = 1)

#Creating the dataframe with waiting times in 2020 by HB because it wouldn't draw the diagram unless I manually created the dataframe from the data obtained
Covid_monthlyae_activity4hour2020 <- data.frame(
  HBName = c("Ayrshire and Arran", "Borders", "Dumfries and Galloway", "Fife","Forth Valley", "Grampian", "Greater Glasgow and Clyde", "Highland", "Lanarkshire", "Lothian", "Orkney", "Shetland", "Tayside", "Western Isles"),
  Percentwithin4hr = c(88.2, 87.5, 92.0, 93.7, 93.7, 92.2, 90.5, 94.9, 87.6, 89.1, 96.7, 96.6, 97.0, 98.1))

Covidscottish_health_boards4hour2020calculated <- scottish_health_boards %>%
  left_join(Covid_monthlyae_activity4hour2020, by="HBName")

#Choropleth waiting times calculated 2020 - I increased the width and height compared to attendance rate diagram as the choropleth was too small
CovidScottish4hour2020calculated <- ggplot(data = Covidscottish_health_boards4hour2020calculated) + 
  geom_sf(aes(fill = Percentwithin4hr)) +  # Plot the health board regions with fill mapped to your data
  geom_sf_text(data = labels_data, aes(label = HBName), size = 3) +  # Add labels
  #scale_fill_viridis_c() +  # Optional: Use a nice color scale
  scale_fill_continuous(labels = label_comma(), type="viridis", 
                        direction = -1, 
                        limits=c(55, 100),
                        guide_legend(title = "Percentage patients \nseen within 4 hours")) +
  #labs(title = "Percentage patients seen within 4 hours in 2020 \nby Scottish health board", fill = "PercentageWithin4HoursAll") +
  #theme_minimal()
  theme_void()
save_plot("Output/CovidScottish4hour2020calculated.svg", fig = CovidScottish4hour2020calculated, width = 16, height = 18)



#Only getting data for 2018, HBName and PercentageWithin4HoursAll column needed for choropleth
Covid_monthlyae_popest4hour2018 <- Covid_monthlyae_popest %>% 
  filter(Year == "2018") %>% 
  select(HBName, PercentageWithin4HoursAll)

Covid_monthlyae_popest4hour2018$PercentageWithin4HoursAll<-round(Covid_monthlyae_popest4hour2018$PercentageWithin4HoursAll,digits = 1)

#Creating the dataframe with waiting times in 2018 by HB because it wouldn't draw the diagram unless I manually created the dataframe from the data obtained
Covid_monthlyae_popest4hour2018 <- data.frame(
  HBName = c("Ayrshire and Arran", "Borders", "Dumfries and Galloway", "Fife","Forth Valley", "Grampian", "Greater Glasgow and Clyde", "Highland", "Lanarkshire", "Lothian", "Orkney", "Shetland", "Tayside", "Western Isles"),
  PercentageWithin4HoursAll = c(96.4, 98.4, 97.1, 98.4, 89.9, 98.9, 93.0, 98.7, 94.5, 89.2, 96.0, 96.4, 99.4, 99.4))


Covidscottish_health_boards4hour2018 <- scottish_health_boards %>%
  left_join(Covid_monthlyae_popest4hour2018, by="HBName")

#Choropleth waiting times 2018 - I increased the width and height compared to attendance rate diagram as the choropleth was too small
CovidScottish4hour2018 <- ggplot(data = Covidscottish_health_boards4hour2018) + 
  geom_sf(aes(fill = PercentageWithin4HoursAll)) +  # Plot the health board regions with fill mapped to your data
  geom_sf_text(data = labels_data, aes(label = HBName), size = 3) +  # Add labels
  #scale_fill_viridis_c() +  # Optional: Use a nice color scale
  scale_fill_continuous(labels = label_comma(), type="viridis", 
                        direction = -1, 
                        limits=c(55, 100),
                        guide_legend(title = "Percentage patients \nseen within 4 hours")) +
  #labs(title = "Percentage patients seen within 4 hours in 2018 \nby Scottish health board", fill = "PercentageWithin4HoursAll") +
  #theme_minimal()
  theme_void()
save_plot("Output/CovidScottish4hour2018.svg", fig = CovidScottish4hour2018, width = 16, height = 18)

##CALCULATED WAITING TIMES
##Instead of using the PercentageWithin4HoursAll data provided, this method is calculating the percentage using the number within 4 hours divided by the number of attendances all

#Only getting data for 2018, HBName and PercentageWithin4HoursAll column needed for choropleth
Covid_monthlyae_activity4hour2018 <- Covid_monthlyae_activity4hour %>% 
  filter(Year == "2018") %>% 
  select(HBName, Percentwithin4hr)


Covid_monthlyae_activity4hour2018$Percentwithin4hr<-round(Covid_monthlyae_activity4hour2018$Percentwithin4hr,digits = 1)

#Creating the dataframe with waiting times in 2018 by HB because it wouldn't draw the diagram unless I manually created the dataframe from the data obtained
Covid_monthlyae_activity4hour2018 <- data.frame(
  HBName = c("Ayrshire and Arran", "Borders", "Dumfries and Galloway", "Fife","Forth Valley", "Grampian", "Greater Glasgow and Clyde", "Highland", "Lanarkshire", "Lothian", "Orkney", "Shetland", "Tayside", "Western Isles"),
  Percentwithin4hr = c(93.2, 92.4, 92.1, 95.7, 84.7, 94.5, 90.1, 96.4, 90.9, 84.2, 96.1, 96.4, 97.8, 98.8))

Covidscottish_health_boards4hour2018calculated <- scottish_health_boards %>%
  left_join(Covid_monthlyae_activity4hour2018, by="HBName")

#Choropleth waiting times calculated 2018 - I increased the width and height compared to attendance rate diagram as the choropleth was too small
CovidScottish4hour2018calculated <- ggplot(data = Covidscottish_health_boards4hour2018calculated) + 
  geom_sf(aes(fill = Percentwithin4hr)) +  # Plot the health board regions with fill mapped to your data
  geom_sf_text(data = labels_data, aes(label = HBName), size = 3) +  # Add labels
  #scale_fill_viridis_c() +  # Optional: Use a nice color scale
  scale_fill_continuous(labels = label_comma(), type="viridis", 
                        direction = -1, 
                        limits=c(55, 100),
                        guide_legend(title = "Percentage patients \nseen within 4 hours")) +
  #labs(title = "Percentage patients seen within 4 hours in 2018 \nby Scottish health board", fill = "PercentageWithin4HoursAll") +
  #theme_minimal()
  theme_void()
save_plot("Output/CovidScottish4hour2018calculated.svg", fig = CovidScottish4hour2018calculated, width = 16, height = 18)

#To compare the attendance rate per 1000 population for the different years 2018, 2020 and 2022
#Creating a data frame containing the attendance rate per 1000 population for 2018, 2020 and 2022
Compareattendanceper1000_2018_2020_2022 <- Covid_monthlyae_popest2018 %>% 
  left_join(Covid_monthlyae_popest2020, by="HBName") %>% 
  left_join(Covid_monthlyae_popest2022, by="HBName")
#changing column names so it is clear which is for which year
colnames(Compareattendanceper1000_2018_2020_2022)<- c("HBName", "Attendanceper1000_2018","Attendanceper1000_2020","Attendanceper1000_2022")

#Calculating the differences between each year
Compareattendanceper1000_2018_2020_2022$diff2020_2018 <- Compareattendanceper1000_2018_2020_2022$Attendanceper1000_2020 - Compareattendanceper1000_2018_2020_2022$Attendanceper1000_2018
Compareattendanceper1000_2018_2020_2022$diff2022_2020 <- Compareattendanceper1000_2018_2020_2022$Attendanceper1000_2022 - Compareattendanceper1000_2018_2020_2022$Attendanceper1000_2020
Compareattendanceper1000_2018_2020_2022$diff2022_2018 <- Compareattendanceper1000_2018_2020_2022$Attendanceper1000_2022 - Compareattendanceper1000_2018_2020_2022$Attendanceper1000_2018


#To compare the percentage patients seen within 4 hours for the different years 2018, 2020 and 2022
#Creating a data frame containing the percentage seen within 4 hours for 2018, 2020 and 2022
Compare4hr_2018_2020_2022 <- Covid_monthlyae_activity4hour2018 %>% 
  left_join(Covid_monthlyae_activity4hour2020, by="HBName") %>% 
  left_join(Covid_monthlyae_activity4hour2022, by="HBName")
#changing column names so it is clear which is for which year
colnames(Compare4hr_2018_2020_2022)<- c("HBName", "Percent4hr2018","Percent4hr2020","Percent4hr2022")

#Calculating the differences between each year
Compare4hr_2018_2020_2022$diff2020_2018 <- Compare4hr_2018_2020_2022$Percent4hr2020 - Compare4hr_2018_2020_2022$Percent4hr2018
Compare4hr_2018_2020_2022$diff2022_2020 <- Compare4hr_2018_2020_2022$Percent4hr2022 - Compare4hr_2018_2020_2022$Percent4hr2020
Compare4hr_2018_2020_2022$diff2022_2018 <- Compare4hr_2018_2020_2022$Percent4hr2022 - Compare4hr_2018_2020_2022$Percent4hr2018



