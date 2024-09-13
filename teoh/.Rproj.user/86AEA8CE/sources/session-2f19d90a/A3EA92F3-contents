#Scottish A&E data - population estimate and attendance rates

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

#POPULATION ESTIMATE
#Loading population estimate (only till 2021) csv file
HBpopulation_estimate <- read_csv(here("Rawdata", "HealthBoard_2019_Populationestimates.csv"))

HBpopulation_estimate_all <- HBpopulation_estimate %>% 
  filter(Sex=="All") %>%
  group_by(HB,Year) %>%
  select(Year, HB, AllAges)


#Creating a dataframe assuming that 2022 population is the same as 2021

HBpopulation22 <-HBpopulation_estimate_all %>% 
  filter(Year=="2021") 

HBpopulation22$Year[HBpopulation22$Year=="2021"] <-"2022"
HBpopulation22$Year<-as.numeric(HBpopulation22$Year)
str(HBpopulation22)

#Creating a dataframe assuming that 2023 population is the same as 2021
HBpopulation23 <-HBpopulation_estimate_all %>% 
  filter(Year=="2021") 

HBpopulation23$Year[HBpopulation23$Year=="2021"] <-"2023"
HBpopulation23$Year<-as.numeric(HBpopulation23$Year)
str(HBpopulation23)

#Combining population estimates till 2021 with made up for 2022 and 2023
HBpopulation_estimate_2023<- rbind(HBpopulation_estimate_all,HBpopulation22,HBpopulation23)

#Dataframe with the names of the health boards
HB_names <- read_csv(here("Rawdata", "HealthBoard_2014_2019_names.csv"))
HB_names_only<-HB_names %>% 
  select(HB, HBName)

HBpopulation_estimate_2023_HBnames<-merge(HBpopulation_estimate_2023, HB_names_only, by="HB")

#Changing the column name from HBName to NHSBoardName
colnames(HBpopulation_estimate_2023_HBnames)<- c("HB","Year","PopulationAllAges","NHSBoardName")


#Creating a graph for Grampian, Lothian, Western Isles attendance per 1000 population

#Creating a new column of Year
Grampian_Lothian_WesternIsle$Year<- as.numeric(format(Grampian_Lothian_WesternIsle$MonthEndingDate, "%Y"))
Grampian_Lothian_WesternIsle

#Combining Grampian,Lothian and WesternIsle with population estimates
Grampian_Lothian_WesternIsle_population<-merge(Grampian_Lothian_WesternIsle, HBpopulation_estimate_2023_HBnames, by=c("NHSBoardName", "Year"))

#Calculating attendance per population
Grampian_Lothian_WesternIsle_population$attendanceperpop<-Grampian_Lothian_WesternIsle_population$NumberOfAttendancesAll/Grampian_Lothian_WesternIsle_population$PopulationAllAges
#Calculating attendance per 1,000 population
Grampian_Lothian_WesternIsle_population$attendanceper1000pop<-Grampian_Lothian_WesternIsle_population$attendanceperpop*1000

#Line graph of Grampian, Lothian and WesternIsles attendances per 1000 population
png(file = "Output/Grampian_Lothian_WesternIsles_per1000pop.png")
ggplot(data=Grampian_Lothian_WesternIsle_population, aes(x=MonthEndingDate, y=attendanceper1000pop, group=NHSBoardName, color=NHSBoardName)) +
  geom_line() +
  #scale_x_continuous(breaks=seq(2007,2024,2))+
  #scale_y_continuous(labels=scales::comma)+
  labs(title = "Scottish accident and emergency (A&E) attendance rate per 1,000 population at NHS Grampian, NHS Lothian and NHS Western Isles",
       x = "Year",
       y = "Attendance rate per 1,000 population", 
       col="NHS Board Name")
dev.off() 

#attendance rate by all HB using estimated (old, not Chris's) population data 
#need to rename NHSBoardName to be the same as in the population dataset column HBName and merge the two datasets (HB attendances with population estimates)
ae_byboard_population<-ae_byboard
ae_byboard_population<- ae_byboard_population %>% 
  mutate(NHSBoardName=recode(NHSBoardName, 
                             "NHS Ayrshire & Arran" = "NHS Ayrshire and Arran",
                             "NHS Borders" = "NHS Borders",
                             "NHS Dumfries & Galloway" = "NHS Dumfries and Galloway",
                             "NHS Fife" = "NHS Fife",
                             "NHS Forth Valley" = "NHS Forth Valley",
                             "NHS Grampian" = "NHS Grampian",
                             "NHS Greater Glasgow & Clyde" = "NHS Greater Glasgow and Clyde",
                             "NHS Highland" = "NHS Highland",
                             "NHS Lanarkshire" = "NHS Lanarkshire",
                             "NHS Lothian" = "NHS Lothian",
                             "NHS Orkney" = "NHS Orkney",
                             "NHS Shetland" = "NHS Shetland",
                             "NHS Tayside" = "NHS Tayside",
                             "NHS Western Isles" = "NHS Western Isles"))


#Combining ae_byboard with population estimates
ae_byboard_population<-merge(ae_byboard_population, HBpopulation_estimate_2023_HBnames, by=c("NHSBoardName", "Year"))

#Calculating attendance per population
ae_byboard_population$attendanceperpop<-ae_byboard_population$NumberOfAttendancesAll/ae_byboard_population$PopulationAllAges
#Calculating attendance per 1,000 population
ae_byboard_population$attendanceper1000pop<-ae_byboard_population$attendanceperpop*1000

png(file = "Output/AttendancebyHBper1000pop.png")
ggplot(data=ae_byboard_population, aes(x=MonthEndingDate, y=attendanceper1000pop, group=NHSBoardName, color=NHSBoardName)) +
  geom_line() +
  labs(title = "Accident and emergency attendance rate per 1000 population by Scottish Health Boards",
       x = "Date",
       y = "Attendance rate per 1000 population")
dev.off()  


#Using Chris's Updated population estimate
HBUpdatespopulation_estimate<- read_csv(here("Rawdata", "UpdatesPopulationEstimates.csv"))


#Creating a dataframe for HB population estimates
HBUpdatespopulation_estimate_HBname <- HBUpdatespopulation_estimate %>%
  select(Year, HB, Sex, AllAges) %>% 
  filter(Sex == 'All') %>% 
  left_join(HB_names_only, HBUpdatespopulation_estimate, by="HB")


#Creating a graph of Grampian, Lothian and Western Isles attendance rate using Chris Updated population estimate

#need to rename NHSBoardName to be the same as in the population dataset column HBName and merge the two datasets (Grampian, Lothian, Western Isles attendance with population estimates)
Grampian_Lothian_WesternIsle_Updatedpopulation<-Grampian_Lothian_WesternIsle
Grampian_Lothian_WesternIsle_Updatedpopulation$Year<- as.numeric(format(Grampian_Lothian_WesternIsle_Updatedpopulation$MonthEndingDate, "%Y"))
colnames(Grampian_Lothian_WesternIsle_Updatedpopulation)<- c("HBName","MonthEndingDate","NumberOfAttendancesAll","Year")
Grampian_Lothian_WesternIsle_Updatedpopulation<-merge(Grampian_Lothian_WesternIsle_Updatedpopulation, HBUpdatespopulation_estimate_HBname, by=c("HBName", "Year"))

#Calculating attendance per population using Chris's updated population estimate
Grampian_Lothian_WesternIsle_Updatedpopulation$attendanceperpop<-Grampian_Lothian_WesternIsle_Updatedpopulation$NumberOfAttendancesAll/Grampian_Lothian_WesternIsle_Updatedpopulation$AllAges
#Calculating attendance per 1,000 populationusing Chris's updated population estimate
Grampian_Lothian_WesternIsle_Updatedpopulation$attendanceper1000pop<-Grampian_Lothian_WesternIsle_Updatedpopulation$attendanceperpop*1000

#Line graph of Grampian, Lothian and WesternIsles attendances per 1000 population using Chris's updated population estimate
png(file = "Output/Grampian_Lothian_WesternIsles_per1000Updatedpop.png")
ggplot(data=Grampian_Lothian_WesternIsle_Updatedpopulation, aes(x=MonthEndingDate, y=attendanceper1000pop, group=HBName, color=HBName)) +
  geom_line() +
  #scale_x_continuous(breaks=seq(2007,2024,2))+
  #scale_y_continuous(labels=scales::comma)+
  labs(title = "Scottish accident and emergency (A&E) attendance rate per 1,000 population at NHS Grampian, NHS Lothian and NHS Western Isles",
       x = "Year",
       y = "Attendance rate per 1,000 population", 
       col="NHS Board Name")
dev.off() 

#Saving as svg line graph of Grampian, Lothian and WesternIsles attendances per 1000 population using Chris's updated population estimate

Grampian_Lothian_WesternIsles_per1000Updatedpop <- ggplot(data=Grampian_Lothian_WesternIsle_Updatedpopulation, aes(x=MonthEndingDate, y=attendanceper1000pop, group=HBName, color=HBName)) +
  geom_line() +
  #scale_x_continuous(breaks=seq(2007,2024,2))+
  #scale_y_continuous(labels=scales::comma)+
  labs(
    #title = "Scottish accident and emergency (A&E) attendance rate per 1,000 population \nat NHS Grampian, NHS Lothian and NHS Western Isles",
    x = "Year",
    y = "Attendance rate per 1,000 population", 
    col="NHS Board Name")
save_plot("Output/Grampian_Lothian_WesternIsles_per1000Updatedpop.svg", fig = Grampian_Lothian_WesternIsles_per1000Updatedpop, width = 14, height = 12)

##Creating a graph of Scottish HB attendance rate using Chris Updated population estimate

#need to rename NHSBoardName to be the same as in the population dataset column HBName and merge the two datasets (HB attendances with population estimates)
ae_byboard_Updatedpopulation<-ae_byboard
colnames(ae_byboard_Updatedpopulation)<- c("HBName","MonthEndingDate","NumberOfAttendancesAll","Year")
ae_byboard_Updatedpopulation<- ae_byboard_Updatedpopulation %>% 
  mutate(HBName=recode(HBName, 
                       "NHS Ayrshire & Arran" = "NHS Ayrshire and Arran",
                       "NHS Borders" = "NHS Borders",
                       "NHS Dumfries & Galloway" = "NHS Dumfries and Galloway",
                       "NHS Fife" = "NHS Fife",
                       "NHS Forth Valley" = "NHS Forth Valley",
                       "NHS Grampian" = "NHS Grampian",
                       "NHS Greater Glasgow & Clyde" = "NHS Greater Glasgow and Clyde",
                       "NHS Highland" = "NHS Highland",
                       "NHS Lanarkshire" = "NHS Lanarkshire",
                       "NHS Lothian" = "NHS Lothian",
                       "NHS Orkney" = "NHS Orkney",
                       "NHS Shetland" = "NHS Shetland",
                       "NHS Tayside" = "NHS Tayside",
                       "NHS Western Isles" = "NHS Western Isles"))


ae_byboard_Updatedpopulation<-merge(ae_byboard_Updatedpopulation, HBUpdatespopulation_estimate_HBname, by=c("HBName", "Year"))

#Calculating attendance per population using Chris's updated population estimate
ae_byboard_Updatedpopulation$attendanceperpop<-ae_byboard_Updatedpopulation$NumberOfAttendancesAll/ae_byboard_Updatedpopulation$AllAges
#Calculating attendance per 1,000 populationusing Chris's updated population estimate
ae_byboard_Updatedpopulation$attendanceper1000pop<-ae_byboard_Updatedpopulation$attendanceperpop*1000

#Line graph of attendances per 1000 population using Chris's updated population estimate
png(file = "Output/AttendancesbyHBper1000Updatedpop.png")
ggplot(data=ae_byboard_Updatedpopulation, aes(x=MonthEndingDate, y=attendanceper1000pop, group=HBName, color=HBName)) +
  geom_line() +
  #scale_x_continuous(breaks=seq(2007,2024,2))+
  #scale_y_continuous(labels=scales::comma)+
  labs(title = "Accident and emergency (A&E) attendance rate per 1,000 population by \nScottish health boards",
       x = "Year",
       y = "Attendance rate per 1,000 population", 
       col="NHS Board Name")
dev.off() 

#Line graph of attendances per 1000 population using Chris's updated population estimate
png(file = "Output/AttendancesbyHBper1000Updatedpop_nocolour.png")
ggplot(data=ae_byboard_Updatedpopulation, aes(x=MonthEndingDate, y=attendanceper1000pop, group=HBName, 
                                              #color=HBName
)) +
  geom_line() +
  #scale_x_continuous(breaks=seq(2007,2024,2))+
  #scale_y_continuous(labels=scales::comma)+
  labs(title = "Accident and emergency (A&E) attendance rate per 1,000 population by \nScottish health boards",
       x = "Year",
       y = "Attendance rate per 1,000 population", 
       col="NHS Board Name")
dev.off()

#saving AttendancesbyHBper1000Updatedpop_nocolour as svg

AttendancesbyHBper1000Updatedpop_nocolour<- ggplot(data=ae_byboard_Updatedpopulation, aes(x=MonthEndingDate, y=attendanceper1000pop, group=HBName, 
                                                                                          #color=HBName
)) +
  geom_line() +
  #scale_x_continuous(breaks=seq(2007,2024,2))+
  #scale_y_continuous(labels=scales::comma)+
  labs(
    #title = "Accident and emergency (A&E) attendance rate per 1,000 population by \nScottish health boards",
    x = "Year",
    y = "Attendance rate per 1,000 population", 
    col="NHS Board Name")
save_plot("Output/AttendancesbyHBper1000Updatedpop_nocolour.svg", fig = AttendancesbyHBper1000Updatedpop_nocolour, width = 14, height = 12)

