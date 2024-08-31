#Scottish A&E data - COVID-19 attendance rate

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
#Loading population estimate (only till 2022) csv file
Covid_HBpopulation_estimate <- read_csv(here("Rawdata", "20240809Popestimate_HB2019_1981to2022.csv"))

#Filtering out total Scottish estimates (HB S92000003) for each year, leaving just individual HBs in the list
Covid_HBpopulation_estimate <- Covid_HBpopulation_estimate %>%
  select(Year, HB, Sex, AllAges) %>% 
  filter(!HB=="S92000003", Sex=="All")

#Dataframe with the names of the health boards. Note some HB codes have been deprecated and replaced, hence more than 14 entries.
HB_names <- read_csv(here("Rawdata", "HealthBoard_2014_2019_names.csv"))
HB_names_only<-HB_names %>% 
  select(HB, HBName)

#Loading A&E monthly attendance and waiting times csv file
Covid_monthlyae_activity <- read_csv(here("Rawdata", "monthlyae_activity_202406.csv"))

#creating a new column for Year using the first 4 digits of the Month column
Covid_monthlyae_activity$Year <- substr(Covid_monthlyae_activity$Month, 1,4)

#Attendance and percentage within 4 hours by HB each month
Covid_monthlyae_activity <- Covid_monthlyae_activity %>% 
  select(HBT, NumberOfAttendancesAll, PercentageWithin4HoursAll, Year) %>% 
  group_by(Year, HBT) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll), 
            PercentageWithin4HoursAll=mean(PercentageWithin4HoursAll))

#want to merge population estimates with HB names
Covid_HBpopulation_estimate <- merge(Covid_HBpopulation_estimate, HB_names_only, by = "HB")

#Changing the column name from HB to HBT to align the column names in the activity and population estimate dataframes
colnames(Covid_monthlyae_activity)<- c("Year","HB","NumberOfAttendancesAll","PercentageWithin4HoursAll")

#to merge A&E activity with population estimate
Covid_monthlyae_popest <- merge(Covid_monthlyae_activity, Covid_HBpopulation_estimate, by=c("HB","Year"))

str(Covid_monthlyae_activity)

#Calculating attendance per population
Covid_monthlyae_popest$attendanceperpop<-Covid_monthlyae_popest$NumberOfAttendancesAll/Covid_monthlyae_popest$AllAges
#Calculating attendance per 1,000 population
Covid_monthlyae_popest$attendanceper1000pop<-Covid_monthlyae_popest$attendanceperpop*1000


#Month returns as numeric. Lubridate cannot handle just year and month, need to introduce and artificial day of the month to the string
#Covid_monthlyae_activity$Month <- format(as.Date(Covid_monthlyae_activity$Month, format="%Y/%m/%d"))
#concatenating Month with an artificial day of the month 01
#Covid_monthlyae_activity$Date <- paste(Covid_monthlyae_activity$Month,"01")
#Covid_monthlyae_activity$Date <- format(as.Date(Covid_monthlyae_activity$Date, format="%Y/%m/%d")) 

#merge(df1, df2, by.x = "df1ColName", by.y = "df2ColName")


