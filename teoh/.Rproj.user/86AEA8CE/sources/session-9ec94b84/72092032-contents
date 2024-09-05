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

#Checking if there are null values, result returned zero
sum(is.na(Covid_HBpopulation_estimate))

#Filtering out total Scottish estimates (HB S92000003) for each year, leaving just individual HBs in the list
Covid_HBpopulation_estimate <- Covid_HBpopulation_estimate %>%
  select(Year, HB, Sex, AllAges) %>% 
  filter(!HB=="S92000003", Sex=="All")

#Dataframe with the names of the health boards. Note some HB codes have been deprecated and replaced, hence more than 14 entries.
HB_names <- read_csv(here("Rawdata", "HealthBoard_2014_2019_names.csv"))
HB_names_only<-HB_names %>% 
  select(HB, HBName)

#want to merge population estimates with HB names
Covid_HBpopulation_estimate <- merge(Covid_HBpopulation_estimate, HB_names_only, by = "HB")

#Loading A&E monthly attendance and waiting times csv file
Covid_monthlyae_activity <- read_csv(here("Rawdata", "monthlyae_activity_202406.csv"))

#Checking if there are null values, result returned 138776
sum(is.na(Covid_monthlyae_activity))
#Checking if there are null values for NumberOfAttendancesAll, result returned zero
sum(is.na(Covid_monthlyae_activity$NumberOfAttendancesAll))
#Checking if there are null values for NumberWithin4HoursAll, result returned zero
sum(is.na(Covid_monthlyae_activity$NumberWithin4HoursAll))
#Checking if there are null values for PercentageWithin4HoursAll, result returned zero
sum(is.na(Covid_monthlyae_activity$PercentageWithin4HoursAll))

#creating a new column for Year using the first 4 digits of the Month column
Covid_monthlyae_activity$Year <- substr(Covid_monthlyae_activity$Month, 1,4)
Covid_monthlyae_activity$monthnumeric <- substr(Covid_monthlyae_activity$Month, 5,6)

#Attendance and percentage within 4 hours by HB each month
Covid_monthlyae_activity <- Covid_monthlyae_activity %>% 
  select(HBT, NumberOfAttendancesAll, PercentageWithin4HoursAll, Year, monthnumeric) %>% 
  group_by(Year, monthnumeric, HBT) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll), 
            PercentageWithin4HoursAll=mean(PercentageWithin4HoursAll))

#Changing the column name from HBT to HB to align the column names in the activity and population estimate dataframes
colnames(Covid_monthlyae_activity)<- c("Year","monthnumeric", "HB","NumberOfAttendancesAll","PercentageWithin4HoursAll")

#to merge A&E activity with population estimate
Covid_monthlyae_popest <- merge(Covid_monthlyae_activity, Covid_HBpopulation_estimate, by=c("HB","Year"))

#Calculating attendance per population for each month
Covid_monthlyae_popest$attendanceperpop<-Covid_monthlyae_popest$NumberOfAttendancesAll/Covid_monthlyae_popest$AllAges
#Calculating attendance per 1,000 population for each month
Covid_monthlyae_popest$attendanceper1000pop<-Covid_monthlyae_popest$attendanceperpop*1000

#mean attendance per population and attendance per 1,000 population for each year
Covid_monthlyae_popest <- Covid_monthlyae_popest %>% 
  select(HB, Year, NumberOfAttendancesAll, PercentageWithin4HoursAll, AllAges, attendanceperpop, attendanceper1000pop) %>% 
  group_by(Year, HB) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll), 
            PercentageWithin4HoursAll=mean(PercentageWithin4HoursAll),
            AllAges=mean(AllAges),
            attendanceperpop=mean(attendanceperpop),
            attendanceper1000pop=mean(attendanceper1000pop))

#adding in the HB names
Covid_monthlyae_popest  <- merge(Covid_monthlyae_popest , HB_names_only, by = "HB")


str(Covid_monthlyae_activity)




#Month returns as numeric. Lubridate cannot handle just year and month, need to introduce and artificial day of the month to the string
#Covid_monthlyae_activity$Month <- format(as.Date(Covid_monthlyae_activity$Month, format="%Y/%m/%d"))
#concatenating Month with an artificial day of the month 01
#Covid_monthlyae_activity$Date <- paste(Covid_monthlyae_activity$Month,"01")
#Covid_monthlyae_activity$Date <- format(as.Date(Covid_monthlyae_activity$Date, format="%Y/%m/%d")) 

#merge(df1, df2, by.x = "df1ColName", by.y = "df2ColName")


