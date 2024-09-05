#ScottishCovidGLM

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

#A&E attendances
#Loading A&E monthly attendance and waiting times csv file
Covid_monthlyae_glm <- read_csv(here("Rawdata", "monthlyae_activity_202406.csv"))

#Attendance and percentage within 4 hours by HB each month
Covid_monthlyae_glm <- Covid_monthlyae_glm %>% 
  select(Month, 
         #HBT, 
         #DepartmentType, 
         NumberOfAttendancesAll) %>% 
  group_by(Month 
          #,HBT
          #,DepartmentType
          ) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll))


#Demographics
#Loading A&E demographic csv file
Covid_monthlyae_glmdemographics <- read_csv(here("Rawdata", "monthlyae_demographics_202406.csv"))

#To create a date column
#creating a new column for Year, monthnumeric and day using data from the Month column
Covid_monthlyae_glmdemographics$Year <- substr(Covid_monthlyae_glmdemographics$Month, 1,4)
Covid_monthlyae_glmdemographics$monthnumeric <- substr(Covid_monthlyae_glmdemographics$Month, 5,6)
Covid_monthlyae_glmdemographics$day <- "01"

#converting from character to numeric variable
Covid_monthlyae_glmdemographics$Year <- as.numeric(Covid_monthlyae_glmdemographics$Year)
Covid_monthlyae_glmdemographics$monthnumeric <- as.numeric(Covid_monthlyae_glmdemographics$monthnumeric)
Covid_monthlyae_glmdemographics$day <- as.numeric(Covid_monthlyae_glmdemographics$day)

#making a date column using the Year, monthnumeric and day columns
Covid_monthlyae_glmdemographics<- Covid_monthlyae_glmdemographics %>% 
  mutate(date=make_date(Year, monthnumeric, day))

str(Covid_monthlyae_glmdemographics)

#AGE
#calculating total attendances for all age groups
Covid_monthlyae_glmdemographicstotal <- Covid_monthlyae_glmdemographics %>% 
  select(date, NumberOfAttendances) %>% 
  group_by(date) %>% 
  summarise(NumberOfAttendances=sum(NumberOfAttendances))

#calculating total attendances by age groups
Covid_monthlyae_glmdemographicsage <- Covid_monthlyae_glmdemographics %>% 
  select(date, Age, NumberOfAttendances) %>% 
  group_by(date, Age) %>% 
  summarise(NumberOfAttendances=sum(NumberOfAttendances))

#renaming all na age to unknown
Covid_monthlyae_glmdemographicsage <- Covid_monthlyae_glmdemographicsage %>% replace_na(list(Age="UnknownAge"))

#merge age group attendances with total attendances to calculate proportion
Covid_monthlyae_glmdemographicsage <- merge(Covid_monthlyae_glmdemographicsage, Covid_monthlyae_glmdemographicstotal, by=c("date"))

#changing column names so it is clear which is total and which is for age
colnames(Covid_monthlyae_glmdemographicsage)<- c("date", "Age","AgeAttendances","TotalAttendances")

#to calculate the proportion for each age
Covid_monthlyae_glmdemographicsage$Proportions <-Covid_monthlyae_glmdemographicsage$AgeAttendances/Covid_monthlyae_glmdemographicsage$TotalAttendances

Covid_monthlyae_glmdemographicsage <- Covid_monthlyae_glmdemographicsage %>% select(date,Age,Proportions)

#to change the data from a long to a wide format
Covid_monthlyae_glmdemographicsage <- pivot_wider(Covid_monthlyae_glmdemographicsage, names_from = Age, values_from = Proportions)



str(Covid_monthlyae_glmdemographicsage)


#When
Covid_monthlyae_glmwhen <- read_csv(here("Rawdata", "monthlyae_when_202406.csv"))

Covid_monthlyae_glmwhen <- Covid_monthlyae_glmwhen %>% 
  select(Month, NumberOfAttendances) %>% 
  group_by(Month) %>% 
  summarise(NumberOfAttendances=sum(NumberOfAttendances))

