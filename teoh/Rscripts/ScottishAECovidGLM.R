#ScottishCovidGLM

##BEFORE DOING GLM REMEMBER TO LIMIT TO REQUIRED DATE RANGE using filter##
##To combine all the individual variable dataframes into one to perform the GLM##
#Covid_monthlyae_glmdemographicsage
#Covid_monthlyae_glmdemographicssex
#Covid_monthlyae_glmdemographicsdeprivation
#Covid_monthlyae_glmdemographicsdepttype
#Covid_monthlyae_glmdemographicsHB
#Covid_monthlyae_glmdemographicsCoviddate_all
#Covid_monthlyae_glmwhenday
#Covid_monthlyae_glmwhenhour
#Covid_monthlyae_glmwhenmonth


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

#calculating total attendances
Covid_monthlyae_glmdemographicstotal <- Covid_monthlyae_glmdemographics %>% 
  select(date, NumberOfAttendances) %>% 
  group_by(date) %>% 
  summarise(NumberOfAttendances=sum(NumberOfAttendances))

#AGE

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

#renaming the column names so that they can be added to the linear model
Covid_monthlyae_glmdemographicsage <- Covid_monthlyae_glmdemographicsage %>% 
  rename(c("Under18" = "Under 18",
           "EighteentoTwentyfour" = "18-24",
           "TwentyfivetoThirtynine" = "25-39",
           "FortytoSixtyfour" = "40-64",
           "SixtyfivetoSeventyfour" = "65-74",
           "Seventyfiveplus" = "75 plus"))

str(Covid_monthlyae_glmdemographicsage)

#SEX

#calculating total attendances by sex
Covid_monthlyae_glmdemographicssex <- Covid_monthlyae_glmdemographics %>% 
  select(date, Sex, NumberOfAttendances) %>% 
  group_by(date, Sex) %>% 
  summarise(NumberOfAttendances=sum(NumberOfAttendances))

#renaming all na age to unknown
Covid_monthlyae_glmdemographicssex <- Covid_monthlyae_glmdemographicssex %>% replace_na(list(Sex="UnknownSex"))

#merge sex attendances with total attendances to calculate proportion
Covid_monthlyae_glmdemographicssex <- merge(Covid_monthlyae_glmdemographicssex, Covid_monthlyae_glmdemographicstotal, by=c("date"))

#changing column names so it is clear which is total and which is for age
colnames(Covid_monthlyae_glmdemographicssex)<- c("date", "Sex","SexAttendances","TotalAttendances")

#to calculate the proportion for each age
Covid_monthlyae_glmdemographicssex$Proportions <-Covid_monthlyae_glmdemographicssex$SexAttendances/Covid_monthlyae_glmdemographicssex$TotalAttendances

Covid_monthlyae_glmdemographicssex <- Covid_monthlyae_glmdemographicssex %>% select(date,Sex,Proportions)

#to change the data from a long to a wide format
Covid_monthlyae_glmdemographicssex <- pivot_wider(Covid_monthlyae_glmdemographicssex, names_from = Sex, values_from = Proportions)

str(Covid_monthlyae_glmdemographicssex)

#DEPRIVATION

#calculating total attendances by deprivation
Covid_monthlyae_glmdemographicsdeprivation <- Covid_monthlyae_glmdemographics %>% 
  select(date, Deprivation, NumberOfAttendances) %>% 
  group_by(date, Deprivation) %>% 
  summarise(NumberOfAttendances=sum(NumberOfAttendances))

#renaming all na deprivation to unknown
str(Covid_monthlyae_glmdemographicsdeprivation)
#have to change Deprivation from double to character before changing na to unknown
Covid_monthlyae_glmdemographicsdeprivation$Deprivation <- as.character(Covid_monthlyae_glmdemographicsdeprivation$Deprivation)
Covid_monthlyae_glmdemographicsdeprivation <- Covid_monthlyae_glmdemographicsdeprivation %>% replace_na(list(Deprivation="UnknownSIMD"))

#merge deprivation attendances with total attendances to calculate proportion
Covid_monthlyae_glmdemographicsdeprivation <- merge(Covid_monthlyae_glmdemographicsdeprivation, Covid_monthlyae_glmdemographicstotal, by=c("date"))

#changing column names so it is clear which is total and which is for deprivation
colnames(Covid_monthlyae_glmdemographicsdeprivation)<- c("date", "SIMD","SIMDAttendances","TotalAttendances")

#to calculate the proportion for each deprivation
Covid_monthlyae_glmdemographicsdeprivation$Proportions <-Covid_monthlyae_glmdemographicsdeprivation$SIMDAttendances/Covid_monthlyae_glmdemographicsdeprivation$TotalAttendances

Covid_monthlyae_glmdemographicsdeprivation <- Covid_monthlyae_glmdemographicsdeprivation %>% select(date,SIMD,Proportions)

#to change the data from a long to a wide format
Covid_monthlyae_glmdemographicsdeprivation <- pivot_wider(Covid_monthlyae_glmdemographicsdeprivation, names_from = SIMD, values_from = Proportions)

#changing SIMD column names to be clearer
colnames(Covid_monthlyae_glmdemographicsdeprivation)<- c("date", "SIMD1","SIMD2","SIMD3", "SIMD4", "SIMD5", "UnknownSIMD")

str(Covid_monthlyae_glmdemographicsdeprivation)

#DEPARTMENT TYPE

#calculating total attendances by department type
Covid_monthlyae_glmdemographicsdepttype <- Covid_monthlyae_glmdemographics %>% 
  select(date, DepartmentType, NumberOfAttendances) %>% 
  group_by(date, DepartmentType) %>% 
  summarise(NumberOfAttendances=sum(NumberOfAttendances))

#merge dept type attendances with total attendances to calculate proportion
Covid_monthlyae_glmdemographicsdepttype <- merge(Covid_monthlyae_glmdemographicsdepttype, Covid_monthlyae_glmdemographicstotal, by=c("date"))

#changing column names so it is clear which is total and which is for depttype
colnames(Covid_monthlyae_glmdemographicsdepttype)<- c("date", "DepartmentType","DepttypeAttendances","TotalAttendances")

#to calculate the proportion for each depttype
Covid_monthlyae_glmdemographicsdepttype$Proportions <-Covid_monthlyae_glmdemographicsdepttype$DepttypeAttendances/Covid_monthlyae_glmdemographicsdepttype$TotalAttendances

Covid_monthlyae_glmdemographicsdepttype <- Covid_monthlyae_glmdemographicsdepttype %>% select(date,DepartmentType,Proportions)

#to change the data from a long to a wide format
Covid_monthlyae_glmdemographicsdepttype <- pivot_wider(Covid_monthlyae_glmdemographicsdepttype, names_from = DepartmentType, values_from = Proportions)

#changing column names to ED and MIU
colnames(Covid_monthlyae_glmdemographicsdepttype)<- c("date", "ED","MIU/Other")

str(Covid_monthlyae_glmdemographicsdepttype)

#HEALTH BOARD

#calculating total attendances by health board
Covid_monthlyae_glmdemographicsHB <- Covid_monthlyae_glmdemographics %>% 
  select(date, HBT, NumberOfAttendances) %>% 
  group_by(date, HBT) %>% 
  summarise(NumberOfAttendances=sum(NumberOfAttendances))

#merge HB attendances with total attendances to calculate proportion
Covid_monthlyae_glmdemographicsHB <- merge(Covid_monthlyae_glmdemographicsHB, Covid_monthlyae_glmdemographicstotal, by=c("date"))

#changing column names so it is clear which is total and which is for HB
colnames(Covid_monthlyae_glmdemographicsHB)<- c("date", "HBT","HBAttendances","TotalAttendances")

#to calculate the proportion for each depttype
Covid_monthlyae_glmdemographicsHB$Proportions <-Covid_monthlyae_glmdemographicsHB$HBAttendances/Covid_monthlyae_glmdemographicsHB$TotalAttendances

Covid_monthlyae_glmdemographicsHB <- Covid_monthlyae_glmdemographicsHB %>% select(date,HBT,Proportions)

#to change the data from a long to a wide format
Covid_monthlyae_glmdemographicsHB <- pivot_wider(Covid_monthlyae_glmdemographicsHB, names_from = HBT, values_from = Proportions)

Covid_monthlyae_glmdemographicsHB <- Covid_monthlyae_glmdemographicsHB %>% 
  rename(c("NHSAyrshireandArran" = "S08000015",
           "NHSBorders" = "S08000016",
           "NHSDumfriesandGalloway" = "S08000017",
           "NHSForthValley" = "S08000019",
           "NHSGrampian" = "S08000020",
           "NHSHighland" = "S08000022",
           "NHSLothian" = "S08000024",
           "NHSOrkney" = "S08000025",
           "NHSShetland" = "S08000026",
           "NHSWesternIsles" = "S08000028",
           "NHSFife" = "S08000029",
           "NHSTayside" = "S08000030",
           "NHSGreaterGlasgowandClyde" = "S08000031",
           "NHSLanarkshire" = "S08000032"))


#COVIDdates

#Attribute
#1 Lockdown
#2 Tightening of restrictions
#3 Easing of restrictions
#4 No or most legal restrictions removed

Covid_monthlyae_glmdemographicsCoviddate <- Covid_monthlyae_glmdemographics %>% 
  select(date, NumberOfAttendances) %>% 
  group_by(date) %>% 
  summarise(NumberOfAttendances=sum(NumberOfAttendances))


#To add a columns for Coviddate and populate

#Tried using the date range to populate but it didn't work, so did it in batches below and combined them together
#Covid_monthlyae_glmdemographicsCoviddate <- Covid_monthlyae_glmdemographicsCoviddate %>% 
#  mutate(Coviddate = ifelse(.$date==between(date, as.Date('2018-01-01'), as.Date('2020-03-31'), '4', '0')),
#         Coviddate = ifelse(.$date==between(date, as.Date('2020-04-01'), as.Date('2020-05-31'), '1', '0')),
#         Coviddate = ifelse(.$date==between(date, as.Date('2020-06-01'), as.Date('2020-09-30'), '3', '0')),
#         Coviddate = ifelse(.$date==between(date, as.Date('2020-10-01'), as.Date('2020-12-31'), '2', '0')),
#         Coviddate = ifelse(.$date==between(date, as.Date('2021-01-01'), as.Date('2021-04-30'), '1', '0')),
#         Coviddate = ifelse(.$date==between(date, as.Date('2021-05-01'), as.Date('2021-07-31'), '3', '0')),
#         Coviddate = ifelse(.$date==between(date, as.Date('2021-08-01'), as.Date('2024-06-31'), '4', '0')))

#Covid_monthlyae_glmdemographicsCoviddate <- Covid_monthlyae_glmdemographicsCoviddate %>% 
#  mutate(Coviddate4 = ifelse((.$date==between(date, as.Date('2018-01-01'), as.Date('2020-03-31')), '1', '0')),
#         Coviddate1 = ifelse((.$date==between(date, as.Date('2020-04-01'), as.Date('2020-05-31')), '1', '0')),
#         Coviddate3 = ifelse((.$date==between(date, as.Date('2020-06-01'), as.Date('2020-09-30')), '1', '0')),
#         Coviddate2 = ifelse((.$date==between(date, as.Date('2020-10-01'), as.Date('2020-12-31')), '1', '0')))
         
        # Coviddate1 = ifelse(.$date==between(date, as.Date('2021-01-01'), as.Date('2021-04-30'), '1', '0')),
        # Coviddate3 = ifelse(.$date==between(date, as.Date('2021-05-01'), as.Date('2021-07-31'), '1', '0')),
        # Coviddate4 = ifelse(.$date==between(date, as.Date('2021-08-01'), as.Date('2024-06-31'), '1', '0')))


#Populating each batch of date ranges individually
Covid_monthlyae_glmdemographicsCoviddate_a <- Covid_monthlyae_glmdemographicsCoviddate %>% 
  filter(between(date, as.Date('2018-01-01'), as.Date('2020-03-31'))) %>% 
  mutate(Coviddate1 = 0,
         Covidadate2 = 0, 
         Coviddate3 = 0,
         Coviddate4 = 1)

Covid_monthlyae_glmdemographicsCoviddate_b <- Covid_monthlyae_glmdemographicsCoviddate %>% 
  filter(between(date, as.Date('2020-04-01'), as.Date('2020-05-31'))) %>% 
  mutate(Coviddate1 = 1,
         Covidadate2 = 0, 
         Coviddate3 = 0,
         Coviddate4 = 0)

Covid_monthlyae_glmdemographicsCoviddate_c <- Covid_monthlyae_glmdemographicsCoviddate %>% 
  filter(between(date, as.Date('2020-06-01'), as.Date('2020-09-30'))) %>% 
  mutate(Coviddate1 = 0,
         Covidadate2 = 0, 
         Coviddate3 = 1,
         Coviddate4 = 0)

Covid_monthlyae_glmdemographicsCoviddate_d <- Covid_monthlyae_glmdemographicsCoviddate %>% 
  filter(between(date, as.Date('2020-10-01'), as.Date('2020-12-31'))) %>% 
  mutate(Coviddate1 = 0,
         Covidadate2 = 1, 
         Coviddate3 = 0,
         Coviddate4 = 0)

Covid_monthlyae_glmdemographicsCoviddate_e <- Covid_monthlyae_glmdemographicsCoviddate %>% 
  filter(between(date, as.Date('2021-01-01'), as.Date('2021-04-30'))) %>% 
  mutate(Coviddate1 = 1,
         Covidadate2 = 0, 
         Coviddate3 = 0,
         Coviddate4 = 0)

Covid_monthlyae_glmdemographicsCoviddate_f <- Covid_monthlyae_glmdemographicsCoviddate %>% 
  filter(between(date, as.Date('2021-05-01'), as.Date('2021-07-31'))) %>% 
  mutate(Coviddate1 = 0,
         Covidadate2 = 0, 
         Coviddate3 = 1,
         Coviddate4 = 0)

Covid_monthlyae_glmdemographicsCoviddate_g <- Covid_monthlyae_glmdemographicsCoviddate %>% 
  filter(between(date, as.Date('2021-08-01'), as.Date('2024-06-30'))) %>% 
  mutate(Coviddate1 = 0,
         Covidadate2 = 0, 
         Coviddate3 = 0,
         Coviddate4 = 1)

#Combining all the batches of date ranges for Covid into one dataframe
Covid_monthlyae_glmdemographicsCoviddate_all <- rbind(Covid_monthlyae_glmdemographicsCoviddate_a, Covid_monthlyae_glmdemographicsCoviddate_b)
Covid_monthlyae_glmdemographicsCoviddate_all <- rbind(Covid_monthlyae_glmdemographicsCoviddate_all, Covid_monthlyae_glmdemographicsCoviddate_c)
Covid_monthlyae_glmdemographicsCoviddate_all <- rbind(Covid_monthlyae_glmdemographicsCoviddate_all, Covid_monthlyae_glmdemographicsCoviddate_d)
Covid_monthlyae_glmdemographicsCoviddate_all <- rbind(Covid_monthlyae_glmdemographicsCoviddate_all, Covid_monthlyae_glmdemographicsCoviddate_e)
Covid_monthlyae_glmdemographicsCoviddate_all <- rbind(Covid_monthlyae_glmdemographicsCoviddate_all, Covid_monthlyae_glmdemographicsCoviddate_f)
Covid_monthlyae_glmdemographicsCoviddate_all <- rbind(Covid_monthlyae_glmdemographicsCoviddate_all, Covid_monthlyae_glmdemographicsCoviddate_g)



#When
Covid_monthlyae_glmwhen <- read_csv(here("Rawdata", "monthlyae_when_202406.csv"))

#To create a date column
#creating a new column for Year, monthnumeric and day using data from the Month column
Covid_monthlyae_glmwhen$Year <- substr(Covid_monthlyae_glmwhen$Month, 1,4)
Covid_monthlyae_glmwhen$monthnumeric <- substr(Covid_monthlyae_glmwhen$Month, 5,6)
Covid_monthlyae_glmwhen$day <- "01"

#converting from character to numeric variable
Covid_monthlyae_glmwhen$Year <- as.numeric(Covid_monthlyae_glmwhen$Year)
Covid_monthlyae_glmwhen$monthnumeric <- as.numeric(Covid_monthlyae_glmwhen$monthnumeric)
Covid_monthlyae_glmwhen$day <- as.numeric(Covid_monthlyae_glmwhen$day)

#making a date column using the Year, monthnumeric and day columns
Covid_monthlyae_glmwhen<- Covid_monthlyae_glmwhen%>% 
  mutate(date=make_date(Year, monthnumeric, day))

str(Covid_monthlyae_glmwhen)

#calculating total attendances
Covid_monthlyae_glmwhentotal <- Covid_monthlyae_glmwhen %>% 
  select(date, NumberOfAttendances) %>% 
  group_by(date) %>% 
  summarise(NumberOfAttendances=sum(NumberOfAttendances))

#DAY

#calculating total attendances by day
Covid_monthlyae_glmwhenday <- Covid_monthlyae_glmwhen %>% 
  select(date, Day, NumberOfAttendances) %>% 
  group_by(date, Day) %>% 
  summarise(NumberOfAttendances=sum(NumberOfAttendances))

#merge day attendances with total attendances to calculate proportion
Covid_monthlyae_glmwhenday <- merge(Covid_monthlyae_glmwhenday, Covid_monthlyae_glmwhentotal, by=c("date"))

#changing column names so it is clear which is total and which is for day
colnames(Covid_monthlyae_glmwhenday)<- c("date", "Day","DayAttendances","TotalAttendances")

#to calculate the proportion for each day
Covid_monthlyae_glmwhenday$Proportions <-Covid_monthlyae_glmwhenday$DayAttendances/Covid_monthlyae_glmwhenday$TotalAttendances

Covid_monthlyae_glmwhenday <- Covid_monthlyae_glmwhenday %>% select(date,Day,Proportions)

#to change the data from a long to a wide format
Covid_monthlyae_glmwhenday <- pivot_wider(Covid_monthlyae_glmwhenday, names_from = Day, values_from = Proportions)

str(Covid_monthlyae_glmwhenday)


#HOUR

#calculating total attendances by hour
Covid_monthlyae_glmwhenhour <- Covid_monthlyae_glmwhen %>% 
  select(date, Hour, NumberOfAttendances) %>% 
  group_by(date, Hour) %>% 
  summarise(NumberOfAttendances=sum(NumberOfAttendances))

#merge hour attendances with total attendances to calculate proportion
Covid_monthlyae_glmwhenhour <- merge(Covid_monthlyae_glmwhenhour, Covid_monthlyae_glmwhentotal, by=c("date"))

#changing column names so it is clear which is total and which is for hour
colnames(Covid_monthlyae_glmwhenhour)<- c("date", "Hour","HourAttendances","TotalAttendances")

#to calculate the proportion for each hour
Covid_monthlyae_glmwhenhour$Proportions <-Covid_monthlyae_glmwhenhour$HourAttendances/Covid_monthlyae_glmwhenhour$TotalAttendances

Covid_monthlyae_glmwhenhour <- Covid_monthlyae_glmwhenhour %>% select(date,Hour,Proportions)

#to change the data from a long to a wide format
Covid_monthlyae_glmwhenhour <- pivot_wider(Covid_monthlyae_glmwhenhour, names_from = Hour, values_from = Proportions)


Covid_monthlyae_glmwhenhour <- Covid_monthlyae_glmwhenhour %>% 
  rename(c("Midnighttoone" = "00:00 to 00:59",
           "Onetotwoam" = "01:00 to 01:59",
           "Twotothreeam" = "02:00 to 02:59",
           "Threetofouram" = "03:00 to 03:59",
           "Fourtofiveam" = "04:00 to 04:59",
           "Fivetosixam" = "05:00 to 05:59",
           "Sixtosevenam" = "06:00 to 06:59",
           "Seventoeightam" = "07:00 to 07:59",
           "Eighttonineam" = "08:00 to 08:59",
           "Ninetotenam" = "09:00 to 09:59",
           "Tentoelevenam" = "10:00 to 10:59",
           "Eleventonoon" = "11:00 to 11:59",
           "Noontoonepm" = "12:00 to 12:59",
           "Onetotwopm" = "13:00 to 13:59",
           "Twotothreepm" = "14:00 to 14:59",
           "Threetofourpm" = "15:00 to 15:59",
           "Fourtofivepm" = "16:00 to 16:59",
           "Fivetosixpm" = "17:00 to 17:59",
           "Sixtosevenpm" = "18:00 to 18:59",
           "Seventoeightpm" = "19:00 to 19:59",
           "Eighttoninepm" = "20:00 to 20:59",
           "Ninetotenpm" = "21:00 to 21:59",
           "Tentoelevenpm" = "22:00 to 22:59",
           "Eleventomidnight" = "23:00 to 23:59"))

str(Covid_monthlyae_glmwhenhour)

#MONTH

#29 April 2024- Chris advised that if using month, should't calculate proportion based on year's attendance. Should show as 1 if it's that month, and 0 for others.

Covid_monthlyae_glmwhenmonth <- Covid_monthlyae_glmwhen %>% 
  select(date, monthnumeric, NumberOfAttendances) %>% 
  group_by(date) %>% 
  summarise(NumberOfAttendances=sum(NumberOfAttendances))
  
#adding a new column for monthnumeric
Covid_monthlyae_glmwhenmonth$monthnumeric <-as.numeric(format(Covid_monthlyae_glmwhenmonth$date, "%m"))

#To set up the proportions for each month as a separate column
Covid_monthlyae_glmwhenmonth <- Covid_monthlyae_glmwhenmonth %>% 
  mutate(Jan = ifelse(.$monthnumeric=='1', '1', '0'),
         Feb = ifelse(.$monthnumeric=='2', '1', '0'),
         March = ifelse(.$monthnumeric=='3', '1', '0'),
         April = ifelse(.$monthnumeric=='4', '1', '0'),
         May = ifelse(.$monthnumeric=='5', '1', '0'),
         June = ifelse(.$monthnumeric=='6', '1', '0'),
         July = ifelse(.$monthnumeric=='7', '1', '0'),
         August = ifelse(.$monthnumeric=='8', '1', '0'),
         Sept = ifelse(.$monthnumeric=='9', '1', '0'),
         Oct = ifelse(.$monthnumeric=='10', '1', '0'),
         Nov = ifelse(.$monthnumeric=='11', '1', '0'),
         Dec = ifelse(.$monthnumeric=='12', '1', '0'))

str(Covid_monthlyae_glmwhenmonth)

Covid_monthlyae_glmwhenmonth <- Covid_monthlyae_glmwhenmonth %>% 
  select(date, Jan, Feb, March, April, May, June, July, August, Sept, Oct, Nov, Dec)

#To change from character to numeric

Covid_monthlyae_glmwhenmonth$Jan <- as.numeric(Covid_monthlyae_glmwhenmonth$Jan)
Covid_monthlyae_glmwhenmonth$Feb <- as.numeric(Covid_monthlyae_glmwhenmonth$Feb)
Covid_monthlyae_glmwhenmonth$March <- as.numeric(Covid_monthlyae_glmwhenmonth$March)
Covid_monthlyae_glmwhenmonth$April <- as.numeric(Covid_monthlyae_glmwhenmonth$April)
Covid_monthlyae_glmwhenmonth$May <- as.numeric(Covid_monthlyae_glmwhenmonth$May)
Covid_monthlyae_glmwhenmonth$June <- as.numeric(Covid_monthlyae_glmwhenmonth$June)
Covid_monthlyae_glmwhenmonth$July <- as.numeric(Covid_monthlyae_glmwhenmonth$July)
Covid_monthlyae_glmwhenmonth$August <- as.numeric(Covid_monthlyae_glmwhenmonth$August)
Covid_monthlyae_glmwhenmonth$Sept <- as.numeric(Covid_monthlyae_glmwhenmonth$Sept)
Covid_monthlyae_glmwhenmonth$Oct <- as.numeric(Covid_monthlyae_glmwhenmonth$Oct)
Covid_monthlyae_glmwhenmonth$Nov <- as.numeric(Covid_monthlyae_glmwhenmonth$Nov)
Covid_monthlyae_glmwhenmonth$Dec <- as.numeric(Covid_monthlyae_glmwhenmonth$Dec)



