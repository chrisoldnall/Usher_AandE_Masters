#Scottish A&E data - logistic regression

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

#LOGISTIC REGRESSION

#preparing dataset for sex

##to get the total attendances for a particular month
whoattends_sex_monthtotal <- whoattends_sex_total %>% 
  select(Month, Attendances) %>% 
  group_by(Month) %>% 
  summarise(Attendances=sum(Attendances)) %>% 
  rename(c("Total_attendances" = "Attendances"))

##to add the total attendances values to the dataframe
whoattends_sex_proportions <- merge(whoattends_sex_total, whoattends_sex_monthtotal, by=c("Month"))

##to calculate the proportion for each sex
whoattends_sex_proportions$Proportions <- whoattends_sex_proportions$Attendances/whoattends_sex_proportions$Total_attendances

whoattends_sex_proportions <- whoattends_sex_proportions %>% select(Month, Sex, Proportions)

##to change the data from a long to a wide format
whoattends_sex_proportions <- pivot_wider(whoattends_sex_proportions, names_from = Sex, values_from = Proportions)

#preparing dataset for age

##to get the total attendances for a particular month
agegroupScot_monthtotal <- agegroupScot %>% 
  select(Month, Attendances) %>% 
  group_by(Month) %>% 
  summarise(Attendances=sum(Attendances)) %>% 
  rename(c("Total_attendances" = "Attendances"))

##to add the total attendances values to the dataframe
agegroupScot_proportions <- agegroupScot %>% 
  select(Month, Age, Attendances) %>% 
  group_by(Month, Age) %>% 
  summarise(Attendances=sum(Attendances))

agegroupScot_proportions <- merge(agegroupScot_proportions, agegroupScot_monthtotal, by=c("Month"))

##to calculate the proportion for each age
agegroupScot_proportions$Proportions <- agegroupScot_proportions$Attendances/agegroupScot_proportions$Total_attendances

agegroupScot_proportions <- agegroupScot_proportions %>% select(Month, Age, Proportions)

##to change the data from a long to a wide format
agegroupScot_proportions <- pivot_wider(agegroupScot_proportions, names_from = Age, values_from = Proportions)

#preparing dataset for deprivation

##to get the total attendances for a particular month
deprivationScot_monthtotal <- deprivationScot %>% 
  select(Month, Attendances) %>% 
  group_by(Month) %>% 
  summarise(Attendances=sum(Attendances)) %>% 
  rename(c("Total_attendances" = "Attendances"))

##to add the total attendances values to the dataframe
deprivationScot_proportions <- deprivationScot %>% 
  select(Month, Deprivation, Attendances) %>% 
  group_by(Month, Deprivation) %>% 
  summarise(Attendances=sum(Attendances))

deprivationScot_proportions <- merge(deprivationScot_proportions, deprivationScot_monthtotal, by=c("Month"))

##to calculate the proportion for each deprivation status
deprivationScot_proportions$Proportions <- deprivationScot_proportions$Attendances/deprivationScot_proportions$Total_attendances

deprivationScot_proportions <- deprivationScot_proportions %>% select(Month, Deprivation, Proportions)

##to change the data from a long to a wide format
deprivationScot_proportions <- pivot_wider(deprivationScot_proportions, names_from = Deprivation, values_from = Proportions)

#preparing dataset for day of the week

##to get the total attendances for a particular month
when_dayofweek_monthtotal <- when_dayofweek %>% 
  select(Month, Average) %>% 
  group_by(Month) %>% 
  summarise(Average=sum(Average)) %>% 
  rename(c("Total_average" = "Average"))

##to add the total average values to the dataframe
when_dayofweek_proportions <- when_dayofweek %>% 
  select(Month, Day, Average) %>% 
  group_by(Month, Day) %>% 
  summarise(Average=sum(Average))

when_dayofweek_proportions <- merge(when_dayofweek_proportions, when_dayofweek_monthtotal, by=c("Month"))

##to calculate the proportion for each day
when_dayofweek_proportions$Proportions <- when_dayofweek_proportions$Average/when_dayofweek_proportions$Total_average

when_dayofweek_proportions <- when_dayofweek_proportions %>% select(Month, Day, Proportions)

##to change the data from a long to a wide format
when_dayofweek_proportions <- pivot_wider(when_dayofweek_proportions, names_from = Day, values_from = Proportions)

#preparing dataset for arrival hour

##to get the total attendances for a particular month
arrivalhrScot_hourtotal <- arrivalhrScot %>% 
  select(Month, Attendances) %>% 
  group_by(Month) %>% 
  summarise(Attendances=sum(Attendances)) %>% 
  rename(c("Total_attendances" = "Attendances"))

##to add the total attendances values to the dataframe
arrivalhrScot_proportions <- arrivalhrScot %>% 
  select(Month, Hour, Attendances) %>% 
  group_by(Month, Hour) %>% 
  summarise(Attendances=sum(Attendances))

arrivalhrScot_proportions <- merge(arrivalhrScot_proportions, arrivalhrScot_hourtotal, by=c("Month"))

##to calculate the proportion for each hour
arrivalhrScot_proportions$Proportions <- arrivalhrScot_proportions$Attendances/arrivalhrScot_proportions$Total_attendances

arrivalhrScot_proportions <- arrivalhrScot_proportions %>% select(Month, Hour, Proportions)

##to change the data from a long to a wide format
arrivalhrScot_proportions <- pivot_wider(arrivalhrScot_proportions, names_from = Hour, values_from = Proportions)

#preparing dataset for ED/MIU

##ae_monthly_total has the total attendances for a particular month

#to get the number of attendances by department type from 2017-12-31 to 2023-07-01
#note the dates are different than for age, sex, deprivation, arrival hour, day of the week
ae_monthly_attendance_proportions <- ae_monthly_attendance %>% 
  select(MonthEndingDate, DepartmentType, NumberOfAttendancesAll) %>% 
  group_by(MonthEndingDate, DepartmentType) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll)) %>% 
  rename(c("Attendances" = "NumberOfAttendancesAll")) %>% 
  filter(between(MonthEndingDate, as.Date('2017-12-31'), as.Date('2023-07-01')))

ae_monthly_total_2018to2023 <- ae_monthly_total %>% 
  filter(between(MonthEndingDate, as.Date('2017-12-31'), as.Date('2023-07-01')))

ae_monthly_attendance_proportions <- 
  merge(ae_monthly_attendance_proportions, ae_monthly_total_2018to2023, by=c("MonthEndingDate"))

#The dates in ae_monthly_attendances (end of the month) is not the same as in age, deprivation, sex, etc tables (start of the month).
#Assuming that the end of the month in ae_monthly_attendances relates to the start of the month.
#obtaining a column of the dates based on the start of the month, so that it can be replaced in the ae_monthly_attendance_proportions dataframe.
Month_2018to2023 <- whoattends_sex %>% 
  filter(Sex=="Female"| Sex=="Male") %>% 
  group_by(Month, Sex) %>% 
  summarise(Attendances=sum(Attendances), 
            `Rate/100,000`=sum(`Rate/100,000`)) %>% 
  select(Month)

#adding the new date column 
ae_monthly_attendance_proportions <- cbind(ae_monthly_attendance_proportions, Month = Month_2018to2023$Month)


##to calculate the proportion for each department type
ae_monthly_attendance_proportions$Proportions <- ae_monthly_attendance_proportions$Attendances/ae_monthly_attendance_proportions$NumberOfAttendancesAll

ae_monthly_attendance_proportionsnewdate <- ae_monthly_attendance_proportions %>% select(Month, DepartmentType, Proportions)

##to change the data from a long to a wide format
ae_monthly_attendance_proportionsnewdate <- pivot_wider(ae_monthly_attendance_proportionsnewdate, names_from = DepartmentType, values_from = Proportions)

ae_monthly_attendance_proportionsnewdate<-cbind(ae_monthly_attendance_proportionsnewdate, Total_attendances = ae_monthly_total_2018to2023$NumberOfAttendancesAll)

#combine dataframe for sex, age, deprivation, day of the week, arrival hour, department type

#whoattends_sex_proportions
#agegroupScot_proportions
#deprivationScot_proportions
#when_dayofweek_proportions
#arrivalhrScot_proportions
#ae_monthly_attendance_proportionsnewdate

#combining ae_monthly_attendance_proportionsnewdate with whoattends_sex_proportions
sexagesimddayhourdepttype_proportions <- 
  merge(ae_monthly_attendance_proportionsnewdate, whoattends_sex_proportions, by=c("Month"))

#both age and sex have an unknown column - so to rename the Unknown columns
whoattends_sex_proportions <- whoattends_sex_proportions %>% 
  rename(c("Unknownsex" = "Unknown"))

agegroupScot_proportions <- agegroupScot_proportions %>% 
  rename(c("Unknownage" = "Unknown"))

#adding age to the dataframe
sexagesimddayhourdepttype_proportions <- 
  merge(sexagesimddayhourdepttype_proportions, agegroupScot_proportions, by=c("Month"))

#renaming the unknown column in deprivation
deprivationScot_proportions <- deprivationScot_proportions %>% 
  rename(c("Unknownsimd" = "Unknown"))

#adding deprivation
sexagesimddayhourdepttype_proportions <- 
  merge(sexagesimddayhourdepttype_proportions, deprivationScot_proportions, by=c("Month"))

#adding day of the week
sexagesimddayhourdepttype_proportions <- 
  merge(sexagesimddayhourdepttype_proportions, when_dayofweek_proportions, by=c("Month"))

#adding arrival hour
sexagesimddayhourdepttype_proportions <- 
  merge(sexagesimddayhourdepttype_proportions, arrivalhrScot_proportions, by=c("Month"))

#preparing dataset for HB
ae_byboard2018to2023 <- ae_byboard %>% 
  filter(between(MonthEndingDate, as.Date('2017-12-31'), as.Date('2023-07-01'))) %>% 
  rename(c("Attendances" = "NumberOfAttendancesAll"))

ae_byboard2018to2023_proportions <- merge(ae_byboard2018to2023, ae_monthly_total_2018to2023, by=c("MonthEndingDate"))

##to calculate the proportion for HB
ae_byboard2018to2023_proportions$Proportions <- ae_byboard2018to2023_proportions$Attendances/ae_byboard2018to2023_proportions$NumberOfAttendancesAll

ae_byboard2018to2023_proportions <- ae_byboard2018to2023_proportions %>% select(MonthEndingDate, NHSBoardName, Proportions)

##to change the data from a long to a wide format
ae_byboard2018to2023_proportions <- pivot_wider(ae_byboard2018to2023_proportions, names_from = NHSBoardName, values_from = Proportions)

#aligning the date for ae from end of the month to beginning of the month
ae_byboard2018to2023_proportionsnewdate <- cbind(ae_byboard2018to2023_proportions, Month = whoattends_sex_monthtotal$Month)

#selecting only the columns needed for ae_byboard2018to2023_proportionsnewdate
ae_byboard2018to2023_proportionsnewdate <- ae_byboard2018to2023_proportionsnewdate %>% 
  select(-MonthEndingDate)

#adding HB proportions to the dataframe for linear modelling
sexagesimddayhourdepttypeHB_proportions <- 
  merge(sexagesimddayhourdepttype_proportions, ae_byboard2018to2023_proportionsnewdate, by=c("Month"))

#renaming the column names so that they can be added to the linear model
sexagesimddayhourdepttypeHB_proportions <- sexagesimddayhourdepttypeHB_proportions %>% 
  rename(c("Under18" = "Under 18",
           "EighteentoTwentyfour" = "18-24",
           "TwentyfivetoThirtynine" = "25-39",
           "FortytoSixtyfour" = "40-64",
           "SixtyfivetoSeventyfour" = "65-74",
           "Seventyfiveplus" = "75 plus"))

sexagesimddayhourdepttypeHB_proportions <- sexagesimddayhourdepttypeHB_proportions %>% 
  rename(c("MIU_Other" = "MIU/Other",
           "SIMDOne" = "1",
           "SIMDTwo" ="2",
           "SIMDThree" = "3",
           "SIMDFour" = "4",
           "SIMDFive" = "5",
           "Midnighttoone" = "00:00 to 00:59",
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

sexagesimddayhourdepttypeHB_proportions <- sexagesimddayhourdepttypeHB_proportions %>% 
  rename(c("NHSAyrshireandArran" = "NHS Ayrshire & Arran",
           "NHSBorders" = "NHS Borders",
           "NHSFife" = "NHS Fife",
           "NHSShetland" = "NHS Shetland",
           "NHSLanarkshire" = "NHS Lanarkshire",
           "NHSDumfriesandGalloway" = "NHS Dumfries & Galloway",
           "NHSForthValley" = "NHS Forth Valley",
           "NHSGrampian" = "NHS Grampian",
           "NHSWesternIsles" = "NHS Western Isles",
           "NHSOrkney" = "NHS Orkney",
           "NHSTayside" = "NHS Tayside",
           "NHSGreaterGlasgowandClyde" = "NHS Greater Glasgow & Clyde",
           "NHSHighland" = "NHS Highland",
           "NHSLothian" = "NHS Lothian"))


#MONTH - TO FIGURE OUT HOW TO ADD THIS TO THE MODEL
#To add month to the dataframe for linear modelling, working out the proportions
monthattendances_proportions <- sexagesimddayhourdepttypeHB_proportions %>% 
  select(Month, Total_attendances)

#adding a new column for month
monthattendances_proportions$monthnumeric <-as.numeric(format(sexagesimddayhourdepttypeHB_proportions$Month, "%m"))
monthattendances_proportions$yearnumeric <-as.numeric(format(sexagesimddayhourdepttypeHB_proportions$Month, "%Y"))

#creating the tablet to calculate proportions of attendances per month by year
#calculating annual total attendances for each year from 2018-2023
monthattendances_proportions %>% 
  filter(yearnumeric==2018) %>% 
  summarise(Total_attendances=sum(Total_attendances))
#2018=1672650

monthattendances_proportions %>% 
  filter(yearnumeric==2019) %>% 
  summarise(Total_attendances=sum(Total_attendances))
#2019=1737468

monthattendances_proportions %>% 
  filter(yearnumeric==2020) %>% 
  summarise(Total_attendances=sum(Total_attendances))
#2020=1323031

monthattendances_proportions %>% 
  filter(yearnumeric==2021) %>% 
  summarise(Total_attendances=sum(Total_attendances))
#2021=1390547

monthattendances_proportions %>% 
  filter(yearnumeric==2022) %>% 
  summarise(Total_attendances=sum(Total_attendances))
#2022=1512675

monthattendances_proportions %>% 
  filter(yearnumeric==2023) %>% 
  summarise(Total_attendances=sum(Total_attendances))
#2023=884475

#calculating the monthly proportions for each year from 2018-2023
monthattendances_proportions2018 <- monthattendances_proportions %>% 
  filter(yearnumeric==2018) %>% 
  mutate(monthproportions=Total_attendances/1672650)

monthattendances_proportions2019 <- monthattendances_proportions %>% 
  filter(yearnumeric==2019) %>% 
  mutate(monthproportions=Total_attendances/1737468)

monthattendances_proportions2020 <- monthattendances_proportions %>% 
  filter(yearnumeric==2020) %>% 
  mutate(monthproportions=Total_attendances/1323031)

monthattendances_proportions2021 <- monthattendances_proportions %>% 
  filter(yearnumeric==2021) %>% 
  mutate(monthproportions=Total_attendances/1390547)

monthattendances_proportions2022 <- monthattendances_proportions %>% 
  filter(yearnumeric==2022) %>% 
  mutate(monthproportions=Total_attendances/1512675)

monthattendances_proportions2023 <- monthattendances_proportions %>% 
  filter(yearnumeric==2023) %>% 
  mutate(monthproportions=Total_attendances/884475)

monthattendances_proportions20182023 <- rbind(monthattendances_proportions2018, monthattendances_proportions2019, monthattendances_proportions2020, monthattendances_proportions2021, monthattendances_proportions2022, monthattendances_proportions2023)

#To set up the proportions for each month as a separate column
monthattendances_proportions20182023wide <- monthattendances_proportions20182023 %>% 
  mutate(Jan = ifelse(.$monthnumeric=='1', .$monthproportions, '0'),
         Feb = ifelse(.$monthnumeric=='2', .$monthproportions, '0'),
         March = ifelse(.$monthnumeric=='3', .$monthproportions, '0'),
         April = ifelse(.$monthnumeric=='4', .$monthproportions, '0'),
         May = ifelse(.$monthnumeric=='5', .$monthproportions, '0'),
         June = ifelse(.$monthnumeric=='6', .$monthproportions, '0'),
         July = ifelse(.$monthnumeric=='7', .$monthproportions, '0'),
         August = ifelse(.$monthnumeric=='8', .$monthproportions, '0'),
         Sept = ifelse(.$monthnumeric=='9', .$monthproportions, '0'),
         Oct = ifelse(.$monthnumeric=='10', .$monthproportions, '0'),
         Nov = ifelse(.$monthnumeric=='11', .$monthproportions, '0'),
         Dec = ifelse(.$monthnumeric=='12', .$monthproportions, '0'))

#To change from character to numeric

monthattendances_proportions20182023wide$Jan <- as.numeric(monthattendances_proportions20182023wide$Jan)
monthattendances_proportions20182023wide$Feb <- as.numeric(monthattendances_proportions20182023wide$Feb)
monthattendances_proportions20182023wide$March <- as.numeric(monthattendances_proportions20182023wide$March)
monthattendances_proportions20182023wide$April <- as.numeric(monthattendances_proportions20182023wide$April)
monthattendances_proportions20182023wide$May <- as.numeric(monthattendances_proportions20182023wide$May)
monthattendances_proportions20182023wide$June <- as.numeric(monthattendances_proportions20182023wide$June)
monthattendances_proportions20182023wide$July <- as.numeric(monthattendances_proportions20182023wide$July)
monthattendances_proportions20182023wide$August <- as.numeric(monthattendances_proportions20182023wide$August)
monthattendances_proportions20182023wide$Sept <- as.numeric(monthattendances_proportions20182023wide$Sept)
monthattendances_proportions20182023wide$Oct <- as.numeric(monthattendances_proportions20182023wide$Oct)
monthattendances_proportions20182023wide$Nov <- as.numeric(monthattendances_proportions20182023wide$Nov)
monthattendances_proportions20182023wide$Dec <- as.numeric(monthattendances_proportions20182023wide$Dec)

monthattendances_proportions20182023wideJantoDec <- monthattendances_proportions20182023wide %>% 
  select(Jan, Feb, March, April, May, June, July, August, Sept, Oct, Nov, Dec)

#combine month proportions for 2018 to 2023 to sexagesimddayhourdepttypeHB_proportions
sexagesimddayhourdepttypeHBmonth_proportions<- cbind(sexagesimddayhourdepttypeHB_proportions, monthattendances_proportions20182023wideJantoDec)

#29 April 2024- Chris advised that if using month, should't calculate proportion based on year's attendance. Should show as 1 if it's that month, and 0 for others.
#Recreating dataframe for month - adding new Updated columns with new values
sexagesimddayhourdepttypeHBupdatedmonth_proportions <- sexagesimddayhourdepttypeHB_proportions

#adding a new column for month
sexagesimddayhourdepttypeHBupdatedmonth_proportions$monthnumeric <-as.numeric(format(sexagesimddayhourdepttypeHBupdatedmonth_proportions$Month, "%m"))
sexagesimddayhourdepttypeHBupdatedmonth_proportions$yearnumeric <-as.numeric(format(sexagesimddayhourdepttypeHBupdatedmonth_proportions$Month, "%Y"))

#To set up the proportions for each month as a separate column
sexagesimddayhourdepttypeHBupdatedmonth_proportions <- sexagesimddayhourdepttypeHBupdatedmonth_proportions %>% 
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

#To change from character to numeric

sexagesimddayhourdepttypeHBupdatedmonth_proportions$Jan <- as.numeric(sexagesimddayhourdepttypeHBupdatedmonth_proportions$Jan)
sexagesimddayhourdepttypeHBupdatedmonth_proportions$Feb <- as.numeric(sexagesimddayhourdepttypeHBupdatedmonth_proportions$Feb)
sexagesimddayhourdepttypeHBupdatedmonth_proportions$March <- as.numeric(sexagesimddayhourdepttypeHBupdatedmonth_proportions$March)
sexagesimddayhourdepttypeHBupdatedmonth_proportions$April <- as.numeric(sexagesimddayhourdepttypeHBupdatedmonth_proportions$April)
sexagesimddayhourdepttypeHBupdatedmonth_proportions$May <- as.numeric(sexagesimddayhourdepttypeHBupdatedmonth_proportions$May)
sexagesimddayhourdepttypeHBupdatedmonth_proportions$June <- as.numeric(sexagesimddayhourdepttypeHBupdatedmonth_proportions$June)
sexagesimddayhourdepttypeHBupdatedmonth_proportions$July <- as.numeric(sexagesimddayhourdepttypeHBupdatedmonth_proportions$July)
sexagesimddayhourdepttypeHBupdatedmonth_proportions$August <- as.numeric(sexagesimddayhourdepttypeHBupdatedmonth_proportions$August)
sexagesimddayhourdepttypeHBupdatedmonth_proportions$Sept <- as.numeric(sexagesimddayhourdepttypeHBupdatedmonth_proportions$Sept)
sexagesimddayhourdepttypeHBupdatedmonth_proportions$Oct <- as.numeric(sexagesimddayhourdepttypeHBupdatedmonth_proportions$Oct)
sexagesimddayhourdepttypeHBupdatedmonth_proportions$Nov <- as.numeric(sexagesimddayhourdepttypeHBupdatedmonth_proportions$Nov)
sexagesimddayhourdepttypeHBupdatedmonth_proportions$Dec <- as.numeric(sexagesimddayhourdepttypeHBupdatedmonth_proportions$Dec)

#Including time in the data frame
#example given: df$Time <- as.numeric(df$Date - min(df$Date)) + 1
sexagesimddayhourdepttypeHBmonthTime_proportions <- sexagesimddayhourdepttypeHBmonth_proportions %>% 
  mutate(Time = as.numeric(sexagesimddayhourdepttypeHBmonth_proportions$Month - min(sexagesimddayhourdepttypeHBmonth_proportions$Month)) + 1)

#Including time in the dataframe without month
sexagesimddayhourdepttypeHBTime_proportions <- sexagesimddayhourdepttypeHB_proportions %>% 
  mutate(Time = as.numeric(sexagesimddayhourdepttypeHB_proportions$Month - min(sexagesimddayhourdepttypeHB_proportions$Month)) + 1)

#29 April 2024- Chris advised that if using month, should't calculate proportion based on year's attendance. Should show as 1 if it's that month, and 0 for others.
#Recreating dataframe which includes Time for Updated month
sexagesimddayhourdepttypeHBupdatedmonthTime_proportions <- sexagesimddayhourdepttypeHBupdatedmonth_proportions %>% 
  mutate(Time = as.numeric(sexagesimddayhourdepttypeHBupdatedmonth_proportions$Month - min(sexagesimddayhourdepttypeHBupdatedmonth_proportions$Month)) + 1)

#don't need to use this, just for information, this is how to delete a column from a dataframe
#sexagesimddayhourdepttypeHB_proportions <-sexagesimddayhourdepttypeHB_proportions %>% select(-monthproportions)

#LM for sex (excludes Unknown age)
modelsex <- lm(Total_attendances ~ Male + Female, data = sexagesimddayhourdepttypeHB_proportions)
summary(modelsex)

#LM for sex and age (excluded Unknown and 75 plus)
#Looks like SixtyfivetoSeventyfour and Seventyfiveplus maybe correlated, therefore removed Seventyfiveplus from the linear model
modelsexage <- lm(Total_attendances ~ Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour, data = sexagesimddayhourdepttypeHB_proportions)
summary(modelsexage)

#Coefficients: (1 not defined because of singularities) NA returned for Seventyfiveplus
#This indicates that two or more predictor variables in the model have a perfect linear relationship and thus not every regression coefficient in the model can be estimated.
#run the cor() function against age data to find out which variables is causing this error
agegroupScot_proportions_forcor <- sexagesimddayhourdepttypeHB_proportions %>% 
  select(Total_attendances, Under18, EighteentoTwentyfour, TwentyfivetoThirtynine, FortytoSixtyfour, SixtyfivetoSeventyfour, Seventyfiveplus, Unknownage)

cor(agegroupScot_proportions_forcor)
#LM for sex (excluded unknownsex) and age (excluded Unknownage and 75 plus) and deprivation (excluding UnknownSIMD)
#Looks like SixtyfivetoSeventyfour and Seventyfiveplus maybe correlated, therefore removed Seventyfiveplus from the linear model
modelsexageSIMD <- lm(Total_attendances ~ Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMDOne +SIMDTwo + SIMDThree + SIMDFour + SIMDFive, data = sexagesimddayhourdepttypeHB_proportions)
summary(modelsexageSIMD)

#LM for sex (excluded unknownsex) and age (excluded Unknown and 75 plus), deprivation (excluding UnknownSIMD), day of the week (excluding Monday)
#Looks like SixtyfivetoSeventyfour and Seventyfiveplus maybe correlated, therefore removed Seventyfiveplus from the linear model
modelsexageSIMDday <- lm(Total_attendances ~ Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMDOne +SIMDTwo + SIMDThree + SIMDFour + SIMDFive + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday, data = sexagesimddayhourdepttypeHB_proportions)
summary(modelsexageSIMDday)

#LM for sex (excluded unknownsex) and age (excluded Unknown and 75 plus), deprivation (excluding UnknownSIMD), day of the week (excluding Monday), department type (exclude MIU/Other)
#Looks like SixtyfivetoSeventyfour and Seventyfiveplus maybe correlated, therefore removed Seventyfiveplus from the linear model
modelsexageSIMDdaytype <- lm(Total_attendances ~ Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMDOne+  SIMDTwo + SIMDThree + SIMDFour + SIMDFive + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED, data = sexagesimddayhourdepttypeHB_proportions)
summary(modelsexageSIMDdaytype)

#LM for sex and age (excluded Unknown and 75 plus), deprivation (excluding SIMDOne), day of the week (excluding Monday), department type (exclude MIU/Other) and hour (forgot to exclude MidnighttoOneam maybe why the results returned there were values that were correlated)
#Looks like SixtyfivetoSeventyfour and Seventyfiveplus maybe correlated, therefore removed Seventyfiveplus from the linear model
modelsexageSIMDdaytypehour <- lm(Total_attendances ~ Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMDOne +SIMDTwo + SIMDThree + SIMDFour + SIMDFive + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED + Midnighttoone + Onetotwoam + Twotothreeam + Threetofouram + Fourtofiveam + Fivetosixam + Sixtosevenam + Seventoeightam + Eighttonineam + Ninetotenam + Tentoelevenam + Eleventonoon + Noontoonepm + Onetotwopm + Twotothreepm + Threetofourpm + Fourtofivepm + Fivetosixpm + Sixtosevenpm + Seventoeightpm + Eighttoninepm + Ninetotenpm + Tentoelevenpm + Eleventomidnight, data = sexagesimddayhourdepttypeHB_proportions)
summary(modelsexageSIMDdaytypehour)
#Coefficients: (1 not defined because of singularities) NA returned for Eleventomidnight

#This indicates that two or more predictor variables in the model have a perfect linear relationship and thus not every regression coefficient in the model can be estimated.
#run the cor() function against hour data to find out which variables is causing this error
arrivalhrScot_proportions_forcor <- sexagesimddayhourdepttypeHB_proportions %>% 
  select(Total_attendances, Midnighttoone, Onetotwoam, Twotothreeam, Threetofouram, Fourtofiveam, Fivetosixam, Sixtosevenam, Seventoeightam, Eighttonineam, Ninetotenam, Tentoelevenam, Eleventonoon, Noontoonepm, Onetotwopm, Twotothreepm, Threetofourpm, Fourtofivepm, Fivetosixpm, Sixtosevenpm, Seventoeightpm, Eighttoninepm, Ninetotenpm, Tentoelevenpm, Eleventomidnight)

cor(arrivalhrScot_proportions_forcor)
#Looks like Onetotwoam and Twotothreeam maybe correlated, therefore removed Onetotwoam from the linear model
modelsexageSIMDdaytypehour <- lm(Total_attendances ~ Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMDOne + SIMDTwo + SIMDThree + SIMDFour + SIMDFive + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED + Midnighttoone + Twotothreeam + Threetofouram + Fourtofiveam + Fivetosixam + Sixtosevenam + Seventoeightam + Eighttonineam + Ninetotenam + Tentoelevenam + Eleventonoon + Noontoonepm + Onetotwopm + Twotothreepm + Threetofourpm + Fourtofivepm + Fivetosixpm + Sixtosevenpm + Seventoeightpm + Eighttoninepm + Ninetotenpm + Tentoelevenpm + Eleventomidnight, data = sexagesimddayhourdepttypeHB_proportions)
summary(modelsexageSIMDdaytypehour)

#LM for sex and age (excluded Unknown and 75 plus), deprivation (excluding unknownSIMD), day of the week (excluding Monday), department type (exclude MIU/Other) and hour (excluding Onetotwoam) and HB (excluding NHSAyrshireandArran)
#Looks like SixtyfivetoSeventyfour and Seventyfiveplus maybe correlated, therefore removed Seventyfiveplus from the linear model
#Looks like Onetotwoam and Twotothreeam maybe correlated, therefore removed Onetotwoam from the linear model
modelsexageSIMDdaytypehourHB <- lm(Total_attendances ~ Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMDOne + SIMDTwo + SIMDThree + SIMDFour + SIMDFive + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED + Midnighttoone + Twotothreeam + Threetofouram + Fourtofiveam + Fivetosixam + Sixtosevenam + Seventoeightam + Eighttonineam + Ninetotenam + Tentoelevenam + Eleventonoon + Noontoonepm + Onetotwopm + Twotothreepm + Threetofourpm + Fourtofivepm + Fivetosixpm + Sixtosevenpm + Seventoeightpm + Eighttoninepm + Ninetotenpm + Tentoelevenpm + Eleventomidnight + NHSBorders + NHSFife + NHSShetland + NHSLanarkshire + NHSDumfriesandGalloway+ NHSForthValley + NHSGrampian + NHSWesternIsles + NHSOrkney + NHSTayside + NHSGreaterGlasgowandClyde + NHSHighland + NHSLothian, data = sexagesimddayhourdepttypeHB_proportions)
summary(modelsexageSIMDdaytypehourHB)

#LM for sex and age (excluded Unknown and 75 plus), deprivation (excluding unknownSIMD), day of the week (excluding Monday), department type (exclude MIU/Other) and hour (excluding Onetotwoam) and HB (excluding NHSAyrshireandArran)
#Looks like SixtyfivetoSeventyfour and Seventyfiveplus maybe correlated, therefore removed Seventyfiveplus from the linear model
#Looks like Onetotwoam and Twotothreeam maybe correlated, therefore removed Onetotwoam from the linear model
#Excluded Jan
#~NOT SURE WHY WEIRD RESULTS RETURNED
modelsexageSIMDdaytypehourHBmonth <- lm(Total_attendances ~ Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMDOne + SIMDTwo + SIMDThree + SIMDFour + SIMDFive + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED + Midnighttoone + Twotothreeam + Threetofouram + Fourtofiveam + Fivetosixam + Sixtosevenam + Seventoeightam + Eighttonineam + Ninetotenam + Tentoelevenam + Eleventonoon + Noontoonepm + Onetotwopm + Twotothreepm + Threetofourpm + Fourtofivepm + Fivetosixpm + Sixtosevenpm + Seventoeightpm + Eighttoninepm + Ninetotenpm + Tentoelevenpm + Eleventomidnight + NHSBorders + NHSFife + NHSShetland + NHSLanarkshire + NHSDumfriesandGalloway+ NHSForthValley + NHSGrampian + NHSWesternIsles + NHSOrkney + NHSTayside + NHSGreaterGlasgowandClyde + NHSHighland + NHSLothian + Feb + March + April + May + June + July + August + Sept + Oct + Nov + Dec, data = sexagesimddayhourdepttypeHBmonth_proportions)
summary(modelsexageSIMDdaytypehourHBmonth)

