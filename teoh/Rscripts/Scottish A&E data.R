#Scottish A&E data

#install.packages("tidyverse")
#install.packages("here")
#install.packages("dplyr")
#install.packages("ggplot2")

library(tidyverse)
library(here)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)

#ATTENDANCESALL
#Loading A&E monthly attendance and waiting times csv file
ae_monthly_attendance <- read_csv(here("Rawdata", "2023-09-05-ae-monthly-attendance-and-waiting-times-data.csv"))

#Eyeballing the dataset
view(ae_monthly_attendance)
head(ae_monthly_attendance)
str(ae_monthly_attendance)
summary(ae_monthly_attendance)

#Checking if there are null values for NumberOfAttendancesAll, result returned zero
sum(is.na(ae_monthly_attendance$NumberOfAttendancesAll))

#NHS Lothian, LocationCode= S314H, Royal Infirmary Of Edinburgh - ED
#NHS Highland, LocationCode= C108H, Islay Hospital

#Creating a tibble with Edinburgh and Islay hospital monthly NumberofAttendancesAll
Edinburgh_Islay <- ae_monthly_attendance %>% 
  filter(LocationCode == "S314H"|LocationCode == "C108H") %>% 
  select (MonthEndingDate, LocationCode, NumberOfAttendancesAll)

View(Edinburgh_Islay)

#Line plot of the Edinburgh_Islay NumberOfAttendancesAll over time

#Creating the basic plot
Edinburgh_Islay_plot <- ggplot(data=Edinburgh_Islay, aes(x=MonthEndingDate, y=NumberOfAttendancesAll, group=LocationCode, color=LocationCode)) +
  geom_line() +
  labs(title = "Number of attendances in Royal Infirmary of Edinburgh and Islay Hospital",
       x = "Date",
       y = "Number of attendances")

#Amending the legend labels, name and position and saving the plot
png(file = "Output/ED_Edinburgh_Islay_monthlyattendancegraph.png")
Edinburgh_Islay_plot + scale_colour_discrete(name="Hospital",
  breaks=c("C108H", "S314H"),
  labels=c("Islay Hospital", "Royal Informary of Edinburgh")) +
  theme(legend.position="top")
dev.off()

#Creating a line plot for different health boards

ae_byboard <- ae_monthly_attendance %>% 
  group_by(NHSBoardName, MonthEndingDate) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll))

png(file = "Output/AttendancebyHB.png")
ggplot(data=ae_byboard, aes(x=MonthEndingDate, y=NumberOfAttendancesAll, group=NHSBoardName, color=NHSBoardName)) +
  geom_line() +
  labs(title = "Number of attendances by Scottish Health Boards",
       x = "Date",
       y = "Number of attendances")
dev.off() 

#GENDER
whoattends_sex <- read_excel("Rawdata/2023-09-05-whoattends-sex.xlsx", 
              sheet = "NHSScotland")

#Eyeballing the data
view(whoattends_sex)
str(whoattends_sex)

#Change Month from a character to a date using Lubridate
whoattends_sex$Month <- ymd(whoattends_sex$Month)
class(whoattends_sex$Month)

#Filtering and plotting a line graph just for ED by sex
ED_whoattends_sex <- whoattends_sex %>% 
  filter(Type == "ED Only")

png(file = "Output/ED_whoattends_sexgraph.png")
ggplot(data=ED_whoattends_sex, aes(x=Month, y=Attendances, group=Sex, color=Sex)) +
  geom_line() +
  labs(title = "Number of attendances in Emergency Departments (ED) by Sex",
       x = "Date",
       y = "Number of Attendances")
dev.off()

#Filtering and plotting a line graph just for MIU/Other Only by sex
MIU_whoattends_sex <- whoattends_sex %>% 
  filter(Type == "MIU/Other Only")

png(file = "Output/MIU_whoattends_sexgraph.png")
ggplot(data=MIU_whoattends_sex, aes(x=Month, y=Attendances, group=Sex, color=Sex)) +
  geom_line() +
  labs(title = "Number of emergency activity attendances in MIU/Other by Sex",
       subtitle = "Note: MIU/Other refer to minor injuries units (MIU), small hospitals and health centres",
       x = "Date",
       y = "Number of Attendances")
dev.off()

#WHEN PEOPLE ATTEND - DAY OF WEEK
#Importing data of when people attend by day of week
when_dayofweek <- read_excel("Rawdata/2023-09-05-whenpeopleattend-dayofweek.xlsx", 
                             sheet = "NHS Scotland")
view(when_dayofweek)
str(when_dayofweek)

#Bar chart of July 2023 average attendances by day of the week
EDJuly_when_dayofweek <- when_dayofweek %>% 
  filter(Type == "ED Only", Month == "2023-07-01") 

png(file = "Output/EDJuly2023_when_dayofweek.png")
EDJuly_when_dayofweek %>% 
ggplot(aes(x=factor(Day, level=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), y=Average)) +
  geom_col(fill = 'blue') +
  labs(title = "Average attendances in Scottish Emergency Departments(ED) in July 2023",
       x = "Day",
       y = "Average Attendances")
dev.off()

#AGEGROUP
#Importing data of attendance by age group
agegroup <- read_excel("Rawdata/2023-09-05-whoattends-agegroup.xlsx", 
                             sheet = "Health Board")
view(agegroup)

#Checking if there are null values for Rate/100,000 for NHS Lothian, returned 67
sum(is.na(EDLothian_agegroup$`Rate/100,000`))

#Filtering for NHS Lothian, ED Only, exclude Age Unknown
EDLothian_agegroup <- agegroup %>% 
  filter(HealthBoard == "NHS Lothian", Type == "ED Only") %>% 
  filter(!Age %in% c("Unknown"))

#Checking if there are null values for Rate/100,000 for NHS Lothian, returned 0 after excluding Age unknown
sum(is.na(EDLothian_agegroup$`Rate/100,000`))

str(EDLothian_agegroup)

#Change Month from a character to a date using Lubridate
EDLothian_agegroup$Month <- ymd(EDLothian_agegroup$Month)
class(EDLothian_agegroup$Month)

view(EDLothian_agegroup)

#Line graph of NHS Lothian ED attendance rate by age group
png(file = "Output/Lothian_ED_attendancerate_agegroup.png")
ggplot(data = EDLothian_agegroup, aes(x= Month, y= `Rate/100,000`, group= Age, color= Age)) +
  geom_line() +
  labs(title = "Attendance rate per 100,000 in NHS Lothian ED by age group",
       x = "Date",
       y = "Attendance rate per 100,000") 
dev.off()

#DEPRIVATION
#Importing deprivation data
deprivation <- read_excel("Rawdata/2023-09-05-whoattends-deprivation.xlsx", 
                       sheet = "Health Board")

view(deprivation)
str(deprivation)

#Checking if there are null values for Rate/100,000 for NHS Fife in 2022, returned 24
sum(is.na(Fife2022_deprivation$`Rate/100,000`))

#Change Month from a character to a date using Lubridate
deprivation$Month <- ymd(deprivation$Month)
class(deprivation$Month)

#Filtering to exclude Deprivation unknown (NA), for 2022, NHS Fife
EDFife2022_deprivation <- deprivation %>% filter(between(Month, as.Date('2022-01-01'), as.Date('2022-12-01'))) %>% 
  filter(HealthBoard == "NHS Fife", Type == "ED Only") %>% 
  filter(!Deprivation %in% c("Unknown"))

view(EDFife2022_deprivation)

#Checking if there are null values for Rate/100,000 for NHS Fife in 2022, returned 0 after removing Deprivation unknown
sum(is.na(EDFife2022_deprivation$`Rate/100,000`))

#Line graph of NHS Fife ED attendance rate by deprivation status in 2022
png(file = "Output/EDFife_2022_attendancerate_deprivation.png")
ggplot(data = EDFife2022_deprivation, aes(x= Month, y= `Rate/100,000`, group= Deprivation, color= Deprivation)) +
  geom_line() +
  labs(title = "NHS Fife ED Attendance rate per 100,000 in 2022 by deprivation status",
       x = "Date",
       y = "Attendance rate per 100,000") 
dev.off()

#WAITING TIMES
ae_4HwaitingbyHB <- ae_monthly_attendance %>% 
  group_by(NHSBoardName, MonthEndingDate) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll),
            NumberWithin4HoursAll=sum(NumberWithin4HoursAll), 
            PercentageWithin4HoursAll=mean(PercentageWithin4HoursAll))

png(file = "Output/4HwaitingbyHB.png")
ggplot(data=ae_4HwaitingbyHB, aes(x=MonthEndingDate, y=PercentageWithin4HoursAll, group=NHSBoardName, color=NHSBoardName)) +
  geom_line() +
  geom_hline(yintercept=95, color="orange", size=.5) +
  annotate(geom="text", x=as.Date("2008-04-30"), y=94.5, size=3,
           label="95% target") +
  labs(title = "Percentage within 4 hours by Scottish Health Boards",
       x = "Date",
       y = "Percentage within 4 hours")
dev.off()  
