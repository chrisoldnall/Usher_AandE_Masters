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


#Creating a new column of Year
ae_byboard$Year<- as.numeric(format(ae_byboard$MonthEndingDate, "%Y"))
ae_byboard

png(file = "Output/AttendancebyHBper1000pop.png")
ggplot(data=ae_byboard_population, aes(x=MonthEndingDate, y=attendanceper1000pop, group=NHSBoardName, color=NHSBoardName)) +
  geom_line() +
  labs(title = "Accident and emergency attendance rate per 1000 population by Scottish Health Boards",
       x = "Date",
       y = "Attendance rate per 1000 population")
dev.off()  

#Creating a line plot for all A&E attendances
##FIGURE 1

ae_monthly_total <- ae_byboard %>% 
  group_by(MonthEndingDate) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll))

png(file = "Output/AttendanceTotal.png")
ggplot(data=ae_monthly_total, aes(x=MonthEndingDate, y=NumberOfAttendancesAll)) +
  geom_line() +
  #scale_x_continuous(breaks=seq(2007,2024,2))+
  scale_y_continuous(labels=scales::comma)+
  labs(title = "Total number of Scottish accident and emergency (A&E) attendances",
       x = "Date",
       y = "Total number of Scottish A&E attendances")
dev.off() 

#Creating line plot for Grampian, Lothian and Western Isle
Grampian_Lothian_WesternIsle <- ae_monthly_attendance %>% 
  filter(NHSBoardName == "NHS Lothian"|NHSBoardName == "NHS Grampian"|NHSBoardName == "NHS Western Isles") %>% 
  select (MonthEndingDate, NHSBoardName, NumberOfAttendancesAll) %>% 
  group_by(NHSBoardName, MonthEndingDate) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll))

png(file = "Output/Grampian_Lothian_WesternIsle.png")
ggplot(data=Grampian_Lothian_WesternIsle, aes(x=MonthEndingDate, y=NumberOfAttendancesAll, ggroup=NHSBoardName, color=NHSBoardName)) +
  geom_line() +
  #scale_x_continuous(breaks=seq(2007,2024,2))+
  scale_y_continuous(labels=scales::comma)+
  labs(title = "Total number of accident and emergency attendances at NHS Grampian,\nNHS Lothian and NHS Western Isles",
       x = "Year",
       y = "Total number of A&E attendances",
       col="NHS Board")
dev.off() 

#Creating a new column of Year
Grampian_Lothian_WesternIsle$Year<- as.numeric(format(Grampian_Lothian_WesternIsle$MonthEndingDate, "%Y"))
Grampian_Lothian_WesternIsle

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

#AGEGROUP NHS Lothian
#Importing data of attendance by age group
agegroupHB <- read_excel("Rawdata/2023-09-05-whoattends-agegroup.xlsx", 
                         sheet = "Health Board")
view(agegroupHB)

#Checking if there are null values for Rate/100,000 for NHS Lothian, returned 67
sum(is.na(EDLothian_agegroup$`Rate/100,000`))

#Filtering for NHS Lothian, ED Only, exclude Age Unknown
EDLothian_agegroup <- agegroupHB %>% 
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

#AGE GROUP NHS Scotland

agegroupScot <- read_excel("Rawdata/2023-09-05-whoattends-agegroup.xlsx",
                           sheet ="NHS Scotland")

#Checking if there are null values for Rate/100,000 for agegroupScotED, returned 67
sum(is.na(agegroupScotED$`Rate/100,000`))

#Filtering for ED Only, exclude Age Unknown
agegroupScotED <- agegroupScot %>% 
  filter(Type == "ED Only") %>% 
  filter(!Age %in% c("Unknown"))

#Checking if there are null values for Rate/100,000 for agegroupScotED, returned 0 after excluding Age unknown
sum(is.na(agegroupScotED$`Rate/100,000`))

str(agegroupScotED)

#Change Month from a character to a date using Lubridate
agegroupScotED$Month <- ymd(agegroupScotED$Month)
class(agegroupScotED$Month)

view(agegroupScotED)

#Line graph of NHS Scotland ED attendance rate by age group
png(file = "Output/Scot_ED_attendancerate_agegroup.png")
ggplot(data = agegroupScotED, aes(x= Month, y= `Rate/100,000`, group= Age, color= Age)) +
  geom_line() +
  labs(title = "Attendance rate per 100,000 in NHS Scotland ED by age group",
       x = "Date",
       y = "Attendance rate per 100,000") 
dev.off()

#DEPRIVATION
#Importing deprivation data
deprivation <- read_excel("Rawdata/2023-09-05-whoattends-deprivation.xlsx", 
                       sheet = "Health Board")

deprivationScot <-read_excel("Rawdata/2023-09-05-whoattends-deprivation.xlsx", 
                             sheet = "NHS Scotland")


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

png(file = "Output/4HwaitingbyHB_nocolour.png")
ggplot(data=ae_4HwaitingbyHB, aes(x=MonthEndingDate, y=PercentageWithin4HoursAll, group=NHSBoardName)) +
  geom_line() +
  geom_hline(yintercept= c(80,95), color="orange", size=.5) +
  annotate(geom="text", x=as.Date("2008-04-30"), y=94.5, size=3,
           label="95% target") +
  annotate(geom="text", x=as.Date("2008-04-30"), y=79.5, size=3,
           label="80% target") +
  labs(title = "Percentage A&E attendances seen within 4 hours \nby Scottish Health Boards",
       x = "Year",
       y = "Percentage A&E attendances seen within 4 hours")
dev.off() 

png(file = "Output/4HwaitingbyHB_nocolour.png")
ggplot(data=ae_4HwaitingbyHB, aes(x=MonthEndingDate, y=PercentageWithin4HoursAll, group=NHSBoardName)) +
  geom_line() +
  geom_hline(yintercept=95, color="orange", size=.5) +
  annotate(geom="text", x=as.Date("2008-04-30"), y=94.5, size=3,
           label="95% target") +
  labs(title = "Percentage A&E attendances seen within 4 hours \nby Scottish Health Boards",
       x = "Year",
       y = "Percentage A&E attendances seen within 4 hours")
dev.off() 

#Comparing 4H waiting times by HB in 2022

ae_4HwaitingbyHB$Year<-as.numeric(format(ae_4HwaitingbyHB$MonthEndingDate, "%Y"))
ae_4HwaitingbyHB_2022<-ae_4HwaitingbyHB %>% 
  filter(Year=="2022") 

ae_4HwaitingbyHB_2022<- ae_4HwaitingbyHB_2022 %>% 
  select(NHSBoardName, NumberOfAttendancesAll, NumberWithin4HoursAll) %>% 
  group_by(NHSBoardName)%>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll),
            NumberWithin4HoursAll=sum(NumberWithin4HoursAll))

#counting the percentages for within 4 hours
ae_4HwaitingbyHB_2022$percent4Hr<-ae_4HwaitingbyHB_2022$NumberWithin4HoursAll/ae_4HwaitingbyHB_2022$NumberOfAttendancesAll
ae_4HwaitingbyHB_2022$percent4Hr<-ae_4HwaitingbyHB_2022$percent4Hr*100

#rounding up percent4Hr to 1 decimal point
ae_4HwaitingbyHB_2022$percent4Hr<-round(ae_4HwaitingbyHB_2022$percent4Hr,digits = 1)

#Side ways bar chart of waiting times within 4 hours by HB
png(file = "Output/percent4HwaitingbyHB.png")
ae_4HwaitingbyHB_2022 %>% 
  mutate(NHSBoardName = fct_reorder(NHSBoardName, percent4Hr)) %>% #to reorder by value
  ggplot(aes(x=NHSBoardName, y=percent4Hr)) +
  geom_col()+
  geom_text(aes(label=percent4Hr), hjust = -0.2, size=2.5)+
  coord_flip()+
  labs(title = "Percentage A&E attendances seen within 4 hours by \nScottish Health Boards in 2022",
       x = "Percentage A&E attendances seen within 4 hours",
       y = "NHS Board")
dev.off()

#ATTENDANCES HEAT MAP by month and year

#Heat map for all attendances
attendance_monthenddate <- ae_monthly_attendance %>% 
  group_by(MonthEndingDate) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll))

#Heat map for ED only
attendance_monthenddateED <- ae_monthly_attendance %>% 
  filter(DepartmentType == "ED") %>% 
  group_by(MonthEndingDate) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll))

#Heat map for MIU/Other only
attendance_monthenddateMIUOther <- ae_monthly_attendance %>% 
  filter(DepartmentType == "MIU/Other") %>% 
  group_by(MonthEndingDate) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll))

#Creating a new column for date and month data
attendance_monthenddate$month <- format(as.Date(attendance_monthenddate$MonthEndingDate, format="%Y/%m/%d"),"%m")
#df$month <- format(as.Date(df$date, format="%d/%m/%Y"),"%m")
attendance_monthenddateED$month <- format(as.Date(attendance_monthenddateED$MonthEndingDate, format="%Y/%m/%d"),"%m")
attendance_monthenddateMIUOther$month <- format(as.Date(attendance_monthenddateMIUOther$MonthEndingDate, format="%Y/%m/%d"),"%m")

attendance_monthenddate$year <- format(as.Date(attendance_monthenddate$MonthEndingDate, format="%Y/%m/%d"),"%Y")
attendance_monthenddateED$year <- format(as.Date(attendance_monthenddateED$MonthEndingDate, format="%Y/%m/%d"),"%Y")
attendance_monthenddateMIUOther$year <- format(as.Date(attendance_monthenddateMIUOther$MonthEndingDate, format="%Y/%m/%d"),"%Y")

#Creating and saving the heat maps
#All NHS Scotland
png(file = "Output/attendancemonthyearheatmap.png")
ggplot(attendance_monthenddate, aes(x = year, y = month, fill = NumberOfAttendancesAll)) +
  geom_tile() +
  scale_fill_distiller(palette = "Blues", direction = +1) +
  labs(title = "Heatmap of NHS Scotland attendances by month and year",
       x = "Year",
       y = "Month")
dev.off()

#ED
ggplot(attendance_monthenddateED, aes(x = year, y = month, fill = NumberOfAttendancesAll)) +
  geom_tile() +
  scale_fill_distiller(palette = "Blues", direction = +1) +
  labs(title = "Attendances ED Heatmap",
       x = "Year",
       y = "Month")

#MIU/Other
ggplot(attendance_monthenddateMIUOther, aes(x = year, y = month, fill = NumberOfAttendancesAll)) +
  geom_tile() +
  scale_fill_distiller(palette = "Blues", direction = +1) +
  labs(title = "Attendances MIU/Other Heatmap",
       x = "Year",
       y = "Month")

#Heat map attendance by time of the day - weekend and weekdays
#Importing when people attend arrival hour
arrivalhrScot <- read_excel("Rawdata/2023-09-05-whenpeopleattend-arrivalhour.xlsx", 
                            sheet = "NHS Scotland")

#Change Month from a character to a date using Lubridate
arrivalhrScot$Month <- ymd(arrivalhrScot$Month)
class(arrivalhrScot$Month)

#Heat map for weekday ED only attendances by arrival hour
arrivalhrScotEDwkdy <- arrivalhrScot %>% 
  filter(Type == "ED Only", Week == "Weekday") 
#group_by(MonthEndingDate) %>% 
#summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll))

png(file = "Output/arrivalhrScotEDwkdyheatmap.png")
ggplot(arrivalhrScotEDwkdy, aes(x = Month, y = Hour, fill = Attendances)) +
  geom_tile() +
  scale_fill_distiller(palette = "Blues", direction = +1) +
  labs(title = "Heatmap of weekday ED attendances by arrival hour",
       x = "Time",
       y = "Arrival hour")
dev.off()

#Heat map for weekday MIU/Other Only attendance by arrival hour
arrivalhrScotMIUOtherwkdy <- arrivalhrScot %>% 
  filter(Type == "MIU/Other Only", Week == "Weekday") 
#group_by(MonthEndingDate) %>% 
#summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll))

png(file = "Output/arrivalhrScotMIUOtherwkdyheatmap.png")
ggplot(arrivalhrScotMIUOtherwkdy, aes(x = Month, y = Hour, fill = Attendances)) +
  geom_tile() +
  scale_fill_distiller(palette = "Blues", direction = +1) +
  labs(title = "Heatmap of weekday MIU/Other attendances by arrival hour",
       x = "Time",
       y = "Arrival hour")
dev.off()

#Heat map for weekend ED only attendances by arrival hour
arrivalhrScotEDwknd <- arrivalhrScot %>% 
  filter(Type == "ED Only", Week == "Weekend") 
#group_by(MonthEndingDate) %>% 
#summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll))

png(file = "Output/arrivalhrScotEDwkndheatmap.png")
ggplot(arrivalhrScotEDwknd, aes(x = Month, y = Hour, fill = Attendances)) +
  geom_tile() +
  scale_fill_distiller(palette = "Blues", direction = +1) +
  labs(title = "Heatmap of weekend ED attendances by arrival hour",
       x = "Time",
       y = "Arrival hour")
dev.off()

#Heat map for weekend MIU/Other Only attendance by arrival hour
arrivalhrScotMIUOtherwknd <- arrivalhrScot %>% 
  filter(Type == "MIU/Other Only", Week == "Weekend") 
#group_by(MonthEndingDate) %>% 
#summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll))

png(file = "Output/arrivalhrScotMIUOtherwkndheatmap.png")
ggplot(arrivalhrScotMIUOtherwknd, aes(x = Month, y = Hour, fill = Attendances)) +
  geom_tile() +
  scale_fill_distiller(palette = "Blues", direction = +1) +
  labs(title = "Heatmap of weekend MIU/Other attendances by arrival hour",
       x = "Time",
       y = "Arrival hour")
dev.off()


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

#Combining Grampian,Lothian and WesternIsle with population estimates
Grampian_Lothian_WesternIsle_population<-merge(Grampian_Lothian_WesternIsle, HBpopulation_estimate_2023_HBnames, by=c("NHSBoardName", "Year"))

#Calculating attendance per population
Grampian_Lothian_WesternIsle_population$attendanceperpop<-Grampian_Lothian_WesternIsle_population$NumberOfAttendancesAll/Grampian_Lothian_WesternIsle_population$PopulationAllAges
#Calculating attendance per 1,000 population
Grampian_Lothian_WesternIsle_population$attendanceper1000pop<-Grampian_Lothian_WesternIsle_population$attendanceperpop*1000

#Combining ae_byboard with population estimates
ae_byboard_population<-merge(ae_byboard, HBpopulation_estimate_2023_HBnames, by=c("NHSBoardName", "Year"))

#Calculating attendance per population
ae_byboard_population$attendanceperpop<-ae_byboard_population$NumberOfAttendancesAll/ae_byboard_population$PopulationAllAges
#Calculating attendance per 1,000 population
ae_byboard_population$attendanceper1000pop<-ae_byboard_population$attendanceperpop*1000



#LOGISTIC REGRESSION

#LM-SEX
#scatter plot of whoattends_sex
ggplot(whoattends_sex, aes(x=Month, y=Attendances, color=Sex))+
  geom_point()

#box plot of whoattends_sex
ggplot(whoattends_sex, aes(x=Sex, y=Attendances))+
  geom_boxplot()

#boxplot of whoattends_sex_ED, excluding unknown
ggplot(whoattends_sex %>% filter(Sex != "Unknown", Type == "ED Only"), aes(x=Sex, y=Attendances))+
  geom_boxplot()

#Linear regression for sex and type

Sex_Type_LM <-whoattends_sex %>% 
  lm(formula = Attendances ~ Sex + Type)
Sex_Type_LM
summary(Sex_Type_LM)

Sex_ED_LM <-whoattends_sex %>% 
  filter(Sex !="Unknown", Type == "ED Only") %>% 
  lm(formula = Attendances ~ Sex)
Sex_ED_LM
summary(Sex_ED_LM)
#when just look at sex, the p-value is 0.1366 
#and the multiple R-squared is 0.01671
#not significant


#LM-AGE
#scatter plot of age
ggplot(agegroupScot, aes(x=Month, y=Attendances, color=Age))+
  geom_point()


Age_ED_LM <-agegroupScot %>% 
  filter(Age !="Unknown", Type == "ED Only") %>% 
  lm(formula = Attendances ~ Age)
Age_ED_LM
summary(Age_ED_LM)


#LM-DEPRIVATION
#box plot of deprivationScot
ggplot(deprivationScot, aes(x=Deprivation, y=Attendances))+
  geom_boxplot()


Deprivation_ED_LM <-deprivationScot %>% 
  filter(Deprivation !="Unknown", Type == "ED Only") %>% 
  lm(formula = Attendances ~ Deprivation)
Deprivation_ED_LM
summary(Deprivation_ED_LM)


#LM-DAYOFWEEK
#box plot of when_dayofweek
ggplot(when_dayofweek, aes(x=Day, y=Average))+
  geom_boxplot()


Dayofweek_ED_LM <-when_dayofweek %>% 
  filter(Type == "ED Only") %>% 
  lm(formula = Average ~ Day)
Dayofweek_ED_LM
summary(Dayofweek_ED_LM)

#LM-MONTH
#box plot of attendance_monthenddate
ggplot(attendance_monthenddate, aes(x=month, y=NumberOfAttendancesAll))+
  geom_boxplot()

#box plot of attendance_monthenddateED
ggplot(attendance_monthenddateED, aes(x=month, y=NumberOfAttendancesAll))+
  geom_boxplot()

Month_ED_LM <-attendance_monthenddateED%>% 
  lm(formula = NumberOfAttendancesAll ~ month)
Month_ED_LM
summary(Month_ED_LM)


#LM-HEALTHBOARD
#box plot of ae_byboard
ggplot(ae_byboard, aes(x=NHSBoardName, y=NumberOfAttendancesAll))+
  geom_boxplot()

#box plot of ae_monthly_attendance - filter by ED
ggplot(ae_monthly_attendance %>% filter(DepartmentType == "ED"), aes(x=NHSBoardName, y=NumberOfAttendancesAll))+
  geom_boxplot()

HB_ED_LM <-ae_monthly_attendance %>% 
  filter(DepartmentType == "ED") %>% 
  lm(formula = NumberOfAttendancesAll ~ NHSBoardName)
HB_ED_LM
summary(HB_ED_LM)

