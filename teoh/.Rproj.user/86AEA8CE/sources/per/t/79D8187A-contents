#Scottish A&E data - attendances

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

#ATTENDANCESALL
#Loading A&E monthly attendance and waiting times csv file
ae_monthly_attendance <- read_csv(here("Rawdata", "2023-09-05-ae-monthly-attendance-and-waiting-times-data.csv"))

#Eyeballing the dataset
view(ae_monthly_attendance)
head(ae_monthly_attendance)
str(ae_monthly_attendance)
summary(ae_monthly_attendance)
#mean waiting time = 96.732% 28/2/2015 NHS Western Isle

#Count number of ED and MIU/Other sites
ae_monthly_attendance %>% filter(DepartmentType=="ED") %>% count(LocationName)
ae_monthly_attendance %>% filter(DepartmentType=="MIU/Other") %>% count(LocationName)
#ED = 36, MIU/Other=74


#Entry with the lowest and highest NumberOfAttendancesAll
ae_monthly_attendance[which.min(ae_monthly_attendance$NumberOfAttendancesAll),]
ae_monthly_attendance[which.max(ae_monthly_attendance$NumberOfAttendancesAll),]
#min= 1, 2007-11-30, Moffat Hospital (MIU/Other)
#max=11,579, 2019-08-31, Royal Infirmary of Edinburgh (ED)

#Entry with the lowest and highest PercentageWithin4HoursAll
ae_monthly_attendance[which.min(ae_monthly_attendance$PercentageWithin4HoursAll),]
ae_monthly_attendance[which.max(ae_monthly_attendance$PercentageWithin4HoursAll),]
#min = 40.2%, 2022-10-31, Forth Valley Royal Hospital (ED)
#max=100%, 2007-07-31, Arran War Memorial Hospital (MIU/Other)

#Summary split by ED and MIU/Other
ae_monthly_attendance %>%filter(DepartmentType == 'ED')%>%summary()
#mean 92.072% 31/12/2008 NHS Grampian, 92.074% 31/7/2020 NHS Lanarkshire
ae_monthly_attendance %>%filter(DepartmentType == 'MIU/Other')%>%summary()
#min 49.596 31/12/2022 NHS Lothian
#mean 99.361% 28/2/2017 NHS Highland, 99.3569% 31/8/2016 NHS Ayshire, multiple 

ae_monthly_attendance %>%
  summarise(mean=mean(NumberOfAttendancesAll), 
            median = median(NumberOfAttendancesAll),
            sd=sd(NumberOfAttendancesAll),
            min=min(NumberOfAttendancesAll),
            max=max(NumberOfAttendancesAll), 
            var=var(NumberOfAttendancesAll))

ae_monthly_attendance %>%filter(DepartmentType == 'ED')%>%
  summarise(mean=mean(NumberOfAttendancesAll), 
            median = median(NumberOfAttendancesAll),
            sd=sd(NumberOfAttendancesAll),
            min=min(NumberOfAttendancesAll),
            max=max(NumberOfAttendancesAll),
            var=var(NumberOfAttendancesAll))

ae_monthly_attendance %>%filter(DepartmentType == 'MIU/Other')%>%
  summarise(mean=mean(NumberOfAttendancesAll), 
            median = median(NumberOfAttendancesAll),
            sd=sd(NumberOfAttendancesAll),
            min=min(NumberOfAttendancesAll),
            max=max(NumberOfAttendancesAll), 
            var=var(NumberOfAttendancesAll))



#Summary for the last 5 years (period between 2018-07-31 to 2023-07-31)
ae_monthly_attendance %>%filter(between(MonthEndingDate, as.Date('2018-07-31'), as.Date('2023-07-31')))%>%summary()

#Summary for the last 5 years (period between 2018-07-31 to 2023-07-31) split by ED and MIU/Other
ae_monthly_attendance %>%filter(between(MonthEndingDate, as.Date('2018-07-31'), as.Date('2023-07-31')), DepartmentType == 'ED')%>%summary()
ae_monthly_attendance %>%filter(between(MonthEndingDate, as.Date('2018-07-31'), as.Date('2023-07-31')), DepartmentType == 'MIU/Other')%>%summary()

#Histogram for distribution of number of attendances

png(file = "Output/HistogramAEattendancesbyDeptType.png")
ae_monthly_attendance %>%
  ggplot(aes(x=NumberOfAttendancesAll))+
  geom_histogram() +
  facet_wrap(~DepartmentType) +
  labs(Title="Number of attendances by department type", 
       x="Number of attendances",
       y="Count")
dev.off()

#saving HistogramAEattendancesbyDeptType as svg
HistogramAEattendancesbyDeptType <-ggplot(ae_monthly_attendance ,aes(x=NumberOfAttendancesAll))+
  geom_histogram() +
  facet_wrap(~DepartmentType) +
  labs(Title="Number of attendances by department type", 
       x="Number of attendances",
       y="Number of days")
save_plot("Output/HistogramAEattendancesbyDeptType.svg", fig = HistogramAEattendancesbyDeptType, width=12, height=8)

#saving HistogramAEattendancesbyDeptType as svg with vlines

Attendances_stats <- ae_monthly_attendance %>% 
  group_by(DepartmentType) %>% 
  summarise(mean = mean(NumberOfAttendancesAll),
            median = median(NumberOfAttendancesAll))

HistogramAEattendancesbyDeptType_vline <-ggplot(ae_monthly_attendance ,aes(x=NumberOfAttendancesAll))+
  geom_histogram() +
  geom_vline(xintercept=mean(Attendances_stats$mean), color="blue", size=0.5)+
  geom_vline(xintercept=median(Attendances_stats$median), color="orange", size=0.5)+
  facet_wrap(~DepartmentType) +
  labs(Title="Number of attendances by department type", 
       x="Number of attendances",
       y="Count")
save_plot("Output/HistogramAEattendancesbyDeptType_vline.svg", fig = HistogramAEattendancesbyDeptType_vline, width=12, height=8)

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


#Quantitative analysis for AE (ED+MIU/Other) by month and HB
summary(ae_byboard)
#Data from 2007-07-31 to 2023-07031
#NumberOfAttendancesAll: min = 142, max = 43022
unique(ae_byboard$NHSBoardName)

png(file = "Output/AttendancebyHB.png")
ggplot(data=ae_byboard, aes(x=MonthEndingDate, y=NumberOfAttendancesAll, group=NHSBoardName)) +
  geom_line() +
  labs(title = "Number of attendances by Scottish Health Boards",
       x = "Date",
       y = "Number of attendances") 
dev.off()  

#Creating a new column of Year
ae_byboard$Year<- as.numeric(format(ae_byboard$MonthEndingDate, "%Y"))
ae_byboard


#Creating a line plot for all A&E attendances

ae_monthly_total <- ae_byboard %>% 
  group_by(MonthEndingDate) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll))

AttendanceTotal <- ggplot(data=ae_monthly_total, aes(x=MonthEndingDate, y=NumberOfAttendancesAll)) +
  geom_line() +
  #scale_x_continuous(breaks=seq(2007,2024,2))+
  scale_y_continuous(labels=scales::comma)+
  labs(
    #title = "Total number of Scottish accident and emergency (A&E) attendances",
    x = "Year",
    y = "Total number of A&E attendances")
save_plot("Output/AttendanceTotal.svg", fig = AttendanceTotal, width = 14, height = 12) 

#Creating line plot for Grampian, Lothian and Western Isle
Grampian_Lothian_WesternIsle <- ae_monthly_attendance %>% 
  filter(NHSBoardName == "NHS Lothian"|NHSBoardName == "NHS Grampian"|NHSBoardName == "NHS Western Isles") %>% 
  select (MonthEndingDate, NHSBoardName, NumberOfAttendancesAll) %>% 
  group_by(NHSBoardName, MonthEndingDate) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll))

Grampian_Lothian_WesternIsleAE <- ggplot(data=Grampian_Lothian_WesternIsle, aes(x=MonthEndingDate, y=NumberOfAttendancesAll, ggroup=NHSBoardName, color=NHSBoardName)) +
  geom_line() +
  #scale_x_continuous(breaks=seq(2007,2024,2))+
  scale_y_continuous(labels=scales::comma)+
  labs(
    #title = "Total number of accident and emergency (A&E) attendances at \nNHS Grampian, NHS Lothian and NHS Western Isles",
    x = "Year",
    y = "Total number of A&E attendances",
    col="NHS Health Board")
save_plot("Output/Grampian_Lothian_WesternIsleAE.svg", fig = Grampian_Lothian_WesternIsle, width = 14, height = 12)



