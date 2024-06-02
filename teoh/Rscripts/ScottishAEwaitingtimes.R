#Scottish A&E data - WHEN PEOPLE ATTEND - waiting times

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

#WAITING TIMES
#Comparing 4H waiting times by HB
ae_4HwaitingbyHB <- ae_monthly_attendance %>% 
  group_by(NHSBoardName, MonthEndingDate) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll),
            NumberWithin4HoursAll=sum(NumberWithin4HoursAll), 
            PercentageWithin4HoursAll=mean(PercentageWithin4HoursAll))

#Qualitative statistics for waiting times by month and HB in AE (includes ED and MIU/Other)
summary(ae_4HwaitingbyHB)
#Max percentover4HoursAll 55.1% NHS Lanarkshire. Min percentagewithin4HoursAll 44.9.
#Max NumberOfAllAttendances in a month by a HB 43022, min 142
#Min NumberWithin4HoursAll (by month and HB) 141, max 41655

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

#saving as svg fourHwaitingbyHB_nocolour

fourHwaitingbyHB_nocolour <- ggplot(data=ae_4HwaitingbyHB, aes(x=MonthEndingDate, y=PercentageWithin4HoursAll, group=NHSBoardName)) +
  geom_line() +
  geom_hline(yintercept= c(80,95), color="orange", size=.5) +
  annotate(geom="text", x=as.Date("2008-04-30"), y=94.5, size=3,
           label="95% target") +
  annotate(geom="text", x=as.Date("2008-04-30"), y=79.5, size=3,
           label="80% target") +
  labs(
    #title = "Percentage A&E attendances seen within 4 hours \nby Scottish Health Boards",
    x = "Year",
    y = "Percentage A&E attendances seen within 4 hours")
save_plot("Output/fourHwaitingbyHB_nocolour.svg", fig = fourHwaitingbyHB_nocolour, width = 14, height = 12)

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
ae_4HwaitingbyHB_2022$percent4Hr<-round(ae_4HwaitingbyHB_2022$percent4Hr,digits = 3)

#Side ways bar chart of waiting times within 4 hours by HB in 2022
png(file = "Output/percent4HwaitingbyHB2022.png")
ae_4HwaitingbyHB_2022 %>% 
  mutate(NHSBoardName = fct_reorder(NHSBoardName, percent4Hr)) %>% #to reorder by value
  ggplot(aes(x=NHSBoardName, y=percent4Hr)) +
  geom_col()+
  geom_text(aes(label=percent4Hr), hjust = -0.2, size=2.5)+
  coord_flip()+
  labs(
    #title = "Percentage A&E attendances seen within 4 hours by \nScottish Health Boards in 2022",
    y = "Percentage A&E attendances seen within 4 hours",
    x = "NHS Board")
dev.off()

#Saving as svg side ways bar chart of waiting times within 4 hours by HB in 2022

ae_4HwaitingbyHB_2022reorder <- ae_4HwaitingbyHB_2022 %>% 
  mutate(NHSBoardName = fct_reorder(NHSBoardName, percent4Hr)) #to reorder by value

percent4HwaitingbyHB2022 <- ggplot(ae_4HwaitingbyHB_2022reorder, aes(x=NHSBoardName, y=percent4Hr, label=sprintf("%0.3f", round(percent4Hr, digits = 3)))) +
  geom_col()+
  geom_text(aes(label=percent4Hr), hjust = -0.2, size=2.5)+
  coord_flip()+
  labs(
    #title = "Percentage A&E attendances seen within 4 hours by \nScottish Health Boards in 2022",
    y = "Percentage A&E attendances seen within 4 hours",
    x = "NHS Health Board")
save_plot("Output/percent4HwaitingbyHB2022testlabel.svg", fig = percent4HwaitingbyHB2022, width = 32, height = 14)

#Map to demonstrate 4H waiting by HB
ae_4HwaitingbyHB$PercentOver4HoursAll <- 100 - ae_4HwaitingbyHB$PercentageWithin4HoursAll


