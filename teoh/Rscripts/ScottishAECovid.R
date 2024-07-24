#Scottish A&E data - COVID-19

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


ae_monthly_attendance <- read_csv(here("Rawdata", "2023-09-05-ae-monthly-attendance-and-waiting-times-data.csv"))

str(ae_monthly_attendance)

ae_monthly_totalmid2019to2023 <-ae_monthly_attendance %>% 
  filter(between(MonthEndingDate, as.Date('2019-07-01'), as.Date('2023-08-01'))) %>% 
  group_by(MonthEndingDate) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll))

#graph from July 2019 till July 2023 with vertical lines showing lockdown periods from Sept 2020
aeScotlandmid2019to2023 <- ggplot(data=ae_monthly_totalmid2019to2023, aes(x=MonthEndingDate, y=NumberOfAttendancesAll))+
  geom_point()+
  geom_line()+
  geom_vline(xintercept = as.Date("2020-09-23"), color="orange", linewidth=.5)+
  annotate(geom="text", x=as.Date("2020-04-01"), color="orange", y=70000, size=3, label="23/9/20 Nationwide restrictions")+
  geom_vline(xintercept = as.Date("2021-01-05"), color="brown", linewidth=.5)+
  annotate(geom="text", x=as.Date("2020-07-01"), color="brown", y=80000, size=3, label="5/1/21 Mainland Scotland lockdown")+
  geom_vline(xintercept = as.Date("2021-04-02"), color="blue", linewidth=.5)+
  annotate(geom="text", x=as.Date("2020-12-01"), color="blue", y=90000, size=3, label="2/4/21 Stay home>local")+
  geom_vline(xintercept = as.Date("2021-04-23"), color="magenta4", linewidth=.5)+
  annotate(geom="text", x=as.Date("2021-02-01"), color="magenta4", y=100000, size=3, label="23/4/21 Level 3")+
  geom_vline(xintercept = as.Date("2021-05-17"), color="purple4", linewidth=.5)+
  annotate(geom="text", x=as.Date("2020-10-01"), color="purple4", y=110000, size=3, label="17/5/21 Most of mainland Scotland Level 2")+
  geom_vline(xintercept = as.Date("2021-06-05"), color="forestgreen", linewidth=.5)+
  annotate(geom="text", x=as.Date("2020-11-01"), color="forestgreen", y=120000, size=3, label="5/6/21 Most mainland Scotland Level 1")+
  geom_vline(xintercept = as.Date("2021-07-19"), color="cornflowerblue", linewidth=.5)+
  annotate(geom="text", x=as.Date("2021-05-01"), color="cornflowerblue", y=130000, size=3, label="19/7/21 Level 0")+
  labs(x= "Year", 
       y = "Total number of Scottish A&E attendance")
save_plot("Output/aeScotlandmid2019to2023.svg", fig=aeScotlandmid2019to2023, width=14, height=12)

ae_monthly_totalJan2019toDec2022 <-ae_monthly_attendance %>% 
  filter(between(MonthEndingDate, as.Date('2019-01-01'), as.Date('2022-12-31'))) %>% 
  group_by(MonthEndingDate) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll))

ae_monthly_totalJan2019toDec2022$Year <- as.factor(format(ae_monthly_totalJan2019toDec2022$MonthEndingDate, "%Y"))
ae_monthly_totalJan2019toDec2022$Month <- as.factor(format(ae_monthly_totalJan2019toDec2022$MonthEndingDate, "%m"))


aeScotlandJan2019toDec2022<- ggplot(data=ae_monthly_totalJan2019toDec2022, aes(x=Month, y=NumberOfAttendancesAll, group = Year, color = Year))+
  geom_point()+
  geom_line()+
#  geom_vline(xintercept = as.Date("2020-03-23"), color="orange", linewidth=.5)+
#  annotate(geom="text", x=as.Date("2020-02-01"), color="orange", y=70000, size=3, label="23/3/20 UK Lockdown")+
#  geom_vline(xintercept = as.Date("2020-05-29"), color="brown", linewidth=.5)+
#  annotate(geom="text", x=as.Date("2020-04-01"), color="brown", y=75000, size=3, label="29/5/20 Phase 1")+
#  geom_vline(xintercept = as.Date("2020-06-19"), color="blue", linewidth=.5)+
#  annotate(geom="text", x=as.Date("2020-05-01"), color="blue", y=80000, size=3, label="19/6/20 Phase 2")+
#  geom_vline(xintercept = as.Date("2020-07-10"), color="magenta4", linewidth=.5)+
#  annotate(geom="text", x=as.Date("2020-06-01"), color="magenta4", y=85000, size=3, label="10/7/20 Phase 3")+
#  geom_vline(xintercept = as.Date("2020-09-23"), color="purple4", linewidth=.5)+
#  annotate(geom="text", x=as.Date("2020-08-01"), color="purple4", y=90000, size=3, label="23/9/20 Scottish restrictions")+
#  geom_vline(xintercept = as.Date("2021-01-05"), color="forestgreen", linewidth=.5)+
#  annotate(geom="text", x=as.Date("2020-12-01"), color="forestgreen", y=95000, size=3, label="5/1/21 Scottish mainland lockdown")+
#  geom_vline(xintercept = as.Date("2021-04-02"), color="cornflowerblue", linewidth=.5)+
#  annotate(geom="text", x=as.Date("2021-03-01"), color="cornflowerblue", y=100000, size=3, label="2/4/21 Stay home>local")+
#  geom_vline(xintercept = as.Date("2021-04-26"), color="tomato1", linewidth=.5)+
#  annotate(geom="text", x=as.Date("2021-03-01"), color="tomato1", y=105000, size=3, label="26/4/21 Level 3")+
#  geom_vline(xintercept = as.Date("2021-05-17"), color="royalblue4", linewidth=.5)+
#  annotate(geom="text", x=as.Date("2021-04-01"), color="royalblue4", y=110000, size=3, label="17/5/21 Level 2")+
#  geom_vline(xintercept = as.Date("2021-06-05"), color="firebrick2", linewidth=.5)+
#  annotate(geom="text", x=as.Date("2021-05-01"), color="firebrick2", y=115000, size=3, label="17/5/21 Most mainland level 1")+
#  geom_vline(xintercept = as.Date("2021-07-19"), color="sienna", linewidth=.5)+
#  annotate(geom="text", x=as.Date("2021-06-01"), color="sienna", y=120000, size=3, label="19/7/21 Level 0")+
#  geom_vline(xintercept = as.Date("2021-08-09"), color="springgreen4", linewidth=.5)+
#  annotate(geom="text", x=as.Date("2021-07-01"), color="springgreen4", y=125000, size=3, label="9/8/21 Beyond level 0")+
  labs(x= "Month", 
       y = "Total number of Scottish A&E attendance")
save_plot("Output/aeScotlandJan2019toDec2022.svg", fig=aeScotlandJan2019toDec2022, width=14, height=12)




ggplot(data=ae_monthly_totalJan2019toDec2022, aes(x=MonthEndingDate, y=NumberOfAttendancesAll))+
  geom_point()+
  geom_line()+
  facet_wrap(~Year)+
  labs(x= "Month", 
       y = "Total number of Scottish A&E attendance")

#2019
aeScotland2019 <-ae_monthly_totalJan2019toDec2022 %>%
  filter(between(MonthEndingDate, as.Date("2019-01-01"), as.Date("2019-12-31"))) %>% 
  ggplot(aes(x=MonthEndingDate, y=NumberOfAttendancesAll))+
  geom_point()+
  geom_line()+
  labs(x= "Month", 
       y = "Total number of Scottish A&E attendance")
save_plot("Output/aeScotland2019.svg", fig=aeScotland2019, width=14, height=12)

#2020 with lockdown dates
aeScotland2020<- ae_monthly_totalJan2019toDec2022 %>%
  filter(between(MonthEndingDate, as.Date("2020-01-01"), as.Date("2020-12-31"))) %>% 
  ggplot(aes(x=MonthEndingDate, y=NumberOfAttendancesAll))+
  geom_point()+
  geom_line()+
  geom_vline(xintercept = as.Date("2020-03-23"), color="orange", linewidth=.5)+
  annotate(geom="text", x=as.Date("2020-03-01"), color="orange", y=70000, size=3, label="23/3/20 UK Lockdown")+
  geom_vline(xintercept = as.Date("2020-05-29"), color="brown", linewidth=.5)+
  annotate(geom="text", x=as.Date("2020-05-01"), color="brown", y=75000, size=3, label="29/5/20 Phase 1")+
  geom_vline(xintercept = as.Date("2020-06-19"), color="blue", linewidth=.5)+
  annotate(geom="text", x=as.Date("2020-06-01"), color="blue", y=80000, size=3, label="19/6/20 Phase 2")+
  geom_vline(xintercept = as.Date("2020-07-10"), color="magenta4", linewidth=.5)+
  annotate(geom="text", x=as.Date("2020-07-01"), color="magenta4", y=85000, size=3, label="10/7/20 Phase 3")+
  geom_vline(xintercept = as.Date("2020-09-23"), color="purple4", linewidth=.5)+
  annotate(geom="text", x=as.Date("2020-09-01"), color="purple4", y=90000, size=3, label="23/9/20 Scottish restrictions")+
  labs(x= "Month", 
       y = "Total number of Scottish A&E attendance")
save_plot("Output/aeScotland2020.svg", fig=aeScotland2020, width=14, height=12)

#2021 with lockdown dates
aeScotland2021 <-ae_monthly_totalJan2019toDec2022 %>%
  filter(between(MonthEndingDate, as.Date("2021-01-01"), as.Date("2021-12-31"))) %>% 
  ggplot(aes(x=MonthEndingDate, y=NumberOfAttendancesAll))+
  geom_point()+
  geom_line()+
  geom_vline(xintercept = as.Date("2021-01-05"), color="forestgreen", linewidth=.5)+
  annotate(geom="text", x=as.Date("2021-01-01"), color="forestgreen", y=95000, size=3, label="Scottish mainland lockdown 5/1/21 ")+
  geom_vline(xintercept = as.Date("2021-04-02"), color="cornflowerblue", linewidth=.5)+
  annotate(geom="text", x=as.Date("2021-03-01"), color="cornflowerblue", y=100000, size=3, label="2/4/21 Stay home>local")+
  geom_vline(xintercept = as.Date("2021-04-26"), color="tomato1", linewidth=.5)+
  annotate(geom="text", x=as.Date("2021-04-01"), color="tomato1", y=105000, size=3, label="26/4/21 Level 3")+
  geom_vline(xintercept = as.Date("2021-05-17"), color="royalblue4", linewidth=.5)+
  annotate(geom="text", x=as.Date("2021-05-01"), color="royalblue4", y=110000, size=3, label="17/5/21 Level 2")+
  geom_vline(xintercept = as.Date("2021-06-05"), color="firebrick2", linewidth=.5)+
  annotate(geom="text", x=as.Date("2021-06-01"), color="firebrick2", y=115000, size=3, label="17/5/21 Most mainland level 1")+
  geom_vline(xintercept = as.Date("2021-07-19"), color="sienna", linewidth=.5)+
  annotate(geom="text", x=as.Date("2021-07-01"), color="sienna", y=120000, size=3, label="19/7/21 Level 0")+
  geom_vline(xintercept = as.Date("2021-08-09"), color="springgreen4", linewidth=.5)+
  annotate(geom="text", x=as.Date("2021-08-01"), color="springgreen4", y=125000, size=3, label="9/8/21 Beyond level 0")+
  labs(x= "Month", 
       y = "Total number of Scottish A&E attendance")
save_plot("Output/aeScotland2021.svg", fig=aeScotland2021, width=14, height=12)

#2022
aeScotland2022 <-ae_monthly_totalJan2019toDec2022 %>%
  filter(between(MonthEndingDate, as.Date("2022-01-01"), as.Date("2022-12-31"))) %>% 
  ggplot(aes(x=MonthEndingDate, y=NumberOfAttendancesAll))+
  geom_point()+
  geom_line()+
  labs(x= "Month", 
       y = "Total number of Scottish A&E attendance")
save_plot("Output/aeScotland2022.svg", fig=aeScotland2022, width=14, height=12)