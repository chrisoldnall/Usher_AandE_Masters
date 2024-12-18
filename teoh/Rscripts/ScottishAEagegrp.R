#Scottish A&E data - WHEN PEOPLE ATTEND - age group

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

#AGEGROUP
#Importing data of attendance by age group
agegroupHB <- read_excel("Rawdata/2023-09-05-whoattends-agegroup.xlsx", 
                         sheet = "Health Board")
view(agegroupHB)

#Checking if there are null values for Rate/100,000 for NHS Lothian, returned 67
sum(is.na(EDLothian_agegroup$`Rate/100,000`))

#AGEGROUP NHS Lothian
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
summary(agegroupScot)

#Checking if there are null values for Rate/100,000 for agegroupScotED, returned 67
#sum(is.na(agegroupScotED$`Rate/100,000`))

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

# Set the factor levels for AgeGroup in the desired order
agegroupScotED <- agegroupScotED %>%
  mutate(Age = factor(Age, levels = c('Under 18', '18-24', '25-39', '40-64', '65-74', '75 plus')))

#Line graph of NHS Scotland ED attendance rate by age group

Scot_ED_attendancerate_agegroup <- ggplot(data = agegroupScotED, aes(x= Month, y= `Rate/100,000`, group= Age, color= Age)) +
  geom_line() +
  labs(
    #title = "Attendance rate per 100,000 in Scottish emergency departments \nby age group",
    x = "Year",
    y = "Attendance rate per 100,000")
save_plot("Output/Scot_ED_attendancerate_agegroup.svg", fig = Scot_ED_attendancerate_agegroup, width = 14, height = 12)

Scot_ED_attendancerate_agegroup_legendbottom <- ggplot(data = agegroupScotED, aes(x= Month, y= `Rate/100,000`, group= Age, color= Age)) +
  geom_line() +
  theme(axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
      legend.position = "bottom", legend.text=element_text(size=10))+
  labs(
    #title = "Attendance rate per 100,000 in Scottish emergency departments \nby age group",
    x = "Year",
    y = "Attendance rate per 100,000 population")
save_plot("Output/Scot_ED_attendancerate_agegroup_legendbottom.svg", fig = Scot_ED_attendancerate_agegroup_legendbottom, width = 12, height = 14)



#Creating a graph for MIU/Other by age group
#Filtering for MIU/Other Only, exclude Age Unknown
agegroupScotMIUOther <- agegroupScot %>% 
  filter(Type == "MIU/Other Only") %>% 
  filter(!Age %in% c("Unknown"))

#Checking if there are null values for Rate/100,000 for agegroupScotMIUOther, returned 0 after excluding Age unknown
sum(is.na(agegroupScotMIUOther$`Rate/100,000`))

str(agegroupScotMIUOther)

#Change Month from a character to a date using Lubridate
agegroupScotMIUOther$Month <- ymd(agegroupScotMIUOther$Month)
class(agegroupScotMIUOther$Month)

# Set the factor levels for AgeGroup in the desired order
agegroupScotMIUOther <- agegroupScotMIUOther %>%
  mutate(Age = factor(Age, levels = c('Under 18', '18-24', '25-39', '40-64', '65-74', '75 plus')))

#Line graph of NHS Scotland MIU/Other attendance rate by age group

Scot_MIUOther_attendancerate_agegroup <- ggplot(data = agegroupScotMIUOther, aes(x= Month, y= `Rate/100,000`, group= Age, color= Age)) +
  geom_line() +
  labs(
    #title = "Attendance rate per 100,000 in Scottish MIU/Other by age group",
    x = "Year",
    y = "Attendance rate per 100,000") 
save_plot("Output/Scot_MIUOther_attendancerate_agegroup.svg", fig = Scot_MIUOther_attendancerate_agegroup, width = 14, height = 12)


Scot_MIUOther_attendancerate_agegroup_legendbottom <- ggplot(data = agegroupScotMIUOther, aes(x= Month, y= `Rate/100,000`, group= Age, color= Age)) +
  geom_line() +
  theme(axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        legend.position = "bottom", legend.text=element_text(size=10))+
  labs(
    #title = "Attendance rate per 100,000 in Scottish MIU/Other by age group",
    x = "Year",
    y = "Attendance rate per 100,000 population") 
save_plot("Output/Scot_MIUOther_attendancerate_agegroup_legendbottom.svg", fig = Scot_MIUOther_attendancerate_agegroup_legendbottom, width = 12, height = 14)



#Creating a graph for all AE by age group
#Filter to exclude Age Unknown
agegroupScotAE <- agegroupScot %>% 
  select(Month, Type, Age, Attendances, `Rate/100,000`) %>% 
  filter(!Age %in% c("Unknown")) %>% 
  group_by(Month, Age) %>% 
  summarise(`Rate/100,000`=sum(`Rate/100,000`),
            Attendances=sum(Attendances))

#Checking if there are null values for Rate/100,000 for agegroupScotAE, returned 0 after excluding Age unknown
sum(is.na(agegroupScotAE$`Rate/100,000`))

str(agegroupScotAE)

#Change Month from a character to a date using Lubridate
agegroupScotAE$Month <- ymd(agegroupScotAE$Month)
class(agegroupScotAE$Month)

# Set the factor levels for AgeGroup in the desired order
agegroupScotAE <- agegroupScotAE %>%
  mutate(Age = factor(Age, levels = c('Under 18', '18-24', '25-39', '40-64', '65-74', '75 plus')))

#Line graph of NHS Scotland AE attendance rate by age group

Scot_AE_attendancerate_agegroup <- ggplot(data = agegroupScotAE, aes(x= Month, y= `Rate/100,000`, group= Age, color= Age)) +
  geom_line() +
  labs(
    #title = "Attendance rate per 100,000 in Scottish A&E by age group",
    x = "Year",
    y = "Attendance rate per 100,000") 
save_plot("Output/Scot_AE_attendancerate_agegroup.svg", fig = Scot_AE_attendancerate_agegroup, width = 14, height = 12)


Scot_AE_attendancerate_agegroup_legendbottom <- ggplot(data = agegroupScotAE, aes(x= Month, y= `Rate/100,000`, group= Age, color= Age)) +
  geom_line() +
  theme(axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        legend.position = "bottom", legend.text=element_text(size=10))+
  labs(
    #title = "Attendance rate per 100,000 in Scottish A&E by age group",
    x = "Year",
    y = "Attendance rate per 100,000 population") 
save_plot("Output/Scot_AE_attendancerate_agegroup_legendbottom.svg", fig = Scot_AE_attendancerate_agegroup_legendbottom, width = 12, height = 14)
