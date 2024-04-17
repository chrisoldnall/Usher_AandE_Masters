#Scottish A&E data - gender

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

#There are NA data for Rate/100,000 in the whoattends_sex data
#sum(is.na(whoattends_sex $`Rate/100,000`))
#134

#Plotting line graph for ED attendance rate by sex
png(file = "Output/EDrate_whoattends_sexgraph.png")
ggplot(data=ED_whoattends_sex %>% filter(!Sex%in% c("Unknown")), 
       aes(x=Month, y= `Rate/100,000`, group=Sex, color=Sex)) +
  geom_line() +
  labs(title = "Rate of attendances per 100,000 in Scottish Emergency \nDepartments (ED) by Sex",
       x = "Year",
       y = "Rate of attendances per 100,000")
dev.off()

#Plotting line graph for MIU/Other attendance rate by sex
png(file = "Output/MIUrate_whoattends_sexgraph.png")
ggplot(data=MIU_whoattends_sex %>% filter(!Sex%in% c("Unknown")), 
       aes(x=Month, y= `Rate/100,000`, group=Sex, color=Sex)) +
  geom_line() +
  labs(title = "Rate of attendances per 100,000 in Scottish MIU/Other by Sex",
       subtitle = "Note: MIU/Other refer to minor injuries units (MIU), small hospitals and health centres",
       x = "Year",
       y = "Rate of attendances per 100,000")
dev.off()

#To draw line graph for A&E attendance by sex
whoattends_sex_total <- whoattends_sex %>% 
 rename(c("Rateperhundredthousand" = "Rate/100,000"))

whoattends_sex_total<- whoattends_sex_total %>% 
  select(Month, Sex, Attendances, Rateperhundredthousand) %>% 
  group_by(Sex, Month)%>% 
  summarise(Attendances=sum(Attendances), 
            Rateperhundredthousand=sum(Rateperhundredthousand))


Attendanceratetotalbysex <- ggplot(data=whoattends_sex_total %>% filter(!Sex%in% c("Unknown")) , aes(x=Month, y=Rateperhundredthousand, group=Sex, color=Sex)) +
  geom_line() +
  #scale_x_continuous(breaks=seq(2007,2024,2))+
  scale_y_continuous(labels=scales::comma)+
  labs(
    #title = "Scottish A&E attendance rate per 100,000 by sex",
    x = "Year",
    y = "A&E attendance rate per 100,000")
save_plot("Output/Attendanceratetotalbysex.svg", fig = Attendanceratetotalbysex, width = 14, height = 12)
