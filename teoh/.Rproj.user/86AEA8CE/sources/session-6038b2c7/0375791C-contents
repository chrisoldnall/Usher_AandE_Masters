#Scottish A&E data - discharge destination

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

#DISCHARGE DESTINATION
#Importing data for discharge destination
dischargedestination <- read_excel("Rawdata/2023-09-05-dischargedestination.xlsx", 
                                   sheet = "NHS Scotland")
str(dischargedestination)

#Change character variable to year-month-date
dischargedestination$Month<- ymd(dischargedestination$Month)

dischargedestination_bydischarge <- dischargedestination %>% 
  group_by(Month, Discharge) %>% 
  summarise(Attendances=sum(Attendances))


##GRAPH NOT WORKING
ggplot(data=dischargedestination_bydischarge,
       aes(x=Month, y=Attendances, group=Discharge, color=Discharge)+
         geom_line() +
         labs(title = "ED attendance by discharge destination",
              x = "Year",
              y = "Attendances"))

