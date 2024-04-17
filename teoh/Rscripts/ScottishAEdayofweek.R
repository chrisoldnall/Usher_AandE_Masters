#Scottish A&E data - WHEN PEOPLE ATTEND - DAY OF WEEK

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

