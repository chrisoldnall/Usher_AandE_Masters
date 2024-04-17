#Scottish A&E data - WHEN PEOPLE ATTEND - deprivation

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

#DEPRIVATION
#Importing deprivation data
deprivation <- read_excel("Rawdata/2023-09-05-whoattends-deprivation.xlsx", 
                          sheet = "Health Board")
deprivationScot <-read_excel("Rawdata/2023-09-05-whoattends-deprivation.xlsx", 
                             sheet = "NHS Scotland")

view(deprivation)
str(deprivation)

#Checking if there are null values for Rate/100,000 for ED NHS Fife in 2022, returned 24
sum(is.na(EDFife2022_deprivation$`Rate/100,000`))

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

