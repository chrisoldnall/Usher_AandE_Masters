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



