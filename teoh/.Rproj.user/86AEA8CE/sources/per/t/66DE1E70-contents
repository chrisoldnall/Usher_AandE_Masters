#Scottish A&E data - attendances heat map

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

max(attendance_monthenddate$NumberOfAttendancesAll)
#153083
min(attendance_monthenddate$NumberOfAttendancesAll)
#65117

png(file = "Output/attendancemonthyearheatmap.png")
ggplot(attendance_monthenddate, aes(x = year, y = month, fill = NumberOfAttendancesAll)) +
  geom_tile() +
  #scale_fill_distiller(palette = "Blues", direction = +1) +
  #scale_fill_distiller(palette = "YlOrBr", direction = +1) +
  scale_fill_distiller(palette = "OrRd", 
                       limits=c(65000, 154000),
                       guide_legend(title = "Attendances"),
                       direction = +1) +
  scale_y_discrete(limits = rev(levels(as.factor(attendance_monthenddate$month))))+
  labs(title = "Heatmap of Scottish accident and \nemergency attendances by month and year",
       x = "Year",
       y = "Month")+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
dev.off()

#same heatmap as above but with different colouration and scale

attendancemonthyearheatmap1 <- ggplot(attendance_monthenddate, aes(x = year, y = month, fill = NumberOfAttendancesAll)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("blue","green", "yellow", "red"))(1000),
                       limits=c(65000, 154000),
                       guide_legend(title = "Attendances")) +
  scale_y_discrete(limits = rev(levels(as.factor(attendance_monthenddate$month))))+
  labs(
    #title = "Heatmap of Scottish accident and \nemergency attendances by month and year",
    x = "Year",
    y = "Month")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=.5, hjust=1, size = 9))
save_plot("Output/attendancemonthyearheatmap1.svg", fig = attendancemonthyearheatmap1, width = 16, height = 14)

#ED

max(attendance_monthenddateED$NumberOfAttendancesAll)
#127416
min(attendance_monthenddateED$NumberOfAttendancesAll)
#57123

png(file = "Output/attendanceEDmonthyearscaledheatmap.png")
ggplot(attendance_monthenddateED, aes(x = year, y = month, fill = NumberOfAttendancesAll)) +
  geom_tile() +
  #scale_fill_gradient(low="white", high="red") +
  #scale_fill_distiller(palette = "YlOrBr", direction = +1) +
  #scale_fill_distiller(palette = "Reds", direction = +1) +
  scale_fill_distiller(palette = "OrRd", 
                       limits=c(7000, 154000),
                       guide_legend(title = "Attendances"),
                       direction = +1) +
  scale_y_discrete(limits = rev(levels(as.factor(attendance_monthenddateED$month))))+
  labs(title = "Heatmap of Scottish emergency department \nattendances by month and year",
       x = "Year",
       y = "Month")+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
dev.off()

#same heatmap as above but with different colouration and scale

attendanceEDmonthyearscaledheatmap1 <- ggplot(attendance_monthenddateED, aes(x = year, y = month, fill = NumberOfAttendancesAll)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("blue","green", "yellow", "red"))(1000),
                       limits=c(7000, 154000),
                       guide_legend(title = "Attendances")) +
  scale_y_discrete(limits = rev(levels(as.factor(attendance_monthenddateED$month))))+
  labs(
    #title = "Heatmap of Scottish emergency department \nattendances by month and year",
    x = "Year",
    y = "Month")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=.5, hjust=1, size = 9))
save_plot("Output/attendanceEDmonthyearscaledheatmap1.svg", fig = attendanceEDmonthyearscaledheatmap1, width = 16, height = 14)

png(file = "Output/attendanceEDmonthyearheatmap.png")
ggplot(attendance_monthenddateED, aes(x = year, y = month, fill = NumberOfAttendancesAll)) +
  geom_tile() +
  #scale_fill_gradient(low="white", high="red") +
  #scale_fill_distiller(palette = "YlOrBr", direction = +1) +
  #scale_fill_distiller(palette = "Reds", direction = +1) +
  scale_fill_distiller(palette = "OrRd", 
                       #limits=c(7000, 154000),
                       guide_legend(title = "Attendances"),
                       direction = +1) +
  scale_y_discrete(limits = rev(levels(as.factor(attendance_monthenddateED$month))))+
  labs(title = "Heatmap of Scottish emergency department \nattendances by month and year",
       x = "Year",
       y = "Month")+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
dev.off()

#same heatmap as above but with different colouration and scale

attendanceEDmonthyearheatmap1 <- ggplot(attendance_monthenddateED, aes(x = year, y = month, fill = NumberOfAttendancesAll)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("blue","green", "yellow", "red"))(1000),
                       guide_legend(title = "Attendances")) +
  scale_y_discrete(limits = rev(levels(as.factor(attendance_monthenddateED$month))))+
  labs(
    #title = "Heatmap of Scottish emergency department \nattendances by month and year",
    x = "Year",
    y = "Month")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=.5, hjust=1, size = 9))
save_plot("Output/attendanceEDmonthyearheatmap1.svg", fig = attendanceEDmonthyearheatmap1, width = 16, height = 14)

#MIU/Other

max(attendance_monthenddateMIUOther$NumberOfAttendancesAll)
#28583
min(attendance_monthenddateMIUOther$NumberOfAttendancesAll)
#7994

#adjusted scale to be the same as ED
png(file = "Output/attendanceMIUmonthyearscaledheatmap.png")
ggplot(attendance_monthenddateMIUOther, aes(x = year, y = month, fill = NumberOfAttendancesAll)) +
  geom_tile() +
  #scale_fill_distiller(palette = "Blues", direction = +1) +
  #scale_fill_distiller(palette = "YlOrBr", direction = +1) +
  scale_fill_distiller(palette = "OrRd", 
                       limits=c(7000, 154000),
                       guide_legend(title = "Attendances"),
                       direction = +1) +
  scale_y_discrete(limits = rev(levels(as.factor(attendance_monthenddateMIUOther$month))))+
  labs(title = "Heatmap of Scottish minor ailment and \nother units by month and year",
       x = "Year",
       y = "Month")+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
dev.off()

#same heatmap as above but with different colouration

attendanceMIUmonthyearscaledheatmap1 <- ggplot(attendance_monthenddateMIUOther, aes(x = year, y = month, fill = NumberOfAttendancesAll)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("blue","green", "yellow", "red"))(1000),
                       limits=c(7000, 154000),
                       guide_legend(title = "Attendances")) +
  scale_y_discrete(limits = rev(levels(as.factor(attendance_monthenddateMIUOther$month))))+
  labs(
    #title = "Heatmap of Scottish minor ailment and \nother units by month and year",
    x = "Year",
    y = "Month")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=.5, hjust=1, size = 9))
save_plot("Output/attendanceMIUmonthyearscaledheatmap1.svg", fig = attendanceMIUmonthyearscaledheatmap1, width = 16, height = 14)

#did not adjust scale to be the same as ED
png(file = "Output/attendanceMIUmonthyearheatmap.png")
ggplot(attendance_monthenddateMIUOther, aes(x = year, y = month, fill = NumberOfAttendancesAll)) +
  geom_tile() +
  #scale_fill_distiller(palette = "Blues", direction = +1) +
  #scale_fill_distiller(palette = "YlOrBr", direction = +1) +
  scale_fill_distiller(palette = "OrRd", 
                       guide_legend(title = "Attendances"),
                       direction = +1) +
  scale_y_discrete(limits = rev(levels(as.factor(attendance_monthenddateMIUOther$month))))+
  labs(title = "Heatmap of Scottish minor ailment and \nother units by month and year",
       x = "Year",
       y = "Month")+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
dev.off()

#same as above but with different colouration

attendanceMIUmonthyearheatmap1 <- ggplot(attendance_monthenddateMIUOther, aes(x = year, y = month, fill = NumberOfAttendancesAll)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("blue","green", "yellow", "red"))(1000),
                       guide_legend(title = "Attendances")) +
  scale_y_discrete(limits = rev(levels(as.factor(attendance_monthenddateMIUOther$month))))+
  labs(
    #title = "Heatmap of Scottish minor ailment and \nother units by month and year",
    x = "Year",
    y = "Month")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=.5, hjust=1, size = 9))
save_plot("Output/attendanceMIUmonthyearheatmap1.svg", fig = attendanceMIUmonthyearheatmap1, width = 16, height = 14)

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

max(arrivalhrScotEDwkdy$Attendances)
#6413

png(file = "Output/arrivalhrScotEDwkdyheatmap.png")
ggplot(arrivalhrScotEDwkdy, aes(x = Month, y = Hour, fill = Attendances)) +
  geom_tile(
    width=50 #to remove the vertical white lines
  ) +
  scale_fill_distiller(palette = "OrRd", 
                       limits=c(0, 6500),
                       direction = +1
                       #, guide_legend(title = "Number of \nattendances")
  ) +
  labs(title = "Heatmap of Scottish emergency department attendances by \nweekday arrival hour",
       x = "Year",
       y = "Arrival hour") 
dev.off()

#same heatmap as above but with different colouration and scale

arrivalhrScotEDwkdyheatmap1 <- ggplot(arrivalhrScotEDwkdy, aes(x = Month, y = Hour, fill = Attendances)) +
  geom_tile(
    width=50 #to remove the vertical white lines
  ) +
  scale_fill_gradientn(colors = colorRampPalette(c("blue","green", "yellow", "red"))(1000),
                       limits=c(0, 6500),
                       guide_legend(title = "Attendances")) +
  scale_y_discrete(limits = rev(levels(as.factor(arrivalhrScotEDwkdy$Hour))))+
  labs(
    #title = "Heatmap of Scottish emergency department \nattendances by weekday arrival hour",
    x = "Year",
    y = "Arrival hour")+
  #theme_minimal() +
  theme(axis.ticks = element_line())
#axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
save_plot("Output/arrivalhrScotEDwkdyheatmap1.svg", fig = arrivalhrScotEDwkdyheatmap1, width = 17, height = 18)

#Heat map for weekday MIU/Other Only attendance by arrival hour
arrivalhrScotMIUOtherwkdy <- arrivalhrScot %>% 
  filter(Type == "MIU/Other Only", Week == "Weekday") 
#group_by(MonthEndingDate) %>% 
#summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll))

#adjusted scale to be the same as ED
png(file = "Output/arrivalhrScotMIUOtherwkdyscaledheatmap.png")
ggplot(arrivalhrScotMIUOtherwkdy, aes(x = Month, y = Hour, fill = Attendances)) +
  geom_tile(
    width=50 #to remove the vertical white lines
  ) +
  scale_fill_distiller(palette = "OrRd", 
                       limits=c(0, 6500),
                       direction = +1
                       #, guide_legend(title = "Number of \nattendances")
  ) +
  labs(title = "Heatmap of Scottish minor injuries and other units attendances by \nweekday arrival hour",
       x = "Year",
       y = "Arrival hour")
dev.off()

#same heatmap as above but with different colouration and scale

arrivalhrScotMIUOtherwkdyscaledheatmap1 <- ggplot(arrivalhrScotMIUOtherwkdy, aes(x = Month, y = Hour, fill = Attendances)) +
  geom_tile(
    width=50 #to remove the vertical white lines
  ) +
  scale_fill_gradientn(colors = colorRampPalette(c("blue","green", "yellow", "red"))(1000),
                       limits=c(0, 6500),
                       guide_legend(title = "Attendances")) +
  scale_y_discrete(limits = rev(levels(as.factor(arrivalhrScotEDwkdy$Hour))))+
  labs(
    #title = "Heatmap of Scottish minor injuries and other units attendances by \nweekday arrival hour",
    x = "Year",
    y = "Arrival hour")+
  #theme_minimal() +
  theme(axis.ticks = element_line())
#axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
save_plot("Output/arrivalhrScotMIUOtherwkdyscaledheatmap1.svg", fig = arrivalhrScotMIUOtherwkdyscaledheatmap1, width = 17, height = 18)

#did not adjust scale to be the same as ED
png(file = "Output/arrivalhrScotMIUOtherwkdyheatmap.png")
ggplot(arrivalhrScotMIUOtherwkdy, aes(x = Month, y = Hour, fill = Attendances)) +
  geom_tile(
    width=50 #to remove the vertical white lines
  ) +
  scale_fill_distiller(palette = "OrRd", 
                       direction = +1
                       #, guide_legend(title = "Number of \nattendances")
  ) +
  labs(title = "Heatmap of Scottish minor injuries and other units attendances by \nweekday arrival hour",
       x = "Year",
       y = "Arrival hour")
dev.off()

#same heatmap as above but with different colouration and scale

arrivalhrScotMIUOtherwkdyheatmap1 <- ggplot(arrivalhrScotMIUOtherwkdy, aes(x = Month, y = Hour, fill = Attendances)) +
  geom_tile(
    width=50 #to remove the vertical white lines
  ) +
  scale_fill_gradientn(colors = colorRampPalette(c("blue","green", "yellow", "red"))(1000),
                       guide_legend(title = "Attendances")) +
  scale_y_discrete(limits = rev(levels(as.factor(arrivalhrScotEDwkdy$Hour))))+
  labs(
    #title = "Heatmap of Scottish minor injuries and other units attendances by \nweekday arrival hour",
    x = "Year",
    y = "Arrival hour")+
  #theme_minimal() +
  theme(axis.ticks = element_line())
#axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
save_plot("Output/arrivalhrScotMIUOtherwkdyheatmap1.svg", fig = arrivalhrScotMIUOtherwkdyheatmap1, width = 17, height = 18)

#Heat map for weekend ED only attendances by arrival hour
arrivalhrScotEDwknd <- arrivalhrScot %>% 
  filter(Type == "ED Only", Week == "Weekend") 
#group_by(MonthEndingDate) %>% 
#summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll))

max(arrivalhrScotEDwknd$Attendances)
#2631

png(file = "Output/arrivalhrScotEDwkndheatmap.png")
ggplot(arrivalhrScotEDwknd, aes(x = Month, y = Hour, fill = Attendances)) +
  geom_tile(
    width=50 #to remove the vertical white lines
  ) +
  scale_fill_distiller(palette = "OrRd", 
                       limits=c(0, 3000),
                       direction = +1) +
  labs(title = "Heatmap of Scottish emergency department attendances by weekend \narrival hour",
       x = "Year",
       y = "Arrival hour")
dev.off()

#same heatmap as above but with different colouration and scale

arrivalhrScotEDwkndheatmap1 <- ggplot(arrivalhrScotEDwknd, aes(x = Month, y = Hour, fill = Attendances)) +
  geom_tile(
    width=50 #to remove the vertical white lines
  ) +
  scale_fill_gradientn(colors = colorRampPalette(c("blue","green", "yellow", "red"))(1000),
                       limits=c(0, 3000),
                       guide_legend(title = "Attendances")) +
  scale_y_discrete(limits = rev(levels(as.factor(arrivalhrScotEDwkdy$Hour))))+
  labs(
    #title = "Heatmap of Scottish emergency department attendances by weekend \narrival hour",
    x = "Year",
    y = "Arrival hour")+
  #theme_minimal() +
  theme(axis.ticks = element_line())
#axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
save_plot("Output/arrivalhrScotEDwkndheatmap1.svg", fig = arrivalhrScotEDwkndheatmap1, width = 17, height = 18)

#Heat map for weekend MIU/Other Only attendance by arrival hour
arrivalhrScotMIUOtherwknd <- arrivalhrScot %>% 
  filter(Type == "MIU/Other Only", Week == "Weekend") 
#group_by(MonthEndingDate) %>% 
#summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll))

#adjusted scale to be the same as ED
png(file = "Output/arrivalhrScotMIUOtherwkndscaledheatmap.png")
ggplot(arrivalhrScotMIUOtherwknd, aes(x = Month, y = Hour, fill = Attendances)) +
  geom_tile(
    width=50 #to remove the vertical white lines
  ) +
  scale_fill_distiller(palette = "OrRd", 
                       limits=c(0, 3000),
                       direction = +1) +
  labs(title = "Heatmap of Scottish minor injuries and other units attendances by \nweekend arrival hour",
       x = "Year",
       y = "Arrival hour")
dev.off()

#same heatmap as above but with different colouration and scale

arrivalhrScotMIUOtherwkndscaledheatmap1 <- ggplot(arrivalhrScotMIUOtherwknd, aes(x = Month, y = Hour, fill = Attendances)) +
  geom_tile(
    width=50 #to remove the vertical white lines
  ) +
  scale_fill_gradientn(colors = colorRampPalette(c("blue","green", "yellow", "red"))(1000),
                       limits=c(0, 3000),
                       guide_legend(title = "Attendances")) +
  scale_y_discrete(limits = rev(levels(as.factor(arrivalhrScotEDwkdy$Hour))))+
  labs(
    #title = "Heatmap of Scottish minor injuries and other units attendances by \nweekend arrival hour",
    x = "Year",
    y = "Arrival hour")+
  #theme_minimal() +
  theme(axis.ticks = element_line())
#axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
save_plot("Output/arrivalhrScotMIUOtherwkndscaledheatmap1.svg", fig = arrivalhrScotMIUOtherwkndscaledheatmap1, width = 17, height = 18)

#did not adjust scale to be the same as ED
png(file = "Output/arrivalhrScotMIUOtherwkndheatmap.png")
ggplot(arrivalhrScotMIUOtherwknd, aes(x = Month, y = Hour, fill = Attendances)) +
  geom_tile(
    width=50 #to remove the vertical white lines
  ) +
  scale_fill_distiller(palette = "OrRd", direction = +1) +
  labs(title = "Heatmap of Scottish minor injuries and other units attendances by \nweekend arrival hour",
       x = "Year",
       y = "Arrival hour")
dev.off()

#same heatmap as above but with different colouration and scale

arrivalhrScotMIUOtherwkndheatmap1 <- ggplot(arrivalhrScotMIUOtherwknd, aes(x = Month, y = Hour, fill = Attendances)) +
  geom_tile(
    width=50 #to remove the vertical white lines
  ) +
  scale_fill_gradientn(colors = colorRampPalette(c("blue","green", "yellow", "red"))(1000),
                       guide_legend(title = "Attendances")) +
  scale_y_discrete(limits = rev(levels(as.factor(arrivalhrScotEDwkdy$Hour))))+
  labs(
    #title = "Heatmap of Scottish minor injuries and other units attendances by \nweekend arrival hour",
    x = "Year",
    y = "Arrival hour")+
  #theme_minimal() +
  theme(axis.ticks = element_line())
#axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
save_plot("Output/arrivalhrScotMIUOtherwkndheatmap1.svg", fig = arrivalhrScotMIUOtherwkndheatmap1, width = 17, height = 18)

