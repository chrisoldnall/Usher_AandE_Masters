#Scottish A&E data - Descriptive statistics

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

###This analysis is from July 2007 up to Dec 2022

#Loading A&E monthly attendance and waiting times csv file
Covid_monthlyae_activitydescrip <- read_csv(here("Rawdata", "monthlyae_activity_202406.csv"))

#To create a date column
#creating a new column for Year, monthnumeric and day using data from the Month column
Covid_monthlyae_activitydescrip$Year <- substr(Covid_monthlyae_activitydescrip$Month, 1,4)
Covid_monthlyae_activitydescrip$monthnumeric <- substr(Covid_monthlyae_activitydescrip$Month, 5,6)
Covid_monthlyae_activitydescrip$day <- "01"

#converting from character to numeric variable
Covid_monthlyae_activitydescrip$Year <- as.numeric(Covid_monthlyae_activitydescrip$Year)
Covid_monthlyae_activitydescrip$monthnumeric <- as.numeric(Covid_monthlyae_activitydescrip$monthnumeric)
Covid_monthlyae_activitydescrip$day <- as.numeric(Covid_monthlyae_activitydescrip$day)

#making a date column using the Year, monthnumeric and day columns
Covid_monthlyae_activitydescrip<- Covid_monthlyae_activitydescrip %>% 
  mutate(date=make_date(Year, monthnumeric, day))

#Restrict data to years till Dec 2022 (period between 2007-07-01 to 2022-12-31)
Covid_monthlyae_activitydescrip <- Covid_monthlyae_activitydescrip %>%filter(between(date, as.Date('2007-07-01'), as.Date('2022-12-31')))

#Checking if there are null values, result returned 129648
sum(is.na(Covid_monthlyae_activitydescrip))
#Checking if there are null values for NumberOfAttendancesAll, result returned zero
sum(is.na(Covid_monthlyae_activitydescrip$NumberOfAttendancesAll))
#Checking if there are null values for NumberWithin4HoursAll, result returned zero
sum(is.na(Covid_monthlyae_activitydescrip$NumberWithin4HoursAll))
#Checking if there are null values for PercentageWithin4HoursAll, result returned zero
sum(is.na(Covid_monthlyae_activitydescrip$PercentageWithin4HoursAll))

#Eyeballing the dataset
view(Covid_monthlyae_activitydescrip)
head(Covid_monthlyae_activitydescrip)
str(Covid_monthlyae_activitydescrip)
unique(Covid_monthlyae_activitydescrip$HBT)

summary(Covid_monthlyae_activitydescrip)
#NumberOfAttendancesAll: min=1, max=11579, mean=1515.9, median 387.5
#PercentageWithin4HoursAll: min=40.2, max=100, mean=97.02, median=99.8 

#Summary for the the years till Dec 2022 (period between 2007-07-01 to 2022-12-31) split by ED and MIU/Other
Covid_monthlyae_activitydescrip %>%filter(DepartmentType == 'Emergency Department')%>%summary()
#NumberOfAttendancesAll: min=137, max=11579, mean=3575, median=3540
#PercentageWithin4HoursAll: min=40.2, max=100, mean=92.74, median=95.6
Covid_monthlyae_activitydescrip %>%filter(DepartmentType == 'Minor Injury Unit or Other')%>%summary()
#NumberOfAttendancesAll: min=1, max=4394, mean=370, median=139
#PercentageWithin4HoursAll: min=49.6, max=100, mean=99.41, median=100

#Count number of ED and MIU/Other sites
Covid_monthlyae_activitydescrip %>% filter(DepartmentType=="Emergency Department") %>% count(TreatmentLocation)
Covid_monthlyae_activitydescrip %>% filter(DepartmentType=="Minor Injury Unit or Other") %>% count(TreatmentLocation)
#ED = 35, MIU/Other=74

#Entry with the lowest and highest NumberOfAttendancesAll
Covid_monthlyae_activitydescrip[which.min(Covid_monthlyae_activitydescrip$NumberOfAttendancesAll),]
Covid_monthlyae_activitydescrip[which.max(Covid_monthlyae_activitydescrip$NumberOfAttendancesAll),]
#min= 1, 200711, Y109H Moffat Community Hospital (MIU/Other)
#max=11,579, 201908, S314H Royal Infirmary of Edinburgh (ED)

#Entry with the lowest and highest PercentageWithin4HoursAll
Covid_monthlyae_activitydescrip[which.min(Covid_monthlyae_activitydescrip$PercentageWithin4HoursAll),]
Covid_monthlyae_activitydescrip[which.max(Covid_monthlyae_activitydescrip$PercentageWithin4HoursAll),]
#min = 40.2%, 202210, V217H Forth Valley Royal Hospital (ED)
#max=100%, 200707, A101H Arran War Memorial Hospital (MIU/Other)


Covid_monthlyae_activitydescrip %>%
  summarise(mean=mean(NumberOfAttendancesAll), 
            median = median(NumberOfAttendancesAll),
            sd=sd(NumberOfAttendancesAll),
            min=min(NumberOfAttendancesAll),
            max=max(NumberOfAttendancesAll), 
            var=var(NumberOfAttendancesAll))
#mean=1516, median=388, sd=2108, min=1, max=11579, var=4445488


Covid_monthlyae_activitydescrip %>%filter(DepartmentType == 'Emergency Department')%>%
  summarise(mean=mean(NumberOfAttendancesAll), 
            median = median(NumberOfAttendancesAll),
            sd=sd(NumberOfAttendancesAll),
            min=min(NumberOfAttendancesAll),
            max=max(NumberOfAttendancesAll),
            var=var(NumberOfAttendancesAll))
#mean=3575, median=3540, sd=2272, min=137, max=11579, var=5163757


Covid_monthlyae_activitydescrip %>%filter(DepartmentType == 'Minor Injury Unit or Other')%>%
  summarise(mean=mean(NumberOfAttendancesAll), 
            median = median(NumberOfAttendancesAll),
            sd=sd(NumberOfAttendancesAll),
            min=min(NumberOfAttendancesAll),
            max=max(NumberOfAttendancesAll), 
            var=var(NumberOfAttendancesAll))
#mean=370, median=139,  sd=612, min=1,  max=4394, var=374601


Covid_monthlyae_activitydescriptotal <- Covid_monthlyae_activitydescrip %>%
  select(date, Year, HBT, NumberOfAttendancesAll, NumberWithin4HoursAll) %>% 
  group_by(date) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll),
            NumberWithin4HoursAll=sum(NumberWithin4HoursAll))

#Histogram, QQ plot and KS test using A&E activity dataset. Same analysis using the A&E demographics dataset is in ScottishAECovidGLM2 R-script.

#Histogram of total number of attendances July 2007-Dec 2022- distribution skewed to the right, not normal
HistogramCovid_monthlyae_activitydescriptotal <- Covid_monthlyae_activitydescriptotal %>% 
  ggplot(aes(x=NumberOfAttendancesAll))+
  geom_histogram()
save_plot("Output/HistogramCovid_monthlyae_activitydescriptotal.svg", fig = HistogramCovid_monthlyae_activitydescriptotal, width = 14, height = 12)

#Refer to Data analysis for epidemiology - Week 2 Statistical inference in R (part 1) - 'Assumption checking and data transformation'
#Q_Q plot Jan 2018-Dec 2022
QQplotCovid_monthlyae_activitydescriptotal <- Covid_monthlyae_activitydescriptotal %>% 
  ggplot(aes(sample=NumberOfAttendancesAll)) +
  stat_qq() +
  stat_qq_line(color=2)
save_plot("Output/QQplotCovid_monthlyae_activitydescriptotal.svg", fig = QQplotCovid_monthlyae_activitydescriptotal, width = 14, height = 12)

#Kolmogorov-Smirnov test of normality July 2007-Dec 2022
Covid_monthlyae_activitydescriptotal %>% 
  pull(NumberOfAttendancesAll) %>% 
  ks.test(., "pnorm", mean=mean(.), sd=sd(.))
#D = 0.1297, p-value = 0.00383
#alternative hypothesis: two-sided
#Warning message:
#  In ks.test.default(., "pnorm", mean = mean(.), sd = sd(.)) :
#  ties should not be present for the Kolmogorov-Smirnov test