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

###This analysis is from Jan 2018 up to Dec 2022

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

#Restrict data to years till Dec 2022 (period between 2018-01-01 to 2022-12-31)
Covid_monthlyae_activitydescrip <- Covid_monthlyae_activitydescrip %>%filter(between(date, as.Date('2018-01-01'), as.Date('2022-12-31')))

#2007-07-01 to 2022-12-31: Checking if there are null values, result returned 129648
#Jan 2018 up to Dec 2022: Checking if there are null values, result returned 35648
sum(is.na(Covid_monthlyae_activitydescrip))
#Checking if there are null values for NumberOfAttendancesAll, result returned zero for 2007-07-01 to 2022-12-31 and Jan 2018 up to Dec 2022
sum(is.na(Covid_monthlyae_activitydescrip$NumberOfAttendancesAll))
#Checking if there are null values for NumberWithin4HoursAll, result returned zero for 2007-07-01 to 2022-12-31 and Jan 2018 up to Dec 2022
sum(is.na(Covid_monthlyae_activitydescrip$NumberWithin4HoursAll))
#Checking if there are null values for PercentageWithin4HoursAll, result returned zero for 2007-07-01 to 2022-12-31 and Jan 2018 up to Dec 2022
sum(is.na(Covid_monthlyae_activitydescrip$PercentageWithin4HoursAll))

#Eyeballing the dataset
view(Covid_monthlyae_activitydescrip)
head(Covid_monthlyae_activitydescrip)
str(Covid_monthlyae_activitydescrip)
unique(Covid_monthlyae_activitydescrip$HBT)

summary(Covid_monthlyae_activitydescrip)
#July 2007 up to Dec 2022:NumberOfAttendancesAll: min=1, max=11579, mean=1515.9, median 387.5
#July 2007 up to Dec 2022:PercentageWithin4HoursAll: min=40.2, max=100, mean=97.02, median=99.8 

#Jan 2018 up to Dec 2022:NumberOfAttendancesAll:min=1, max=11579, mean=1711, median=537
#Jan 2018 up to Dec 2022:PercentageWithin4HoursAll:min=40.20, max= 100.00, mean=93.97, median=98.90 

#Summary for the the years split by ED and MIU/Other
Covid_monthlyae_activitydescrip %>%filter(DepartmentType == 'Emergency Department')%>%summary()
#July 2007 up to Dec 2022: NumberOfAttendancesAll: min=137, max=11579, mean=3575, median=3540
#July 2007 up to Dec 2022: PercentageWithin4HoursAll: min=40.2, max=100, mean=92.74, median=95.6

#Jan 2018 up to Dec 2022:NumberOfAttendancesAll: min=195, max=11579, mean=3581, median=3200
#Jan 2018 up to Dec 2022:PercentageWithin4HoursAll:min=40.20, max=99.60, mean=86.65, median=90.70

Covid_monthlyae_activitydescrip %>%filter(DepartmentType == 'Minor Injury Unit or Other')%>%summary()
#July 2007 up to Dec 2022: NumberOfAttendancesAll: min=1, max=4394, mean=370, median=139
#July 2007 up to Dec 2022: PercentageWithin4HoursAll: min=49.6, max=100, mean=99.41, median=100

#Jan 2018 up to Dec 2022:NumberOfAttendancesAll: min=1.0, max=4394.0, mean=443.7, median=155.0
#Jan 2018 up to Dec 2022:PercentageWithin4HoursAll:min=49.60, max=100.00, mean=98.92, median=100.00

#Count number of ED and MIU/Other sites
Covid_monthlyae_activitydescrip %>% filter(DepartmentType=="Emergency Department") %>% count(TreatmentLocation)
Covid_monthlyae_activitydescrip %>% filter(DepartmentType=="Minor Injury Unit or Other") %>% count(TreatmentLocation)
#July 2007 up to Dec 2022: ED = 35, MIU/Other=74
#Jan 2018 up to Dec 2022: ED=30, MIU/Other=64
#don't have treatment location in the A&E demographics dataset so couldn't compare with that

#Entry with the lowest and highest NumberOfAttendancesAll
Covid_monthlyae_activitydescrip[which.min(Covid_monthlyae_activitydescrip$NumberOfAttendancesAll),]
Covid_monthlyae_activitydescrip[which.max(Covid_monthlyae_activitydescrip$NumberOfAttendancesAll),]
#July 2007 up to Dec 2022: min= 1, 200711, Y109H Moffat Community Hospital (MIU/Other)
#July 2007 up to Dec 2022: max=11,579, 201908, S314H Royal Infirmary of Edinburgh (ED)

#Jan 2018 up to Dec 2022: min=1, 201805, Y109H Moffat Community Hospital (MIU/Other)
#Jan 2018 up to Dec 2022: max=11579, 201908, s314H Royal Infirmary of Edinburgh (ED)

#Entry with the lowest and highest PercentageWithin4HoursAll
Covid_monthlyae_activitydescrip[which.min(Covid_monthlyae_activitydescrip$PercentageWithin4HoursAll),]
Covid_monthlyae_activitydescrip[which.max(Covid_monthlyae_activitydescrip$PercentageWithin4HoursAll),]
#July 2007 up to Dec 2022: min = 40.2%, 202210, V217H Forth Valley Royal Hospital (ED)
#July 2007 up to Dec 2022: max=100%, 200707, A101H Arran War Memorial Hospital (MIU/Other)

#Jan 2018 up to Dec 2022: min = 40.2%, 202210, V217H Forth Valley Royal Hospital (ED)
#Jan 2018 up to Dec 2022: max=100%, 201801, A216H Girvan Community Hospital(MIU/Other)

Covid_monthlyae_activitydescrip %>%
  summarise(mean=mean(NumberOfAttendancesAll), 
            median = median(NumberOfAttendancesAll),
            sd=sd(NumberOfAttendancesAll),
            min=min(NumberOfAttendancesAll),
            max=max(NumberOfAttendancesAll), 
            var=var(NumberOfAttendancesAll))
#July 2007 up to Dec 2022: mean=1516, median=388, sd=2108, min=1, max=11579, var=4445488
#Jan 2018 up to Dec 2022: mean=1711, median=537, sd=2262, min=1, max=11579, var=5116963

Covid_monthlyae_activitydescrip %>%filter(DepartmentType == 'Emergency Department')%>%
  summarise(mean=mean(NumberOfAttendancesAll), 
            median = median(NumberOfAttendancesAll),
            sd=sd(NumberOfAttendancesAll),
            min=min(NumberOfAttendancesAll),
            max=max(NumberOfAttendancesAll),
            var=var(NumberOfAttendancesAll))
#July 2007 up to Dec 2022: mean=3575, median=3540, sd=2272, min=137, max=11579, var=5163757
#Jan 2018 up to Dec 2022: min=3581, median=3200, sd=2457, min=195, max=11579, var=6035261

Covid_monthlyae_activitydescrip %>%filter(DepartmentType == 'Minor Injury Unit or Other')%>%
  summarise(mean=mean(NumberOfAttendancesAll), 
            median = median(NumberOfAttendancesAll),
            sd=sd(NumberOfAttendancesAll),
            min=min(NumberOfAttendancesAll),
            max=max(NumberOfAttendancesAll), 
            var=var(NumberOfAttendancesAll))
#July 2007 up to Dec 2022: mean=370, median=139,  sd=612, min=1,  max=4394, var=374601
#Jan 2018 up to Dec 2022: mean=444, median=155, sd=720, min=1, max=4394, var=518822

Covid_monthlyae_activitydescriptotal <- Covid_monthlyae_activitydescrip %>%
  select(date, Year, HBT, NumberOfAttendancesAll, NumberWithin4HoursAll) %>% 
  group_by(date) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll),
            NumberWithin4HoursAll=sum(NumberWithin4HoursAll))

#Histogram, QQ plot and KS test using A&E activity dataset. Same analysis using the A&E demographics dataset is in ScottishAECovidGLM2 R-script.

#Histogram of total number of attendances July 2007-Dec 2022- distribution skewed to the left, not normal
#Histogram of total number of attendances July 2007-Dec 2022- distribution skewed to the left, not normal
HistogramCovid_monthlyae_activitydescriptotal <- Covid_monthlyae_activitydescriptotal %>% 
  ggplot(aes(x=NumberOfAttendancesAll))+
  geom_histogram()+
  labs(x= "Number of A&E Attendances", 
       y = "Count")
save_plot("Output/HistogramCovid_monthlyae_activitydescriptotal.svg", fig = HistogramCovid_monthlyae_activitydescriptotal, width = 14, height = 12)

#Refer to Data analysis for epidemiology - Week 2 Statistical inference in R (part 1) - 'Assumption checking and data transformation'
#Q_Q plot Jan 2018-Dec 2022
QQplotCovid_monthlyae_activitydescriptotal <- Covid_monthlyae_activitydescriptotal %>% 
  ggplot(aes(sample=NumberOfAttendancesAll)) +
  stat_qq() +
  stat_qq_line(color=2)+
  labs(x= "Standard normalised theoretical distribution", 
       y = "Data distribution")
save_plot("Output/QQplotCovid_monthlyae_activitydescriptotal.svg", fig = QQplotCovid_monthlyae_activitydescriptotal, width = 14, height = 12)

#Kolmogorov-Smirnov test of normality
Covid_monthlyae_activitydescriptotal %>% 
  pull(NumberOfAttendancesAll) %>% 
  ks.test(., "pnorm", mean=mean(.), sd=sd(.))
#July 2007 up to Dec 2022: D = 0.1297, p-value = 0.00383
#alternative hypothesis: two-sided
#Warning message:
#  In ks.test.default(., "pnorm", mean = mean(.), sd = sd(.)) :
#  ties should not be present for the Kolmogorov-Smirnov test
#Jan 2018 up to Dec 2022: D = 0.15935, p-value = 0.08486
#alternative hypothesis: two-sided