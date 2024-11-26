#Scottish A&E data - COVID-19 attendance rate

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

#POPULATION ESTIMATE
#Loading population estimate (only till 2022) csv file
Covid_HBpopulation_estimate <- read_csv(here("Rawdata", "20240809Popestimate_HB2019_1981to2022.csv"))

#Checking if there are null values, result returned zero
sum(is.na(Covid_HBpopulation_estimate))

#Filtering out total Scottish estimates (HB S92000003) for each year, leaving just individual HBs in the list
Covid_HBpopulation_estimate <- Covid_HBpopulation_estimate %>%
  select(Year, HB, Sex, AllAges) %>% 
  filter(!HB=="S92000003", Sex=="All")

#Dataframe with the names of the health boards. Note some HB codes have been deprecated and replaced, hence more than 14 entries.
HB_names <- read_csv(here("Rawdata", "HealthBoard_2014_2019_names.csv"))
HB_names_only<-HB_names %>% 
  select(HB, HBName)

#want to merge population estimates with HB names
Covid_HBpopulation_estimate <- merge(Covid_HBpopulation_estimate, HB_names_only, by = "HB")

#Loading A&E monthly attendance and waiting times csv file
Covid_monthlyae_activity <- read_csv(here("Rawdata", "monthlyae_activity_202406.csv"))

#Checking if there are null values, result returned 138776
sum(is.na(Covid_monthlyae_activity))
#Checking if there are null values for NumberOfAttendancesAll, result returned zero
sum(is.na(Covid_monthlyae_activity$NumberOfAttendancesAll))
#Checking if there are null values for NumberWithin4HoursAll, result returned zero
sum(is.na(Covid_monthlyae_activity$NumberWithin4HoursAll))
#Checking if there are null values for PercentageWithin4HoursAll, result returned zero
sum(is.na(Covid_monthlyae_activity$PercentageWithin4HoursAll))

#creating a new column for Year using the first 4 digits of the Month column
Covid_monthlyae_activity$Year <- substr(Covid_monthlyae_activity$Month, 1,4)
Covid_monthlyae_activity$monthnumeric <- substr(Covid_monthlyae_activity$Month, 5,6)

#Attendance and percentage within 4 hours by HB each month
Covid_monthlyae_activity <- Covid_monthlyae_activity %>% 
  select(HBT, NumberOfAttendancesAll, PercentageWithin4HoursAll, Year, monthnumeric) %>% 
  group_by(Year, monthnumeric, HBT) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll), 
            PercentageWithin4HoursAll=mean(PercentageWithin4HoursAll))

#Changing the column name from HBT to HB to align the column names in the activity and population estimate dataframes
colnames(Covid_monthlyae_activity)<- c("Year","monthnumeric", "HB","NumberOfAttendancesAll","PercentageWithin4HoursAll")

#to merge A&E activity with population estimate
Covid_monthlyae_popest <- merge(Covid_monthlyae_activity, Covid_HBpopulation_estimate, by=c("HB","Year"))

#Calculating attendance per population for each month
Covid_monthlyae_popest$attendanceperpop<-Covid_monthlyae_popest$NumberOfAttendancesAll/Covid_monthlyae_popest$AllAges
#Calculating attendance per 1,000 population for each month
Covid_monthlyae_popest$attendanceper1000pop<-Covid_monthlyae_popest$attendanceperpop*1000

#mean attendance per population and attendance per 1,000 population for each year
Covid_monthlyae_popest <- Covid_monthlyae_popest %>% 
  select(HB, Year, NumberOfAttendancesAll, PercentageWithin4HoursAll, AllAges, attendanceperpop, attendanceper1000pop) %>% 
  group_by(Year, HB) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll), 
            PercentageWithin4HoursAll=mean(PercentageWithin4HoursAll),
            AllAges=mean(AllAges),
            attendanceperpop=mean(attendanceperpop),
            attendanceper1000pop=mean(attendanceper1000pop))

#adding in the HB names
Covid_monthlyae_popest  <- merge(Covid_monthlyae_popest , HB_names_only, by = "HB")


str(Covid_monthlyae_activity)




#Month returns as numeric. Lubridate cannot handle just year and month, need to introduce and artificial day of the month to the string
#Covid_monthlyae_activity$Month <- format(as.Date(Covid_monthlyae_activity$Month, format="%Y/%m/%d"))
#concatenating Month with an artificial day of the month 01
#Covid_monthlyae_activity$Date <- paste(Covid_monthlyae_activity$Month,"01")
#Covid_monthlyae_activity$Date <- format(as.Date(Covid_monthlyae_activity$Date, format="%Y/%m/%d")) 

#merge(df1, df2, by.x = "df1ColName", by.y = "df2ColName")

#TIMELINE chart

#Loading the monthy activity csv file
Covid_monthlyae_activity_timeline <- read_csv(here("Rawdata", "monthlyae_activity_202406.csv"))

#creating a new column for Year using the first 4 digits of the Month column
Covid_monthlyae_activity_timeline$Year <- substr(Covid_monthlyae_activity_timeline$Month, 1,4)
Covid_monthlyae_activity_timeline$monthnumeric <- substr(Covid_monthlyae_activity_timeline$Month, 5,6)
Covid_monthlyae_activity_timeline$day <- "01"

#converting from character to numeric variable
Covid_monthlyae_activity_timeline$Year <- as.numeric(Covid_monthlyae_activity_timeline$Year)
Covid_monthlyae_activity_timeline$monthnumeric <- as.numeric(Covid_monthlyae_activity_timeline$monthnumeric)
Covid_monthlyae_activity_timeline$day <- as.numeric(Covid_monthlyae_activity_timeline$day)

#making a date column using the Year, monthnumeric and day columns
Covid_monthlyae_activity_timeline<- Covid_monthlyae_activity_timeline %>% 
  mutate(date=make_date(Year, monthnumeric, day))

#Restrict data to years till Dec 2022 (period between 2018-01-01 to 2022-12-31)
Covid_monthlyae_activity_timeline <- Covid_monthlyae_activity_timeline %>%filter(between(date, as.Date('2018-01-01'), as.Date('2022-12-31')))

#Total attendance each month
Covid_monthlyae_activity_timeline <- Covid_monthlyae_activity_timeline %>% 
  select(date, NumberOfAttendancesAll) %>% 
  group_by(date) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll))

#highlights based on lockdown dates
Covid_aeScotlandactivity_timeline2018to2022<- ggplot(data=Covid_monthlyae_activity_timeline, aes(x=date, y=NumberOfAttendancesAll))+
  geom_point()+
  geom_line(size=1,colour="black")+
  geom_rect(aes(xmin = as.Date("2018-01-01"), xmax = as.Date("2020-03-23"), ymin=-Inf, ymax=Inf), fill="#00FFFF", alpha=.01) + 
  geom_rect(aes(xmin = as.Date("2020-03-23"), xmax = as.Date("2020-05-29"), ymin=-Inf, ymax=Inf), fill="#FF0033", alpha=.01) + #UKlockdown
  geom_rect(aes(xmin = as.Date("2020-05-29"), xmax = as.Date("2020-06-19"), ymin=-Inf, ymax=Inf), fill="#FF0099", alpha=.01) + #Phase 1
  geom_rect(aes(xmin = as.Date("2020-06-19"), xmax = as.Date("2020-07-10"), ymin=-Inf, ymax=Inf), fill="#CC33CC", alpha=.01) + #Phase2
  geom_rect(aes(xmin = as.Date("2020-07-10"), xmax = as.Date("2020-09-23"), ymin=-Inf, ymax=Inf), fill="#CC66FF", alpha=.01) + #Phase3
  geom_rect(aes(xmin = as.Date("2020-09-23"), xmax = as.Date("2021-01-05"), ymin=-Inf, ymax=Inf), fill="#FF00CC", alpha=.01) + #Scottish restrictions
  geom_rect(aes(xmin = as.Date("2021-01-05"), xmax = as.Date("2021-04-02"), ymin=-Inf, ymax=Inf), fill="#FF0033", alpha=.01) + #Scottish mainland lockdown
  geom_rect(aes(xmin = as.Date("2021-04-02"), xmax = as.Date("2021-04-26"), ymin=-Inf, ymax=Inf), fill="#FF00CC", alpha=.01) + #Stay home to stay local
  geom_rect(aes(xmin = as.Date("2021-04-26"), xmax = as.Date("2021-05-17"), ymin=-Inf, ymax=Inf), fill="#CC66FF", alpha=.01) + #Level 3
  geom_rect(aes(xmin = as.Date("2021-05-17"), xmax = as.Date("2021-06-05"), ymin=-Inf, ymax=Inf), fill="#9966FF", alpha=.01) + #Level 2
  geom_rect(aes(xmin = as.Date("2021-06-05"), xmax = as.Date("2021-07-19"), ymin=-Inf, ymax=Inf), fill="#9999FF", alpha=.01) + #Most mainland level 1
  geom_rect(aes(xmin = as.Date("2021-07-19"), xmax = as.Date("2021-08-09"), ymin=-Inf, ymax=Inf), fill="#66CCFF", alpha=.01) + #Level 0
  geom_rect(aes(xmin = as.Date("2021-08-09"), xmax = as.Date("2022-12-31"), ymin=-Inf, ymax=Inf), fill="#00FFFF", alpha=.01) + #Beyond level 0
  #theme_classic()+
    labs(x= "Date", 
       y = "Total number of Scottish A&E attendances")
save_plot("Output/Covid_aeScotlandactivity_timeline2018to2022.svg", fig=Covid_aeScotlandactivity_timeline2018to2022, width=14, height=12)

#to flip the graph on its side
Covid_aeScotlandactivity_timeline2018to2022flip <- Covid_aeScotlandactivity_timeline2018to2022 + coord_flip()
save_plot("Output/Covid_aeScotlandactivity_timeline2018to2022flip.svg", fig=Covid_aeScotlandactivity_timeline2018to2022flip, width=14, height=12)

#highlights based on encoding
#Note encoding here is different from the rest of the R scripts as we swapped from "1 Lockdown \n 2 Tightening \n 3 Easing \n 4 No restriction"
# to "1 No or minimal restrictions \n 2 Easing \n 3 Tightening \n 4 Lockdown"
Covid_aeScotlandactivity_encodingtimeline2018to2022<- ggplot(data=Covid_monthlyae_activity_timeline, aes(x=date, y=NumberOfAttendancesAll))+
  geom_point()+
  geom_line(size=1,colour="black")+
  geom_label(aes(label= ("1 Lockdown \n 2 Tightening \n 3 Easing \n 4 No or minimal restrictions"),x=as.Date("2018-11-01"), y=90000))+
  #legend("bottomleft", inset=0.2, title="Restrictions", c("1 No or minimal restrictions \n 2 Easing \n 3 Tightening \n 4 Lockdown"), col=c("#FF0033", "#FF00CC", "#9966FF", "#00FFFF", lty=1:2, cex=0.8)) +
  geom_rect(aes(xmin = as.Date("2018-01-01"), xmax = as.Date("2020-03-31"), ymin=-Inf, ymax=Inf), fill="#00FFFF", alpha=.01) + #1
  #annotate(geom="text", x=as.Date("2019-02-28"), color="black", y=60100, size=3, label="1")+
  geom_rect(aes(xmin = as.Date("2020-04-01"), xmax = as.Date("2020-05-31"), ymin=-Inf, ymax=Inf), fill="#FF0033", alpha=.01) + #4
  #annotate(geom="text", x=as.Date("2020-04-30"), color="black", y=60100, size=3, label="4")+
  geom_rect(aes(xmin = as.Date("2020-06-01"), xmax = as.Date("2020-09-30"), ymin=-Inf, ymax=Inf), fill="#9900FF", alpha=.01) + #2
  #annotate(geom="text", x=as.Date("2020-07-30"), color="black", y=60100, size=3, label="2")+
  geom_rect(aes(xmin = as.Date("2020-10-01"), xmax = as.Date("2020-12-31"), ymin=-Inf, ymax=Inf), fill="#FF00CC", alpha=.01) + #3
  #annotate(geom="text", x=as.Date("2020-11-15"), color="black", y=60100, size=3, label="3")+
  geom_rect(aes(xmin = as.Date("2021-01-01"), xmax = as.Date("2021-04-30"), ymin=-Inf, ymax=Inf), fill="#FF0033", alpha=.01) + #4
  #annotate(geom="text", x=as.Date("2021-03-01"), color="black", y=60100, size=3, label="4")+
  geom_rect(aes(xmin = as.Date("2021-05-01"), xmax = as.Date("2021-07-31"), ymin=-Inf, ymax=Inf), fill="#9900FF", alpha=.01) + #2
  #annotate(geom="text", x=as.Date("2021-06-15"), color="black", y=60100, size=3, label="2")+
  geom_rect(aes(xmin = as.Date("2021-08-01"), xmax = as.Date("2022-12-31"), ymin=-Inf, ymax=Inf), fill="#00FFFF", alpha=.01) + #1
  #annotate(geom="text", x=as.Date("2022-04-01"), color="black", y=60100, size=3, label="1")+
  labs(x= "Date", 
       y = "Total number of Scottish A&E attendances")
save_plot("Output/Covid_aeScotlandactivity_encodingtimeline2018to2022.svg", fig=Covid_aeScotlandactivity_encodingtimeline2018to2022, width=20, height=12)

#plot of lockdown dates using vertical lines
Covid_aeScotlandactivity_vlinetimeline2018to2022<- ggplot(data=Covid_monthlyae_activity_timeline, aes(x=date, y=NumberOfAttendancesAll))+
  geom_point()+
  geom_line(size=1,colour="blue")+  
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
  geom_vline(xintercept = as.Date("2021-01-05"), color="forestgreen", linewidth=.5)+
  annotate(geom="text", x=as.Date("2021-01-01"), color="forestgreen", y=95000, size=3, label="Scottish mainland lockdown 5/1/21 ")+
  geom_vline(xintercept = as.Date("2021-04-02"), color="cornflowerblue", linewidth=.5)+
  annotate(geom="text", x=as.Date("2021-03-01"), color="cornflowerblue", y=100000, size=3, label="2/4/21 Stay home>local")+
  geom_vline(xintercept = as.Date("2021-04-26"), color="tomato1", linewidth=.5)+
  annotate(geom="text", x=as.Date("2021-04-01"), color="tomato1", y=105000, size=3, label="26/4/21 Level 3")+
  geom_vline(xintercept = as.Date("2021-05-17"), color="royalblue4", linewidth=.5)+
  annotate(geom="text", x=as.Date("2021-05-01"), color="royalblue4", y=110000, size=3, label="17/5/21 Level 2")+
  geom_vline(xintercept = as.Date("2021-06-05"), color="firebrick2", linewidth=.5)+
  annotate(geom="text", x=as.Date("2021-06-01"), color="firebrick2", y=115000, size=3, label="5/6/21 Most mainland level 1")+
  geom_vline(xintercept = as.Date("2021-07-19"), color="sienna", linewidth=.5)+
  annotate(geom="text", x=as.Date("2021-07-01"), color="sienna", y=120000, size=3, label="19/7/21 Level 0")+
  geom_vline(xintercept = as.Date("2021-08-09"), color="springgreen4", linewidth=.5)+
  annotate(geom="text", x=as.Date("2021-08-01"), color="springgreen4", y=125000, size=3, label="9/8/21 Beyond level 0")+
  labs(x= "Date", 
       y = "Total number of Scottish A&E attendances")
save_plot("Output/Covid_aeScotlandactivity_vlinetimeline2018to2022.svg", fig=Covid_aeScotlandactivity_vlinetimeline2018to2022, width=14, height=12)

