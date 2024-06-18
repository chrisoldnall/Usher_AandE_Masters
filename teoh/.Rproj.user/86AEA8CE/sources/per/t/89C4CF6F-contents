#Scottish A&E data - Predict

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

#PREDICT

#Creating the predictions models for the various GLMs

glm_sex_predictions <-predict(glm_sex, type = "response")
summary(glm_sex_predictions)

glm_sexage_predictions <-predict(glm_sexage, type = "response")
summary(glm_sexage_predictions)

glm_sexageSIMD_predictions <-predict(glm_sexageSIMD, type = "response")
summary(glm_sexageSIMD_predictions)

glm_sexageSIMDday_predictions <-predict(glm_sexageSIMDday, type = "response")
summary(glm_sexageSIMDday_predictions)

glm_sexageSIMDdaytype_predictions <-predict(glm_sexageSIMDdaytype, type = "response")
summary(glm_sexageSIMDdaytype_predictions)

glm_sexageSIMDdaytypehour_predictions <-predict(glm_sexageSIMDdaytypehour, type = "response")
summary(glm_sexageSIMDdaytypehour_predictions)

glm_sexageSIMDdaytypehourHB_predictions <-predict(glm_sexageSIMDdaytypehourHB, type = "response")
summary(glm_sexageSIMDdaytypehourHB_predictions)

glm_sexageSIMDdaytypehourHBmonth_predictions <- predict(glm_sexageSIMDdaytypehourHBmonth, type = "response")
summary(glm_sexageSIMDdaytypehourHBmonth_predictions)

#using updated month
glm_sexageSIMDdaytypehourHBupdatedmonth_predictions <- predict(glm_sexageSIMDdaytypehourHBupdatedmonth, type = "response")
summary(glm_sexageSIMDdaytypehourHBupdatedmonth_predictions)

glm_sexageSIMDdaytypehourHBTime_predictions <- predict(glm_sexageSIMDdaytypehourHBTime, type = "response")
summary(glm_sexageSIMDdaytypehourHBTime_predictions)


#model_1_predictions <- predict(model_1, type="response")
#summary(model_1_predictions)

#creating a dataframe with population for all Scotland by month for 2018-2023 to join with sexagesimddayhourdepttypeHB_proportions to draw a graph for fitted versus actual
#includes removing S92000003 which is the population for NHS Scotland

ScotUpdatespopulation2018to2023glmplot <- HBUpdatespopulation_estimate_HBname %>% 
  filter(!HB%in%"S92000003") %>% 
  group_by(Year) %>% 
  summarise(AllAges=sum(AllAges)) %>% 
  filter(Year=="2018"|Year=="2019"|Year=="2020"|Year=="2021"|Year=="2022"|Year=="2023") %>% 
  select(Year, AllAges) 

#Selecting only the columns needed for glm plot and adding a column for year to join with the population data
sexagesimddayhourdepttypeHB_proportionsglmplot <- sexagesimddayhourdepttypeHB_proportions %>% 
  select(Month, Total_attendances)
sexagesimddayhourdepttypeHB_proportionsglmplot$Year <- format(as.Date(sexagesimddayhourdepttypeHB_proportions$Month, format="%Y/%m/%d"),"%Y")

#Same as above but including month
#Selecting only the columns needed for glm plot and adding a column for year to join with the population data
sexagesimddayhourdepttypeHBmonth_proportionsglmplot <- sexagesimddayhourdepttypeHBmonth_proportions %>% 
  select(Month, Total_attendances)
sexagesimddayhourdepttypeHBmonth_proportionsglmplot$Year <- format(as.Date(sexagesimddayhourdepttypeHBmonth_proportions$Month, format="%Y/%m/%d"),"%Y")

#Same as above but including updated month
#Selecting only the columns needed for glm plot and adding a column for year to join with the population data
sexagesimddayhourdepttypeHBupdatedmonth_proportionsglmplot <- sexagesimddayhourdepttypeHBupdatedmonth_proportions %>% 
  select(Month, Total_attendances)
sexagesimddayhourdepttypeHBupdatedmonth_proportionsglmplot$Year <- format(as.Date(sexagesimddayhourdepttypeHBupdatedmonth_proportions$Month, format="%Y/%m/%d"),"%Y")


#Same as above but including Time
#Selecting only the columns needed for glm plot and adding a column for year to join with the population data
sexagesimddayhourdepttypeHBTime_proportionsglmplot <- sexagesimddayhourdepttypeHBTime_proportions %>% 
  select(Month, Total_attendances)
sexagesimddayhourdepttypeHBTime_proportionsglmplot$Year <- format(as.Date(sexagesimddayhourdepttypeHBTime_proportions$Month, format="%Y/%m/%d"),"%Y")


#combining population data with attendance data to create the glm plot
sexagesimddayhourdepttypeHB_proportionsglmplot <- 
  merge(ScotUpdatespopulation2018to2023glmplot, sexagesimddayhourdepttypeHB_proportionsglmplot, by=c("Year"))

#same as above but including month
#combining population data with attendance data to create the glm plot
sexagesimddayhourdepttypeHBmonth_proportionsglmplot <- 
  merge(ScotUpdatespopulation2018to2023glmplot, sexagesimddayhourdepttypeHBmonth_proportionsglmplot, by=c("Year"))

#same as above but including updated month
#combining population data with attendance data to create the glm plot
sexagesimddayhourdepttypeHBupdatedmonth_proportionsglmplot <- 
  merge(ScotUpdatespopulation2018to2023glmplot, sexagesimddayhourdepttypeHBupdatedmonth_proportionsglmplot, by=c("Year"))


#same as above but including Time
#combining population data with attendance data to create the glm plot
sexagesimddayhourdepttypeHBTime_proportionsglmplot <- 
  merge(ScotUpdatespopulation2018to2023glmplot, sexagesimddayhourdepttypeHBTime_proportionsglmplot, by=c("Year"))


#combining the glm predictions for glm_sex_prediction data with actual
sexagesimddayhourdepttypeHB_proportionsglmplot <- sexagesimddayhourdepttypeHB_proportionsglmplot %>% 
  mutate("glmsexprediction" = glm_sex_predictions/AllAges, "rates" = Total_attendances/AllAges)

#same as above but including month
#combining the glm predictions for glm_sex_prediction data with actual
sexagesimddayhourdepttypeHBmonth_proportionsglmplot <- sexagesimddayhourdepttypeHBmonth_proportionsglmplot %>% 
  mutate("glmsexprediction" = glm_sex_predictions/AllAges, "rates" = Total_attendances/AllAges)

#same as above but including updated month
#combining the glm predictions for glm_sex_prediction data with actual
sexagesimddayhourdepttypeHBupdatedmonth_proportionsglmplot <- sexagesimddayhourdepttypeHBupdatedmonth_proportionsglmplot %>% 
  mutate("glmsexprediction" = glm_sex_predictions/AllAges, "rates" = Total_attendances/AllAges)

#same as above but including Time instead of month
#combining the glm predictions for glm_sex_prediction data with actual
sexagesimddayhourdepttypeHBTime_proportionsglmplot <- sexagesimddayhourdepttypeHBTime_proportionsglmplot %>% 
  mutate("glmsexprediction" = glm_sex_predictions/AllAges, "rates" = Total_attendances/AllAges)


#Plotting graph for glm sex prediction versus actual
sexagesimddayhourdepttypeHB_proportionsglmplot %>% 
  ggplot(aes(x=Month))+
  labs(x="Time", y= "Rate of attendance", title = "Rate of Scottish A&E attendances")+
  geom_line(aes(y= rates), color = "black")+
  geom_line(aes(y=glmsexprediction), color = "red")

#combining the glm predictions for the other glm models with sexagesimddayhourdepttypeHB_proportionsglmplot
sexagesimddayhourdepttypeHB_proportionsglmplot <- sexagesimddayhourdepttypeHB_proportionsglmplot %>% 
  mutate("glmsexageprediction" = glm_sexage_predictions/AllAges,
         "glmsexageSIMDprediction" = glm_sexageSIMD_predictions/AllAges,
         "glmsexageSIMDdayprediction" = glm_sexageSIMDday_predictions/AllAges,
         "glmsexageSIMDdaytypeprediction" = glm_sexageSIMDdaytype_predictions/AllAges,
         "glmsexageSIMDdaytypehourprediction" = glm_sexageSIMDdaytypehour_predictions/AllAges,
         "glmsexageSIMDdaytypehourHBprediction" = glm_sexageSIMDdaytypehourHB_predictions/AllAges)

#same as above but including month
#combining the glm predictions for the other glm models with sexagesimddayhourdepttypeHBmonth_proportionsglmplot
sexagesimddayhourdepttypeHBmonth_proportionsglmplot <- sexagesimddayhourdepttypeHBmonth_proportionsglmplot %>% 
  mutate("glmsexageprediction" = glm_sexage_predictions/AllAges,
         "glmsexageSIMDprediction" = glm_sexageSIMD_predictions/AllAges,
         "glmsexageSIMDdayprediction" = glm_sexageSIMDday_predictions/AllAges,
         "glmsexageSIMDdaytypeprediction" = glm_sexageSIMDdaytype_predictions/AllAges,
         "glmsexageSIMDdaytypehourprediction" = glm_sexageSIMDdaytypehour_predictions/AllAges,
         "glmsexageSIMDdaytypehourHBprediction" = glm_sexageSIMDdaytypehourHB_predictions/AllAges,
         "glmsexageSIMDdaytypehourHBmonthprediction" = glm_sexageSIMDdaytypehourHBmonth_predictions/AllAges)

#same as above but including updated month
#combining the glm predictions for the other glm models with sexagesimddayhourdepttypeHBmonth_proportionsglmplot
sexagesimddayhourdepttypeHBupdatedmonth_proportionsglmplot <- sexagesimddayhourdepttypeHBupdatedmonth_proportionsglmplot %>% 
  mutate("glmsexageprediction" = glm_sexage_predictions/AllAges,
         "glmsexageSIMDprediction" = glm_sexageSIMD_predictions/AllAges,
         "glmsexageSIMDdayprediction" = glm_sexageSIMDday_predictions/AllAges,
         "glmsexageSIMDdaytypeprediction" = glm_sexageSIMDdaytype_predictions/AllAges,
         "glmsexageSIMDdaytypehourprediction" = glm_sexageSIMDdaytypehour_predictions/AllAges,
         "glmsexageSIMDdaytypehourHBprediction" = glm_sexageSIMDdaytypehourHB_predictions/AllAges,
         "glmsexageSIMDdaytypehourHBupdatedmonthprediction" = glm_sexageSIMDdaytypehourHBupdatedmonth_predictions/AllAges)

#same as above but including Time instead of month
#combining the glm predictions for the other glm models with sexagesimddayhourdepttypeHBTime_proportionsglmplot
sexagesimddayhourdepttypeHBTime_proportionsglmplot <- sexagesimddayhourdepttypeHBTime_proportionsglmplot %>% 
  mutate("glmsexageprediction" = glm_sexage_predictions/AllAges,
         "glmsexageSIMDprediction" = glm_sexageSIMD_predictions/AllAges,
         "glmsexageSIMDdayprediction" = glm_sexageSIMDday_predictions/AllAges,
         "glmsexageSIMDdaytypeprediction" = glm_sexageSIMDdaytype_predictions/AllAges,
         "glmsexageSIMDdaytypehourprediction" = glm_sexageSIMDdaytypehour_predictions/AllAges,
         "glmsexageSIMDdaytypehourHBprediction" = glm_sexageSIMDdaytypehourHB_predictions/AllAges,
         "glmsexageSIMDdaytypehourHBTimeprediction" = glm_sexageSIMDdaytypehourHBTime_predictions/AllAges)


#Plotting graph for all glm prediction versus actual

glmsexageSIMDdaytypehourHBpredictionallinone <- ggplot(data= sexagesimddayhourdepttypeHB_proportionsglmplot, aes(x=Month))+
  labs(x="Time", y= "Rate of attendance", title = "glmsexageSIMDdaytypehourHBpredictionallinone")+
  geom_line(aes(y= rates), color = "black")+
  geom_line(aes(y=glmsexprediction), color = "red")+
  geom_line(aes(y=glmsexageprediction), color = "blue")+
  geom_line(aes(y=glmsexageSIMDprediction), color = "green")+
  geom_line(aes(y=glmsexageSIMDdayprediction), color = "yellow")+
  geom_line(aes(y=glmsexageSIMDdaytypeprediction), color = "orange")+
  geom_line(aes(y=glmsexageSIMDdaytypehourprediction), color = "purple")+
  geom_line(aes(y=glmsexageSIMDdaytypehourHBprediction), color = "brown")
save_plot("Output/glmsexageSIMDdaytypehourHBpredictionallinone.svg", fig = glmsexageSIMDdaytypehourHBpredictionallinone, width = 6, height = 6)

#same as above including month
glmsexageSIMDdaytypehourHBmonthpredictionallinone <- ggplot(data= sexagesimddayhourdepttypeHBmonth_proportionsglmplot, aes(x=Month))+
  labs(x="Time", y= "Rate of attendance", title = "glmsexageSIMDdaytypehourHBmonthpredictionallinone")+
  geom_line(aes(y= rates), color = "black")+
  geom_line(aes(y=glmsexprediction), color = "red")+
  geom_line(aes(y=glmsexageprediction), color = "blue")+
  geom_line(aes(y=glmsexageSIMDprediction), color = "green")+
  geom_line(aes(y=glmsexageSIMDdayprediction), color = "yellow")+
  geom_line(aes(y=glmsexageSIMDdaytypeprediction), color = "orange")+
  geom_line(aes(y=glmsexageSIMDdaytypehourprediction), color = "purple")+
  geom_line(aes(y=glmsexageSIMDdaytypehourHBprediction), color = "brown")+
  geom_line(aes(y=glmsexageSIMDdaytypehourHBmonthprediction), color = "salmon")
save_plot("Output/glmsexageSIMDdaytypehourHBmonthpredictionallinone.svg", fig = glmsexageSIMDdaytypehourHBmonthpredictionallinone, width = 6, height = 6)

#same as above including updated month
glmsexageSIMDdaytypehourHBupdatedmonthpredictionallinone <- ggplot(data= sexagesimddayhourdepttypeHBupdatedmonth_proportionsglmplot, aes(x=Month))+
  labs(x="Time", y= "Rate of attendance", title = "glmsexageSIMDdaytypehourHBupdatedmonthpredictionallinone")+
  geom_line(aes(y= rates), color = "black")+
  geom_line(aes(y=glmsexprediction), color = "red")+
  geom_line(aes(y=glmsexageprediction), color = "blue")+
  geom_line(aes(y=glmsexageSIMDprediction), color = "green")+
  geom_line(aes(y=glmsexageSIMDdayprediction), color = "yellow")+
  geom_line(aes(y=glmsexageSIMDdaytypeprediction), color = "orange")+
  geom_line(aes(y=glmsexageSIMDdaytypehourprediction), color = "purple")+
  geom_line(aes(y=glmsexageSIMDdaytypehourHBprediction), color = "brown")+
  geom_line(aes(y=glmsexageSIMDdaytypehourHBupdatedmonthprediction), color = "salmon")
save_plot("Output/glmsexageSIMDdaytypehourHBupdatedmonthpredictionallinone.svg", fig = glmsexageSIMDdaytypehourHBupdatedmonthpredictionallinone, width = 6, height = 6)



glmsexprediction <- ggplot(data=sexagesimddayhourdepttypeHB_proportionsglmplot, aes(x=Month))+
  labs(x="Time", y= "Attendance rate", 
       #title = "glmsexprediction"
  )+
  geom_line(aes(y= rates), color = "black")+
  geom_line(aes(y=glmsexprediction), color = "red")
save_plot("Output/glmsexprediction.svg", fig = glmsexprediction, width = 5, height = 5)

glmsexageprediction <- ggplot(data=sexagesimddayhourdepttypeHB_proportionsglmplot, aes(x=Month))+
  labs(x="Time", y= "Attendance rate", 
       #title = "glmsexageprediction"
  )+
  geom_line(aes(y= rates), color = "black")+
  geom_line(aes(y=glmsexageprediction), color = "blue")
save_plot("Output/glmsexageprediction.svg", fig = glmsexageprediction, width = 5, height = 5)

glmsexageSIMDprediction <- ggplot(data=sexagesimddayhourdepttypeHB_proportionsglmplot, aes(x=Month))+
  labs(x="Time", y= "Attendance rate", 
       #title = "glmsexageSIMDprediction"
  )+
  geom_line(aes(y= rates), color = "black")+
  geom_line(aes(y=glmsexageSIMDprediction), color = "green")
save_plot("Output/glmsexageSIMDprediction.svg", fig = glmsexageSIMDprediction, width = 5, height = 5)

glmsexageSIMDdayprediction <- ggplot(data=sexagesimddayhourdepttypeHB_proportionsglmplot, aes(x=Month))+
  labs(x="Time", y= "Attendance rate", 
       #title = "glmsexageSIMDdayprediction"
  )+
  geom_line(aes(y= rates), color = "black")+
  geom_line(aes(y=glmsexageSIMDdayprediction), color = "yellow")
save_plot("Output/glmsexageSIMDdayprediction.svg", fig = glmsexageSIMDdayprediction, width = 5, height = 5)

glmsexageSIMDdaytypeprediction <- ggplot(data=sexagesimddayhourdepttypeHB_proportionsglmplot, aes(x=Month))+
  labs(x="Time", y= "Attendance rate", 
       #title = "glmsexageSIMDdaytypeprediction"
  )+
  geom_line(aes(y= rates), color = "black")+
  geom_line(aes(y=glmsexageSIMDdaytypeprediction), color = "orange")
save_plot("Output/glmsexageSIMDdaytypeprediction.svg", fig = glmsexageSIMDdaytypeprediction, width = 5, height = 5)

glmsexageSIMDdaytypehourprediction <- ggplot(data=sexagesimddayhourdepttypeHB_proportionsglmplot, aes(x=Month))+
  labs(x="Time", y= "Attendance rate", 
       #title = "glmsexageSIMDdaytypehourprediction"
  )+
  geom_line(aes(y= rates), color = "black")+
  geom_line(aes(y=glmsexageSIMDdaytypehourprediction), color = "purple")
save_plot("Output/glmsexageSIMDdaytypehourprediction.svg", fig = glmsexageSIMDdaytypehourprediction, width = 5, height = 5)

glmsexageSIMDdaytypehourHBprediction <- ggplot(data=sexagesimddayhourdepttypeHB_proportionsglmplot, aes(x=Month))+
  labs(x="Time", y= "Attendance rate", 
       #title = "glmsexageSIMDdaytypehourHBprediction"
  )+
  geom_line(aes(y= rates), color = "black")+
  geom_line(aes(y=glmsexageSIMDdaytypehourHBprediction), color = "brown")
save_plot("Output/glmsexageSIMDdaytypehourHBprediction.svg", fig = glmsexageSIMDdaytypehourHBprediction, width = 5, height = 5)

##FOR SOME REASON THE BLACK LINE ON THIS DOESN'T SHOW CLEARLY, THOUGH IT SHOWS IF RAN ALONE
glmsexageSIMDdaytypehourHBmonthprediction <- ggplot(data=sexagesimddayhourdepttypeHBmonth_proportionsglmplot, aes(x=Month))+
  labs(x="Time", y= "Attendance rate", 
       #title = "glmsexageSIMDdaytypehourHBmonthprediction"
  )+
  geom_line(aes(y= rates), color = "black")+ 
  geom_line(aes(y=glmsexageSIMDdaytypehourHBmonthprediction), color = "salmon")
save_plot("Output/glmsexageSIMDdaytypehourHBmonthprediction.svg", fig = glmsexageSIMDdaytypehourHBmonthprediction, width = 5, height = 5)

#using updated month
glmsexageSIMDdaytypehourHBupdatedmonthprediction <- ggplot(data=sexagesimddayhourdepttypeHBupdatedmonth_proportionsglmplot, aes(x=Month))+
  labs(x="Time", y= "Attendance rate", 
       #title = "glmsexageSIMDdaytypehourHBupdatedmonthprediction"
  )+
  geom_line(aes(y= rates), color = "black")+ 
  geom_line(aes(y=glmsexageSIMDdaytypehourHBupdatedmonthprediction), color = "salmon")
save_plot("Output/glmsexageSIMDdaytypehourHBupdatedmonthprediction.svg", fig = glmsexageSIMDdaytypehourHBupdatedmonthprediction, width = 5, height = 5)


glmsexageSIMDdaytypehourHBTimeprediction <- ggplot(data=sexagesimddayhourdepttypeHBTime_proportionsglmplot, aes(x=Month))+
  labs(x="Time", y= "Attendance rate", 
       #title = "glmsexageSIMDdaytypehourHBTimeprediction"
  )+
  geom_line(aes(y= rates), color = "black")+ 
  geom_line(aes(y=glmsexageSIMDdaytypehourHBTimeprediction), color = "pink")
save_plot("Output/glmsexageSIMDdaytypehourHBTimeprediction.svg", fig = glmsexageSIMDdaytypehourHBTimeprediction, width = 5, height = 5)


#Forecasting

#Chris said doing this doesn't make sense because it is me assuming that the whole population only consist of males (for example)
#Male= 1, TwentyfivetoThirtynine = 1, SIMDTwo = 1, Saturday = 1, Ninetotenam = 1, ED = 1, NHSOrkney = 1
dataforecast1<- data.frame(Male =1, Female =0, Under18 =0, EighteentoTwentyfour =0, TwentyfivetoThirtynine =1, FortytoSixtyfour =0,  SixtyfivetoSeventyfour =0, SIMDTwo =1, SIMDThree =0, SIMDFour =0, SIMDFive =0, Tuesday =0, Wednesday =0, Thursday =0, Friday =0, Saturday =1, Sunday =0, ED =1, Midnighttoone =0, Twotothreeam =0, Threetofouram =0, Fourtofiveam =0, Fivetosixam =0, Sixtosevenam =0, Seventoeightam =0, Eighttonineam =0, Ninetotenam =1, Tentoelevenam =0, Eleventonoon =0, Noontoonepm =0, Onetotwopm =0, Twotothreepm =0, Threetofourpm =0, Fourtofivepm =0, Fivetosixpm =0, Sixtosevenpm =0, Seventoeightpm =0, Eighttoninepm =0, Ninetotenpm =0, Tentoelevenpm =0, Eleventomidnight =0, NHSBorders =0, NHSFife =0, NHSShetland =0, NHSLanarkshire =0, NHSDumfriesandGalloway =0, NHSForthValley =0, NHSGrampian =0, NHSWesternIsles =0, NHSOrkney =1, NHSTayside =0, NHSGreaterGlasgowandClyde =0, NHSHighland =0, NHSLothian =0)
predict(glm_sexageSIMDdaytypehourHB, dataforecast1, type="response")
#error SIMDOne not found
#result returned = 13927.46

#Chris suggested doing forecasting for 2 scenarios eg Orkney and a deprived area

glm_sexageSIMDdaytypehourHBmonthTime

dataforecast1<- data.frame(Male =1, Female =0, Under18 =0, EighteentoTwentyfour =0, TwentyfivetoThirtynine =1, FortytoSixtyfour =0,  SixtyfivetoSeventyfour =0, SIMDTwo =1, SIMDThree =0, SIMDFour =0, SIMDFive =0, Tuesday =0, Wednesday =0, Thursday =0, Friday =0, Saturday =1, Sunday =0, ED =1, Midnighttoone =0, Twotothreeam =0, Threetofouram =0, Fourtofiveam =0, Fivetosixam =0, Sixtosevenam =0, Seventoeightam =0, Eighttonineam =0, Ninetotenam =1, Tentoelevenam =0, Eleventonoon =0, Noontoonepm =0, Onetotwopm =0, Twotothreepm =0, Threetofourpm =0, Fourtofivepm =0, Fivetosixpm =0, Sixtosevenpm =0, Seventoeightpm =0, Eighttoninepm =0, Ninetotenpm =0, Tentoelevenpm =0, Eleventomidnight =0, NHSBorders =0, NHSFife =0, NHSShetland =0, NHSLanarkshire =0, NHSDumfriesandGalloway =0, NHSForthValley =0, NHSGrampian =0, NHSWesternIsles =0, NHSOrkney =1, NHSTayside =0, NHSGreaterGlasgowandClyde =0, NHSHighland =0, NHSLothian =0)
predict(glm_sexageSIMDdaytypehourHB, dataforecast1, type="response")
#error SIMDOne not found

#using month august
dataforecast2<- data.frame(Male =0.4892402, Female =0.4831486, Under18 =0.1824, EighteentoTwentyfour =0.08412196, TwentyfivetoThirtynine =0.1833776, FortytoSixtyfour =0.2799453,  SixtyfivetoSeventyfour =0.10060757, SIMDOne =0.2573839,SIMDTwo =0.2128418, SIMDThree =0.1744787, SIMDFour =0.1546053, SIMDFive =0.1318929, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =0.8473706, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0.01947048, NHSFife =0.05778455, NHSShetland =0.005656719, NHSLanarkshire =0.1265399, NHSDumfriesandGalloway =0.03022991, NHSForthValley =0.0536222, NHSGrampian =0.07932528, NHSWesternIsles =0.005678588, NHSOrkney =0.004278987, NHSTayside =0.05844061, NHSGreaterGlasgowandClyde =0.2583648, NHSHighland =0.06884285, NHSLothian =0.1691621, Jan= 0, Feb= 0, March = 0, April =0, May =0 , June =0, July=0, August=1, Sept=0, Oct = 0, Nov=0, Dec=0)
predict(glm_sexageSIMDdaytypehourHBmonth, dataforecast2, type="response")
#418.8494

#Creating the prediction for Orkney from August onwards
#Getting the proportions for Orkney in July 2023 for sex, age, deprivation status - did this on Excel

#using month and July 2023 proportions for all for month and time (should have only used month tbh), set ED as 1 because Orkney only has ED
OrkneyAug23<- data.frame(Male =0.5, Female =0.5, Under18 =0.1824, EighteentoTwentyfour =0.08412196, TwentyfivetoThirtynine =0.1833776, FortytoSixtyfour =0.2799453,  SixtyfivetoSeventyfour =0.10060757, SIMDOne =0.2573839,SIMDTwo =0.2128418, SIMDThree =0.1744787, SIMDFour =0.1546053, SIMDFive =0.1318929, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =1, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0, NHSFife =0, NHSShetland =0, NHSLanarkshire =0, NHSDumfriesandGalloway =0, NHSForthValley =0, NHSGrampian =0, NHSWesternIsles =0, NHSOrkney =1, NHSTayside =0, NHSGreaterGlasgowandClyde =0, NHSHighland =0, NHSLothian =0, Jan= 0, Feb= 0, March = 0, April =0, May =0 , June =0, July=0, August=1, Sept=0, Oct = 0, Nov=0, Dec=0)
predict(glm_sexageSIMDdaytypehourHBmonth, OrkneyAug23, type="response")
#2.220446e-16 

#using month and real orkney proportions for age, sex, simd from July 2023, set ED as 1 because Orkney only has ED
OrkneyAug23<- data.frame(Male =0.464179104, Female =0.426865672, Under18 =0.131343284, EighteentoTwentyfour =0.092537313, TwentyfivetoThirtynine =0.153731343, FortytoSixtyfour =0.26119403,  SixtyfivetoSeventyfour =0.126865672, SIMDOne =0.013432836,SIMDTwo =0.155223881, SIMDThree =0.235820896, SIMDFour =0.405970149, SIMDFive =0.068656716, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =1, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0, NHSFife =0, NHSShetland =0, NHSLanarkshire =0, NHSDumfriesandGalloway =0, NHSForthValley =0, NHSGrampian =0, NHSWesternIsles =0, NHSOrkney =1, NHSTayside =0, NHSGreaterGlasgowandClyde =0, NHSHighland =0, NHSLothian =0, Jan= 0, Feb= 0, March = 0, April =0, May =0 , June =0, July=0, August=1, Sept=0, Oct = 0, Nov=0, Dec=0)
predict(glm_sexageSIMDdaytypehourHBmonth, OrkneyAug23, type="response")
#2.220446e-16

#using real orkney proportions for age, sex, simd from July 2023, set ED as 1 because Orkney only has ED
OrkneyAug23Time<- data.frame(Male =0.464179104, Female =0.426865672, Under18 =0.131343284, EighteentoTwentyfour =0.092537313, TwentyfivetoThirtynine =0.153731343, FortytoSixtyfour =0.26119403,  SixtyfivetoSeventyfour =0.126865672, SIMDOne =0.013432836,SIMDTwo =0.155223881, SIMDThree =0.235820896, SIMDFour =0.405970149, SIMDFive =0.068656716, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =1, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0, NHSFife =0, NHSShetland =0, NHSLanarkshire =0, NHSDumfriesandGalloway =0, NHSForthValley =0, NHSGrampian =0, NHSWesternIsles =0, NHSOrkney =1, NHSTayside =0, NHSGreaterGlasgowandClyde =0, NHSHighland =0, NHSLothian =0, Time=2039)
predict(glm_sexageSIMDdaytypehourHBTime, OrkneyAug23Time, type="response")
#2900422 
#Estimated 2023 whole population is 5507479, Orkney population in 2023 is 22731 (see ScotUpdatespopulation2018to2023glmplot and HBpopulation_estimate_2023_HBnames)
#so rate in Orkney for Aug is (22731/2900422) = 0.0078371, = 0.0078371 x 22731 =178.1459
#when just do Orkney without time= 30670.77

#using real orkney proportions for age, sex, simd from July 2023, set ED as 1 because Orkney only has ED
OrkneySept23Time<- data.frame(Male =0.464179104, Female =0.426865672, Under18 =0.131343284, EighteentoTwentyfour =0.092537313, TwentyfivetoThirtynine =0.153731343, FortytoSixtyfour =0.26119403,  SixtyfivetoSeventyfour =0.126865672, SIMDOne =0.013432836,SIMDTwo =0.155223881, SIMDThree =0.235820896, SIMDFour =0.405970149, SIMDFive =0.068656716, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =1, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0, NHSFife =0, NHSShetland =0, NHSLanarkshire =0, NHSDumfriesandGalloway =0, NHSForthValley =0, NHSGrampian =0, NHSWesternIsles =0, NHSOrkney =1, NHSTayside =0, NHSGreaterGlasgowandClyde =0, NHSHighland =0, NHSLothian =0, Time=2070)
predict(glm_sexageSIMDdaytypehourHBTime, OrkneySept23Time, type="response")
#2910265
#so rate in Orkney for Sept is (22731/2910265) = 0.0078106, 0.0078106 x 22731 = 177.54

#using real orkney proportions for age, sex, simd from July 2023, set ED as 1 because Orkney only has ED
OrkneyOct23Time<- data.frame(Male =0.464179104, Female =0.426865672, Under18 =0.131343284, EighteentoTwentyfour =0.092537313, TwentyfivetoThirtynine =0.153731343, FortytoSixtyfour =0.26119403,  SixtyfivetoSeventyfour =0.126865672, SIMDOne =0.013432836,SIMDTwo =0.155223881, SIMDThree =0.235820896, SIMDFour =0.405970149, SIMDFive =0.068656716, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =1, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0, NHSFife =0, NHSShetland =0, NHSLanarkshire =0, NHSDumfriesandGalloway =0, NHSForthValley =0, NHSGrampian =0, NHSWesternIsles =0, NHSOrkney =1, NHSTayside =0, NHSGreaterGlasgowandClyde =0, NHSHighland =0, NHSLothian =0, Time=2100)
predict(glm_sexageSIMDdaytypehourHBTime, OrkneyOct23Time, type="response")
#2919822
#so rate in Orkney for Oct is (22731/2919822) = 0.0077851, 0.0077851 x 22731 = 176.962

#29 April 2024 - Chris advised that the predictions is using all the proportions to calculate the total Scottish attendance,
#and then after that to multiple by the proportion for the required location (eg Orkney, or SIMDOne specifically for Orkney), 
#Calculating prediction using time and and including proportions for all
ScotAug23 <-data.frame(Male =0.4892402, Female =0.4831486, Under18 =0.1824233, EighteentoTwentyfour =0.08412196, TwentyfivetoThirtynine =0.1833776, FortytoSixtyfour =0.2799453,  SixtyfivetoSeventyfour =0.10060757, SIMDOne =0.2573839,SIMDTwo =0.2128418, SIMDThree =0.1744787, SIMDFour =0.1546053, SIMDFive =0.1318929, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =0.8473706, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0.01947048, NHSFife =0.05778455, NHSShetland =0.005656719, NHSLanarkshire =0.1265399, NHSDumfriesandGalloway =0.03022991, NHSForthValley =0.0536222, NHSGrampian =0.07932528, NHSWesternIsles =0.005678588, NHSOrkney =0.004278987, NHSTayside =0.05844061, NHSGreaterGlasgowandClyde =0.2583648, NHSHighland =0.06884285, NHSLothian =0.1691621, Time=2039)
predict(glm_sexageSIMDdaytypehourHBTime, ScotAug23, type="response")
#Total attendances for Scotland in August = 137755.2  (real Orkney Oct 23 reading from PHS is 677)
#Attendance in Orkney (Orkney proportion of total is 0.004278987)
#137755.2 x 0.004278987=589.45
#Attendance in Glasgow (Glasgow proportion of total is 0.2583648)(real Glasgow Aug 23 reading from PHS is 35522)
#137755.2 x 0.2583648=35,591.09

ScotSept23 <-data.frame(Male =0.4892402, Female =0.4831486, Under18 =0.1824233, EighteentoTwentyfour =0.08412196, TwentyfivetoThirtynine =0.1833776, FortytoSixtyfour =0.2799453,  SixtyfivetoSeventyfour =0.10060757, SIMDOne =0.2573839,SIMDTwo =0.2128418, SIMDThree =0.1744787, SIMDFour =0.1546053, SIMDFive =0.1318929, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =0.8473706, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0.01947048, NHSFife =0.05778455, NHSShetland =0.005656719, NHSLanarkshire =0.1265399, NHSDumfriesandGalloway =0.03022991, NHSForthValley =0.0536222, NHSGrampian =0.07932528, NHSWesternIsles =0.005678588, NHSOrkney =0.004278987, NHSTayside =0.05844061, NHSGreaterGlasgowandClyde =0.2583648, NHSHighland =0.06884285, NHSLothian =0.1691621, Time=2070)
predict(glm_sexageSIMDdaytypehourHBTime, ScotSept23, type="response")
#Total attendances for Scotland in Sept = 138222.7 (real Orkney Sept 23 reading from PHS is 587)
#Attendance in Orkney (Orkney proportion of total is 0.004278987)
#138222.7 x 0.004278987=591.45
#Attendance in Glasgow (Glasgow proportion of total is 0.2583648)  (real Glasgow Sept 23 reading from PHS is 34999)
#138222.7 x 0.2583648=35,711.88

ScotOct23 <-data.frame(Male =0.4892402, Female =0.4831486, Under18 =0.1824233, EighteentoTwentyfour =0.08412196, TwentyfivetoThirtynine =0.1833776, FortytoSixtyfour =0.2799453,  SixtyfivetoSeventyfour =0.10060757, SIMDOne =0.2573839,SIMDTwo =0.2128418, SIMDThree =0.1744787, SIMDFour =0.1546053, SIMDFive =0.1318929, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =0.8473706, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0.01947048, NHSFife =0.05778455, NHSShetland =0.005656719, NHSLanarkshire =0.1265399, NHSDumfriesandGalloway =0.03022991, NHSForthValley =0.0536222, NHSGrampian =0.07932528, NHSWesternIsles =0.005678588, NHSOrkney =0.004278987, NHSTayside =0.05844061, NHSGreaterGlasgowandClyde =0.2583648, NHSHighland =0.06884285, NHSLothian =0.1691621, Time=2100)
predict(glm_sexageSIMDdaytypehourHBTime, ScotOct23, type="response")
#Total attendances for Scotland in Oct = 138676.6 (real Orkney Oct 23 reading from PHS is 540)
#Attendance in Orkney (Orkney proportion of total is 0.004278987)
#138676.6 x 0.004278987=593.39
#Attendance in Glasgow (Glasgow proportion of total is 0.2583648) (real Glasgow Oct 23 reading from PHS is 34845)
#138676.6 x 0.2583648=35,829.15

ScotNov23 <-data.frame(Male =0.4892402, Female =0.4831486, Under18 =0.1824233, EighteentoTwentyfour =0.08412196, TwentyfivetoThirtynine =0.1833776, FortytoSixtyfour =0.2799453,  SixtyfivetoSeventyfour =0.10060757, SIMDOne =0.2573839,SIMDTwo =0.2128418, SIMDThree =0.1744787, SIMDFour =0.1546053, SIMDFive =0.1318929, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =0.8473706, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0.01947048, NHSFife =0.05778455, NHSShetland =0.005656719, NHSLanarkshire =0.1265399, NHSDumfriesandGalloway =0.03022991, NHSForthValley =0.0536222, NHSGrampian =0.07932528, NHSWesternIsles =0.005678588, NHSOrkney =0.004278987, NHSTayside =0.05844061, NHSGreaterGlasgowandClyde =0.2583648, NHSHighland =0.06884285, NHSLothian =0.1691621, Time=2131)
predict(glm_sexageSIMDdaytypehourHBTime, ScotNov23, type="response")
#Total attendances for Scotland in Nov = 139147.3 (real Orkney Nov 23 reading from PHS is ....)
#Attendance in Orkney (Orkney proportion of total is 0.004278987)
#139147.3 x 0.004278987=595.41
#Attendance in Glasgow (Glasgow proportion of total is 0.2583648) (real Glasgow Nov 23 reading from PHS is )
#139147.3 x 0.2583648=35,950.76

ScotDec23 <-data.frame(Male =0.4892402, Female =0.4831486, Under18 =0.1824233, EighteentoTwentyfour =0.08412196, TwentyfivetoThirtynine =0.1833776, FortytoSixtyfour =0.2799453,  SixtyfivetoSeventyfour =0.10060757, SIMDOne =0.2573839,SIMDTwo =0.2128418, SIMDThree =0.1744787, SIMDFour =0.1546053, SIMDFive =0.1318929, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =0.8473706, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0.01947048, NHSFife =0.05778455, NHSShetland =0.005656719, NHSLanarkshire =0.1265399, NHSDumfriesandGalloway =0.03022991, NHSForthValley =0.0536222, NHSGrampian =0.07932528, NHSWesternIsles =0.005678588, NHSOrkney =0.004278987, NHSTayside =0.05844061, NHSGreaterGlasgowandClyde =0.2583648, NHSHighland =0.06884285, NHSLothian =0.1691621, Time=2161)
predict(glm_sexageSIMDdaytypehourHBTime, ScotDec23, type="response")
#Total attendances for Scotland in Dec = 139604.2 (real Orkney Dec 23 reading from PHS is ....)
#Attendance in Orkney (Orkney proportion of total is 0.004278987)
#139604.2 x 0.004278987=597.36
#Attendance in Glasgow (Glasgow proportion of total is 0.2583648) (real Glasgow Dec 23 reading from PHS is )
#139604.2 x 0.2583648=36,068.81

ScotJan24 <-data.frame(Male =0.4892402, Female =0.4831486, Under18 =0.1824233, EighteentoTwentyfour =0.08412196, TwentyfivetoThirtynine =0.1833776, FortytoSixtyfour =0.2799453,  SixtyfivetoSeventyfour =0.10060757, SIMDOne =0.2573839,SIMDTwo =0.2128418, SIMDThree =0.1744787, SIMDFour =0.1546053, SIMDFive =0.1318929, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =0.8473706, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0.01947048, NHSFife =0.05778455, NHSShetland =0.005656719, NHSLanarkshire =0.1265399, NHSDumfriesandGalloway =0.03022991, NHSForthValley =0.0536222, NHSGrampian =0.07932528, NHSWesternIsles =0.005678588, NHSOrkney =0.004278987, NHSTayside =0.05844061, NHSGreaterGlasgowandClyde =0.2583648, NHSHighland =0.06884285, NHSLothian =0.1691621, Time=2192)
predict(glm_sexageSIMDdaytypehourHBTime, ScotJan24, type="response")
#Total attendances for Scotland in Jan = 140078 (real Orkney Jan 24 reading from PHS is ....)
#Attendance in Orkney (Orkney proportion of total is 0.004278987)
#140078 x 0.004278987=599.39
#Attendance in Glasgow (Glasgow proportion of total is 0.2583648) (real Glasgow Jan 24 reading from PHS is )
#140078 x 0.2583648=36,191.22

#tried using updated month instead of Time to see how the results differ
ScotAug23updatedmonth <- data.frame(Male =0.4892402, Female =0.4831486, Under18 =0.1824233, EighteentoTwentyfour =0.08412196, TwentyfivetoThirtynine =0.1833776, FortytoSixtyfour =0.2799453,  SixtyfivetoSeventyfour =0.10060757, SIMDOne =0.2573839,SIMDTwo =0.2128418, SIMDThree =0.1744787, SIMDFour =0.1546053, SIMDFive =0.1318929, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =0.8473706, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0.01947048, NHSFife =0.05778455, NHSShetland =0.005656719, NHSLanarkshire =0.1265399, NHSDumfriesandGalloway =0.03022991, NHSForthValley =0.0536222, NHSGrampian =0.07932528, NHSWesternIsles =0.005678588, NHSOrkney =0.004278987, NHSTayside =0.05844061, NHSGreaterGlasgowandClyde =0.2583648, NHSHighland =0.06884285, NHSLothian =0.1691621, Feb= 0, March=0, April=0, May=0, June=0, July=0, August=1, Sept=0, Oct=0, Nov=0, Dec=0)
predict(glm_sexageSIMDdaytypehourHBupdatedmonth, ScotAug23updatedmonth, type="response")
#108639.6
#Attendance in Orkney (Orkney proportion of total is 0.004278987)
#108639.6 x 0.004278987=464.86
#Attendance in Glasgow (Glasgow proportion of total is 0.2583648)
#108639.6 x 0.2583648=27,556.07

ScotSept23updatedmonth <- data.frame(Male =0.4892402, Female =0.4831486, Under18 =0.1824233, EighteentoTwentyfour =0.08412196, TwentyfivetoThirtynine =0.1833776, FortytoSixtyfour =0.2799453,  SixtyfivetoSeventyfour =0.10060757, SIMDOne =0.2573839,SIMDTwo =0.2128418, SIMDThree =0.1744787, SIMDFour =0.1546053, SIMDFive =0.1318929, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =0.8473706, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0.01947048, NHSFife =0.05778455, NHSShetland =0.005656719, NHSLanarkshire =0.1265399, NHSDumfriesandGalloway =0.03022991, NHSForthValley =0.0536222, NHSGrampian =0.07932528, NHSWesternIsles =0.005678588, NHSOrkney =0.004278987, NHSTayside =0.05844061, NHSGreaterGlasgowandClyde =0.2583648, NHSHighland =0.06884285, NHSLothian =0.1691621, Feb= 0, March=0, April=0, May=0, June=0, July=0, August=0, Sept=1, Oct=0, Nov=0, Dec=0)
predict(glm_sexageSIMDdaytypehourHBupdatedmonth, ScotSept23updatedmonth, type="response")
#117194.9
#Attendance in Orkney (Orkney proportion of total is 0.004278987)
#117194.9 x 0.004278987=501.47
#Attendance in Glasgow (Glasgow proportion of total is 0.2583648)
#117194.9 x 0.2583648=30,278.80

ScotOct23updatedmonth <- data.frame(Male =0.4892402, Female =0.4831486, Under18 =0.1824233, EighteentoTwentyfour =0.08412196, TwentyfivetoThirtynine =0.1833776, FortytoSixtyfour =0.2799453,  SixtyfivetoSeventyfour =0.10060757, SIMDOne =0.2573839,SIMDTwo =0.2128418, SIMDThree =0.1744787, SIMDFour =0.1546053, SIMDFive =0.1318929, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =0.8473706, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0.01947048, NHSFife =0.05778455, NHSShetland =0.005656719, NHSLanarkshire =0.1265399, NHSDumfriesandGalloway =0.03022991, NHSForthValley =0.0536222, NHSGrampian =0.07932528, NHSWesternIsles =0.005678588, NHSOrkney =0.004278987, NHSTayside =0.05844061, NHSGreaterGlasgowandClyde =0.2583648, NHSHighland =0.06884285, NHSLothian =0.1691621, Feb= 0, March=0, April=0, May=0, June=0, July=0, August=0, Sept=0, Oct=1, Nov=0, Dec=0)
predict(glm_sexageSIMDdaytypehourHBupdatedmonth, ScotOct23updatedmonth, type="response")
#127143.2
#Attendance in Orkney (Orkney proportion of total is 0.004278987)
#127143.2 x 0.004278987=544.04
#Attendance in Glasgow (Glasgow proportion of total is 0.2583648)
#127143.2 x 0.2583648=32,849.33

ScotNov23updatedmonth <- data.frame(Male =0.4892402, Female =0.4831486, Under18 =0.1824233, EighteentoTwentyfour =0.08412196, TwentyfivetoThirtynine =0.1833776, FortytoSixtyfour =0.2799453,  SixtyfivetoSeventyfour =0.10060757, SIMDOne =0.2573839,SIMDTwo =0.2128418, SIMDThree =0.1744787, SIMDFour =0.1546053, SIMDFive =0.1318929, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =0.8473706, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0.01947048, NHSFife =0.05778455, NHSShetland =0.005656719, NHSLanarkshire =0.1265399, NHSDumfriesandGalloway =0.03022991, NHSForthValley =0.0536222, NHSGrampian =0.07932528, NHSWesternIsles =0.005678588, NHSOrkney =0.004278987, NHSTayside =0.05844061, NHSGreaterGlasgowandClyde =0.2583648, NHSHighland =0.06884285, NHSLothian =0.1691621, Feb= 0, March=0, April=0, May=0, June=0, July=0, August=0, Sept=0, Oct=0, Nov=1, Dec=0)
predict(glm_sexageSIMDdaytypehourHBupdatedmonth, ScotNov23updatedmonth, type="response")
#99116.18
#Attendance in Orkney (Orkney proportion of total is 0.004278987)
#99116.18 x 0.004278987=424.12
#Attendance in Glasgow (Glasgow proportion of total is 0.2583648)
#99116.18 x 0.2583648=25,608.12

ScotDec23updatedmonth <- data.frame(Male =0.4892402, Female =0.4831486, Under18 =0.1824233, EighteentoTwentyfour =0.08412196, TwentyfivetoThirtynine =0.1833776, FortytoSixtyfour =0.2799453,  SixtyfivetoSeventyfour =0.10060757, SIMDOne =0.2573839,SIMDTwo =0.2128418, SIMDThree =0.1744787, SIMDFour =0.1546053, SIMDFive =0.1318929, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =0.8473706, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0.01947048, NHSFife =0.05778455, NHSShetland =0.005656719, NHSLanarkshire =0.1265399, NHSDumfriesandGalloway =0.03022991, NHSForthValley =0.0536222, NHSGrampian =0.07932528, NHSWesternIsles =0.005678588, NHSOrkney =0.004278987, NHSTayside =0.05844061, NHSGreaterGlasgowandClyde =0.2583648, NHSHighland =0.06884285, NHSLothian =0.1691621, Feb= 0, March=0, April=0, May=0, June=0, July=0, August=0, Sept=0, Oct=0, Nov=0, Dec=1)
predict(glm_sexageSIMDdaytypehourHBupdatedmonth, ScotDec23updatedmonth, type="response")
#71488.51
#Attendance in Orkney (Orkney proportion of total is 0.004278987)
#71488.51 x 0.004278987=305.90
#Attendance in Glasgow (Glasgow proportion of total is 0.2583648)
#71488.51 x 0.2583648=18,470.11

ScotJan24updatedmonth <- data.frame(Male =0.4892402, Female =0.4831486, Under18 =0.1824233, EighteentoTwentyfour =0.08412196, TwentyfivetoThirtynine =0.1833776, FortytoSixtyfour =0.2799453,  SixtyfivetoSeventyfour =0.10060757, SIMDOne =0.2573839,SIMDTwo =0.2128418, SIMDThree =0.1744787, SIMDFour =0.1546053, SIMDFive =0.1318929, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =0.8473706, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0.01947048, NHSFife =0.05778455, NHSShetland =0.005656719, NHSLanarkshire =0.1265399, NHSDumfriesandGalloway =0.03022991, NHSForthValley =0.0536222, NHSGrampian =0.07932528, NHSWesternIsles =0.005678588, NHSOrkney =0.004278987, NHSTayside =0.05844061, NHSGreaterGlasgowandClyde =0.2583648, NHSHighland =0.06884285, NHSLothian =0.1691621, Feb= 0, March=0, April=0, May=0, June=0, July=0, August=0, Sept=0, Oct=0, Nov=0, Dec=0)
predict(glm_sexageSIMDdaytypehourHBupdatedmonth, ScotJan24updatedmonth, type="response")
#92906.21 
#Attendance in Orkney (Orkney proportion of total is 0.004278987)
#92906.21  x 0.004278987=397.54
#Attendance in Glasgow (Glasgow proportion of total is 0.2583648)
#92906.21  x 0.2583648=24,003.69

#Orkney attendance rates 2018-2023
#adding new Month column based on assumption that end of one month is first day of next month
Orkney_rates2018to2023 <- ae_byboard2018to2023 %>% filter(NHSBoardName=="NHS Orkney") %>% mutate(Month = ae_byboard2018to2023_proportionsnewdate$Month)
#removing the Month ending dateand year column
Orkney_rates2018to2023 <- Orkney_rates2018to2023 %>% select(-MonthEndingDate, -Year)
#Adding a new column for year
Orkney_rates2018to2023$Year <- as.numeric(format(Orkney_rates2018to2023$Month, "%Y"))
#Getting population estimate data for 2018-2023
Orkney_Updatespopulation2018to2023 <- HBUpdatespopulation_estimate_HBname %>% filter(HBName=="NHS Orkney")
Orkney_Updatespopulation2018to2023 <- Orkney_Updatespopulation2018to2023 %>% filter(Year=="2018"|Year=="2019"|Year=="2020"|Year=="2021"|Year=="2022"|Year=="2023")
#combining the dataframe for attendance and Orkney population in 2018-2023
Orkney_rates2018to2023 <- merge(Orkney_rates2018to2023, Orkney_Updatespopulation2018to2023, by=c("Year"))
#adding a new column containing the attendance rate
Orkney_rates2018to2023 <- Orkney_rates2018to2023 %>% mutate(attendancerate = Attendances/AllAges)
#removing unnecessary columns
Orkney_rates2018to2023 <- Orkney_rates2018to2023 %>% select(-HBName, -HB, -Sex, -Year)
#creating a dataframe for the attendance rate predicted for Aug 2023 and Sept 2023 using model based on time
OrkneyAug23Time_attendancerate <- c(NHSBoardName = "NHS Orkney", Attendances = 589, Month = "2023-08-01", AllAges= 22731, attendancerate=0.0259118)
OrkneySept23Time_attendancerate <- c(NHSBoardName = "NHS Orkney", Attendances = 591, Month = "2023-09-01", AllAges= 22731, attendancerate=0.0259997)
OrkneyOct23Time_attendancerate <- c(NHSBoardName = "NHS Orkney", Attendances = 593, Month = "2023-10-01", AllAges= 22731, attendancerate=0.0260877)
OrkneyNov23Time_attendancerate <- c(NHSBoardName = "NHS Orkney", Attendances = 595, Month = "2023-11-01", AllAges= 22731, attendancerate=0.0261757)
OrkneyDec23Time_attendancerate <- c(NHSBoardName = "NHS Orkney", Attendances = 597, Month = "2023-12-01", AllAges= 22731, attendancerate=0.0262637)
OrkneyJan24Time_attendancerate <- c(NHSBoardName = "NHS Orkney", Attendances = 599, Month = "2024-01-01", AllAges= 22731, attendancerate=0.0263517)

#Combining the dataframe containing actual rates with the prediction for Sept and Aug 2023 
Orkney_rates2018to2023 <- rbind(Orkney_rates2018to2023, OrkneyAug23Time_attendancerate)
Orkney_rates2018to2023 <- rbind(Orkney_rates2018to2023, OrkneySept23Time_attendancerate)
Orkney_rates2018to2023 <- rbind(Orkney_rates2018to2023, OrkneyOct23Time_attendancerate)
Orkney_rates2018to2023 <- rbind(Orkney_rates2018to2023, OrkneyNov23Time_attendancerate)
Orkney_rates2018to2023 <- rbind(Orkney_rates2018to2023, OrkneyDec23Time_attendancerate)
Orkney_rates2018to2023 <- rbind(Orkney_rates2018to2023, OrkneyJan24Time_attendancerate)

str(Orkney_rates2018to2023)
#changing attendancerate to a numeric
Orkney_rates2018to2023$attendancerate <- as.numeric(Orkney_rates2018to2023$attendancerate) 

#graph of actual Orkney attendance rates followed by predicted
Orkney_predict_rates2018to2023 <- ggplot(data=Orkney_rates2018to2023, aes(x=Month, y=attendancerate))+
  geom_point()+
  geom_line()+
  labs(x="Year",
       y="Attendance rate")
save_plot("Output/Orkney_predict_rates2018to2023.svg", fig=Orkney_predict_rates2018to2023, width=14, height=12)

#trying with orkney as 1/14 - spoke with Chris no need to do 1/14
OrkneyAug23Time<- data.frame(Male =0.464179104, Female =0.426865672, Under18 =0.131343284, EighteentoTwentyfour =0.092537313, TwentyfivetoThirtynine =0.153731343, FortytoSixtyfour =0.26119403,  SixtyfivetoSeventyfour =0.126865672, SIMDOne =0.013432836,SIMDTwo =0.155223881, SIMDThree =0.235820896, SIMDFour =0.405970149, SIMDFive =0.068656716, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =1, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0, NHSFife =0, NHSShetland =0, NHSLanarkshire =0, NHSDumfriesandGalloway =0, NHSForthValley =0, NHSGrampian =0, NHSWesternIsles =0, NHSOrkney =0.0714285714285714, NHSTayside =0, NHSGreaterGlasgowandClyde =0, NHSHighland =0, NHSLothian =0, Time=2039)
predict(glm_sexageSIMDdaytypehourHBTime, OrkneyAug23Time, type="response")
#4.931601e+13 

#trying with Orkney as 1/14 and month as 1/12 - Spoke with Chris no need to do this
OrkneyAug23<- data.frame(Male =0.464179104, Female =0.426865672, Under18 =0.131343284, EighteentoTwentyfour =0.092537313, TwentyfivetoThirtynine =0.153731343, FortytoSixtyfour =0.26119403,  SixtyfivetoSeventyfour =0.126865672, SIMDOne =0.013432836,SIMDTwo =0.155223881, SIMDThree =0.235820896, SIMDFour =0.405970149, SIMDFive =0.068656716, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =1, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0, NHSFife =0, NHSShetland =0, NHSLanarkshire =0, NHSDumfriesandGalloway =0, NHSForthValley =0, NHSGrampian =0, NHSWesternIsles =0, NHSOrkney =0.0714285714285714, NHSTayside =0, NHSGreaterGlasgowandClyde =0, NHSHighland =0, NHSLothian =0, Jan= 0, Feb= 0, March = 0, April =0, May =0 , June =0, July=0, August=0.083333333
                         , Sept=0, Oct = 0, Nov=0, Dec=0)
predict(glm_sexageSIMDdaytypehourHBmonth, OrkneyAug23, type="response")
#2.220446e-16



#Calculating sex, age, simd proportions for Orkney - realised I didn't need to do this in the end

#SIMD proportions for Orkney
#Orkney_glmproportions <- deprivation %>% 
#  filter(HealthBoard=="NHS Orkney")

#changing month from character to a date
#ymd(Orkney_glmproportions$Month)
#str(Orkney_glmproportions)

#adding month and year column
#Orkney_glmproportions$yearnumeric <- format(as.Date(Orkney_glmproportions$Month, format="%Y/%m/%d"),"%Y")
#Orkney_glmproportions$monthnumeric <- format(as.Date(Orkney_glmproportions$Month, format="%Y/%m/%d"),"%m")
#calculate total monthly attendances and then proportion
#Orkney_glmproportions %>% filter(yearnumeric=="2023", monthnumeric=="07") %>% 
#  summarise(Attendances=sum(Attendances))
#670
#Adding a new column for Orkney SIMD proportions in July 2023
#Orkney_glmproportions_July23simd <- Orkney_glmproportions %>%
#  filter(yearnumeric=="2023", monthnumeric=="07") %>% 
#  mutate(SIMDprop=Attendances/670)

#Calculating proportions for Orkney in July 23 for age
#Orkney_glmproportions_July23age <- agegroupHB %>%  
#  filter(HealthBoard=="NHS Orkney")

#changing from character to date
#Tried this didnt work: ymd(Orkney_glmproportions_July23age$Month)
#Orkney_glmproportions_July23age$Month<-as.Date(Orkney_glmproportions_July23age$Month)
#str(Orkney_glmproportions_July23age)
#adding columns for month and year
#Orkney_glmproportions_July23age$yearnumeric <- format(as.Date(Orkney_glmproportions_July23age$Month, format="%Y/%m/%d"), "%Y")
#Orkney_glmproportions_July23age$monthnumeric <- format(as.Date(Orkney_glmproportions_July23age$Month, format="%Y/%m/%d"), "%m")

#calculate total monthly attendances and then proportion
#Orkney_glmproportions_July23age<- Orkney_glmproportions_July23age %>% filter(yearnumeric=="2023", monthnumeric=="07")
#Orkney_glmproportions_July23age %>% 
#  summarise(Attendances=sum(Attendances))
#670
#Adding a new column for Orkney age proportions in July 2023
#Orkney_glmproportions_July23age <- Orkney_glmproportions_July23age %>%
#  mutate(ageprop=Attendances/670)

#Calculating proportions for Orkney in July 23 for sex

#whoattends_sexHB <- read_excel("Rawdata/2023-09-05-whoattends-sex.xlsx", 
#                               sheet = "HealthBoard")

#Orkney_glmproportions_July23sex <- whoattends_sexHB %>%  
#  filter(HealthBoard=="NHS Orkney")

#changing from character to date
#Orkney_glmproportions_July23sex$Month<-as.Date(Orkney_glmproportions_July23sex$Month)
#str(Orkney_glmproportions_July23sex)
#adding columns for month and year
#Orkney_glmproportions_July23sex$yearnumeric <- format(as.Date(Orkney_glmproportions_July23sex$Month, format="%Y/%m/%d"), "%Y")
#Orkney_glmproportions_July23sex$monthnumeric <- format(as.Date(Orkney_glmproportions_July23sex$Month, format="%Y/%m/%d"), "%m")

#calculate total monthly attendances and then proportion
#Orkney_glmproportions_July23sex<- Orkney_glmproportions_July23sex %>% filter(yearnumeric=="2023", monthnumeric=="07")
#Orkney_glmproportions_July23sex %>% 
#  summarise(Attendances=sum(Attendances))
#670
#Adding a new column for Orkney sex proportions in July 2023
#Orkney_glmproportions_July23sex <- Orkney_glmproportions_July23sex %>%
#  mutate(sexprop=Attendances/670)

#Just for information how to delete a column
#Orkney_glmproportions <-Orkney_glmproportions %>% select(-Year)


##creating a graph for Orkney for Jan 2018- July 2023
#creating a dataframe for Orkney
ae_monthly_Orkney20182023 <- ae_monthly_totalae %>% 
  filter(between(MonthEndingDate, as.Date("2017-12-31"), as.Date("2023-06-30")),NHSBoardName=="NHS Orkney")
#aligning the dates in the Orkney dataframe by combining with the correct column of dates as per in the proportions table
ae_monthly_Orkney20182023 <- cbind(ae_monthly_Orkney20182023, Month=sexagesimddayhourdepttypeHBmonth_proportions$Month)

ae_attendance_Orkney20182023 <- ggplot(data=ae_monthly_Orkney20182023, aes(x=Month, y=NumberOfAttendancesAll))+
  geom_line()+
  labs(x="Year", 
       y="Number of A&E attendances at NHS Orkney")
save_plot("Output/aeAttendanceOrkney20182023.svg", fig=ae_attendance_Orkney20182023, width=14, height=12)

#ThirtyOneDays_Aggregated <- ThirtyOneDays_Aggregated %>%
#  mutate('fitted1' = model_1_predictions/PopSize, 'rates' = NumberOfEligibleReferrals31DayStandard/PopSize)

#ThirtyOneDays_Plot <- ThirtyOneDays_Aggregated %>% 
#  ggplot(aes(x=Date)) + 
#  labs(x = 'Time', y = 'Rate of Referrals', title = 'Number of 31 day eligible referrals in Scotland for all cancer types') +
#  geom_line(aes(y = rates), color = "black") +
#  geom_line(aes(y = fitted1), color = "red")

#ThirtyOneDays_Plot


#Glasgow

#Calculating sex, age, simd proportions for Glasgow - realised I didn't need to do this in the end

#SIMD proportions for Glasgow
#Glasgow_glmproportions <- deprivation %>% 
#  filter(HealthBoard=="NHS Greater Glasgow & Clyde")

#changing month from character to a date
#ymd(Glasgow_glmproportions$Month)
#str(Glasgow_glmproportions)

#adding month and year column
#Glasgow_glmproportions$yearnumeric <- format(as.Date(Glasgow_glmproportions$Month, format="%Y/%m/%d"),"%Y")
#Glasgow_glmproportions$monthnumeric <- format(as.Date(Glasgow_glmproportions$Month, format="%Y/%m/%d"),"%m")
#calculate total monthly attendances and then proportion
#Glasgow_glmproportions %>% filter(yearnumeric=="2023", monthnumeric=="07") %>% 
#  group_by(Month, Deprivation) %>% 
#  summarise(Attendances=sum(Attendances))
#33373
#Adding a new column for Glasgow SIMD proportions in July 2023
#Glasgow_glmproportions_July23simd <- Glasgow_glmproportions %>%
#  filter(yearnumeric=="2023", monthnumeric=="07") %>% 
#  group_by(Month, Deprivation) %>% 
#  summarise(Attendances=sum(Attendances))

#Glasgow_glmproportions_July23simd <- Glasgow_glmproportions_July23simd %>%
#  mutate(SIMDprop=Attendances/33373)

#Calculating proportions for Glasgow in July 23 for age
#Glasgow_glmproportions_July23age <- agegroupHB %>%  
#  filter(HealthBoard=="NHS Greater Glasgow & Clyde")

#changing from character to date
#Tried this didnt work: ymd(Orkney_glmproportions_July23age$Month)
#Glasgow_glmproportions_July23age$Month<-as.Date(Glasgow_glmproportions_July23age$Month)
#str(Glasgow_glmproportions_July23age)
#adding columns for month and year
#Glasgow_glmproportions_July23age$yearnumeric <- format(as.Date(Glasgow_glmproportions_July23age$Month, format="%Y/%m/%d"), "%Y")
#Glasgow_glmproportions_July23age$monthnumeric <- format(as.Date(Glasgow_glmproportions_July23age$Month, format="%Y/%m/%d"), "%m")

#calculate total monthly attendances and then proportion
#Glasgow_glmproportions_July23age<- Glasgow_glmproportions_July23age %>% filter(yearnumeric=="2023", monthnumeric=="07")%>% 
#  group_by(Month, Age) %>% 
#  summarise(Attendances=sum(Attendances))
#Glasgow_glmproportions_July23age %>% 
#  summarise(Attendances=sum(Attendances))
#33373
#Adding a new column for Orkney age proportions in July 2023
#Glasgow_glmproportions_July23age <- Glasgow_glmproportions_July23age %>%
#  mutate(ageprop=Attendances/33373)

#Calculating proportions for Glasgow in July 23 for sex

#whoattends_sexHB <- read_excel("Rawdata/2023-09-05-whoattends-sex.xlsx", 
#                               sheet = "HealthBoard")

#Glasgow_glmproportions_July23sex <- whoattends_sexHB %>%  
#  filter(HealthBoard=="NHS Greater Glasgow & Clyde")

#changing from character to date
#Glasgow_glmproportions_July23sex$Month<-as.Date(Glasgow_glmproportions_July23sex$Month)
#str(Glasgow_glmproportions_July23sex)
#adding columns for month and year
#Glasgow_glmproportions_July23sex$yearnumeric <- format(as.Date(Glasgow_glmproportions_July23sex$Month, format="%Y/%m/%d"), "%Y")
#Glasgow_glmproportions_July23sex$monthnumeric <- format(as.Date(Glasgow_glmproportions_July23sex$Month, format="%Y/%m/%d"), "%m")

#calculate total monthly attendances and then proportion
#Glasgow_glmproportions_July23sex<- Glasgow_glmproportions_July23sex %>% filter(yearnumeric=="2023", monthnumeric=="07")%>% 
#  group_by(Month, Sex) %>% 
#  summarise(Attendances=sum(Attendances))
#Glasgow_glmproportions_July23sex %>% 
#  summarise(Attendances=sum(Attendances))
#33373
#Adding a new column for Glasgow sex proportions in July 2023
#Glasgow_glmproportions_July23sex <- Glasgow_glmproportions_July23sex %>%
#  mutate(sexprop=Attendances/33373)

#Calculating proportions for Glasgow in July 23 for Dept type

#Glasgow_glmproportions_July23type <- ae_monthly_attendance %>% 
#  select(MonthEndingDate, NHSBoardName, DepartmentType, NumberOfAttendancesAll) %>% 
#  filter(between(MonthEndingDate, as.Date("2023-06-30"), as.Date("2023-06-30")),NHSBoardName=="NHS Greater Glasgow & Clyde") %>% 
#  group_by(DepartmentType) %>% 
#  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll))  
#total attendances in ED and MIU = 35443

#Glasgow_glmproportions_July23type <- Glasgow_glmproportions_July23type %>% 
#  mutate(typeprop=NumberOfAttendancesAll/35443)
#ED=0.8186384, MIU=0.1813616

##creating a graph for Glasgow for Jan 2018- July 2023
#creating a dataframe for Glasgow
ae_monthly_Glasgow20182023 <-ae_monthly_attendance %>% 
  select(MonthEndingDate, NHSBoardName, DepartmentType, NumberOfAttendancesAll) %>% 
  filter(between(MonthEndingDate, as.Date("2017-12-31"), as.Date("2023-06-30")),NHSBoardName=="NHS Greater Glasgow & Clyde") %>% 
  group_by(MonthEndingDate) %>% 
  summarise(NumberOfAttendancesAll=sum(NumberOfAttendancesAll))

ae_monthly_Glasgow20182023 <- ae_monthly_total %>% 
  filter(between(MonthEndingDate, as.Date("2017-12-31"), as.Date("2023-06-30")),NHSBoardName=="NHS Greater Glasgow & Clyde")
#aligning the dates in the Glasgow dataframe by combining with the correct column of dates as per in the proportions table
ae_monthly_Glasgow20182023 <- cbind(ae_monthly_Glasgow20182023, Month=sexagesimddayhourdepttypeHBmonth_proportions$Month)

ae_attendance_Glasgow20182023 <- ggplot(data=ae_monthly_Glasgow20182023, aes(x=Month, y=NumberOfAttendancesAll))+
  geom_line()+
  labs(x="Year", 
       y="Number of A&E attendances at NHS Greater Glasgow and Clyde")
save_plot("Output/aeAttendanceGlasgow20182023.svg", fig=ae_attendance_Glasgow20182023, width=14, height=12)

##Predictions for Glasgow is not working
#using real Glasgow proportions for age, sex, simd, type from July 2023
GlasgowAug23Time<- data.frame(Male =0.493452791, Female =0.497677763, Under18 =0.205825068, EighteentoTwentyfour =0.087945345, TwentyfivetoThirtynine =0.201719953, FortytoSixtyfour =0.282623678,  SixtyfivetoSeventyfour =0.090971744, SIMDOne =0.41566536,SIMDTwo =0.17439247, SIMDThree =0.12435202, SIMDFour =0.11266593, SIMDFive =0.12105594, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =1, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0, NHSFife =0, NHSShetland =0, NHSLanarkshire =0, NHSDumfriesandGalloway =0, NHSForthValley =0, NHSGrampian =0, NHSWesternIsles =0, NHSOrkney =0, NHSTayside =0, NHSGreaterGlasgowandClyde =1, NHSHighland =0, NHSLothian =0, Time=2039)
predict(glm_sexageSIMDdaytypehourHBTime, GlasgowAug23Time, type="response")
#208935.9 
#Estimated 2023 whole population is 5507479, Glasgow population in 2023 is 1185040 (see ScotUpdatespopulation2018to2023glmplot and HBpopulation_estimate_2023_HBnames)
#so rate in Glasgow for Aug is (1185040/208935) =5.6718 , =  x 1185040 =178.1459
#(208935/1185040) =0.176, 

#using real orkney proportions for age, sex, simd from July 2023, set ED as 1 because Orkney only has ED
GlasgowSept23Time<- data.frame(Male =0.493452791, Female =0.497677763, Under18 =0.205825068, EighteentoTwentyfour =0.087945345, TwentyfivetoThirtynine =0.201719953, FortytoSixtyfour =0.282623678,  SixtyfivetoSeventyfour =0.090971744, SIMDOne =0.41566536,SIMDTwo =0.17439247, SIMDThree =0.12435202, SIMDFour =0.11266593, SIMDFive =0.12105594, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =1, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0, NHSFife =0, NHSShetland =0, NHSLanarkshire =0, NHSDumfriesandGalloway =0, NHSForthValley =0, NHSGrampian =0, NHSWesternIsles =0, NHSOrkney =0, NHSTayside =0, NHSGreaterGlasgowandClyde =1, NHSHighland =0, NHSLothian =0, Time=2070)
predict(glm_sexageSIMDdaytypehourHBTime, OrkneySept23Time, type="response")
#2910265
#so rate in Orkney for Sept is (22731/2910265) = 0.0078106, 0.0078106 x 22731 = 177.54

#using real orkney proportions for age, sex, simd from July 2023, set ED as 1 because Orkney only has ED
GlasgowOct23Time<- data.frame(Male =0.493452791, Female =0.497677763, Under18 =0.205825068, EighteentoTwentyfour =0.087945345, TwentyfivetoThirtynine =0.201719953, FortytoSixtyfour =0.282623678,  SixtyfivetoSeventyfour =0.090971744, SIMDOne =0.41566536,SIMDTwo =0.17439247, SIMDThree =0.12435202, SIMDFour =0.11266593, SIMDFive =0.12105594, Tuesday =0.1456571, Wednesday =0.1449873, Thursday =0.1390299, Friday =0.1377609, Saturday =0.1347645, Sunday =0.1395234, ED =1, Midnighttoone =0.02342818, Twotothreeam =0.01517344, Threetofouram =0.01303421, Fourtofiveam =0.012867208, Fivetosixam =0.012525249, Sixtosevenam =0.0134716, Seventoeightam =0.01639814, Eighttonineam =0.03026736, Ninetotenam =0.05104735, Tentoelevenam =0.06138565, Eleventonoon =0.06703195, Noontoonepm =0.06518696, Onetotwopm =0.0656164, Twotothreepm =0.06416109, Threetofourpm =0.06517901, Fourtofivepm =0.06414518, Fivetosixpm =0.06195028, Sixtosevenpm =0.06039954, Seventoeightpm =0.05431584, Eighttoninepm =0.05063382, Ninetotenpm =0.04506704, Tentoelevenpm =0.03786999, Eleventomidnight =0.02978226, NHSBorders =0, NHSFife =0, NHSShetland =0, NHSLanarkshire =0, NHSDumfriesandGalloway =0, NHSForthValley =0, NHSGrampian =0, NHSWesternIsles =0, NHSOrkney =0, NHSTayside =0, NHSGreaterGlasgowandClyde =1, NHSHighland =0, NHSLothian =0, Time=2100)
predict(glm_sexageSIMDdaytypehourHBTime, GlasgowOct23Time, type="response")
#210333.4 
#so rate in Orkney for Oct is (22731/2919822) = 0.0077851, 0.0077851 x 22731 = 176.962

#Glasgow attendance rates 2018-2023
#adding new Month column based on assumption that end of one month is first day of next month
Glasgow_rates2018to2023 <- ae_byboard2018to2023 %>% filter(NHSBoardName=="NHS Greater Glasgow & Clyde") %>% mutate(Month = ae_byboard2018to2023_proportionsnewdate$Month)
#removing the Month ending date and year column
Glasgow_rates2018to2023 <- Glasgow_rates2018to2023 %>% select(-MonthEndingDate, -Year)
#Adding a new column for year
Glasgow_rates2018to2023$Year <- as.numeric(format(Glasgow_rates2018to2023$Month, "%Y"))
#Getting population estimate data for 2018-2023
Glasgow_Updatespopulation2018to2023 <- HBUpdatespopulation_estimate_HBname %>% filter(HBName=="NHS Greater Glasgow and Clyde")
Glasgow_Updatespopulation2018to2023 <- Glasgow_Updatespopulation2018to2023 %>% filter(Year=="2018"|Year=="2019"|Year=="2020"|Year=="2021"|Year=="2022"|Year=="2023")
#combining the dataframe for attendance and population in 2018-2023
Glasgow_rates2018to2023 <- merge(Glasgow_rates2018to2023, Glasgow_Updatespopulation2018to2023, by=c("Year"))
#adding a new column containing the attendance rate
Glasgow_rates2018to2023 <- Glasgow_rates2018to2023 %>% mutate(attendancerate = Attendances/AllAges)
#removing unnecessary columns
Glasgow_rates2018to2023 <- Glasgow_rates2018to2023 %>% select(-HBName, -HB, -Sex, -Year)
#creating a dataframe for the attendance rate predicted for Aug 2023 and Sept 2023 using  model based on time
GlasgowAug23Time_attendancerate <- c(NHSBoardName = "NHS Greater Glasgow & Clyde", Attendances = 35591, Month = "2023-08-01", AllAges= 1192485, attendancerate=0.0298461)
GlasgowSept23Time_attendancerate <- c(NHSBoardName = "NHS Greater Glasgow & Clyde", Attendances = 35712, Month = "2023-09-01", AllAges= 1192485, attendancerate=0.0299475)
GlasgowOct23Time_attendancerate <- c(NHSBoardName = "NHS Greater Glasgow & Clyde", Attendances = 35829, Month = "2023-10-01", AllAges= 1192485, attendancerate=0.0300457)
GlasgowNov23Time_attendancerate <- c(NHSBoardName = "NHS Greater Glasgow & Clyde", Attendances = 35951, Month = "2023-11-01", AllAges= 1192485, attendancerate=0.0301480)
GlasgowDec23Time_attendancerate <- c(NHSBoardName = "NHS Greater Glasgow & Clyde", Attendances = 36069, Month = "2023-12-01", AllAges= 1192485, attendancerate=0.0302469)
GlasgowJan24Time_attendancerate <- c(NHSBoardName = "NHS Greater Glasgow & Clyde", Attendances = 36191, Month = "2024-01-01", AllAges= 1192485, attendancerate=0.0303492)

#Combining the dataframe containing actual rates with the prediction for Sept and Aug 2023 
Glasgow_rates2018to2023 <- rbind(Glasgow_rates2018to2023, GlasgowAug23Time_attendancerate)
Glasgow_rates2018to2023 <- rbind(Glasgow_rates2018to2023, GlasgowSept23Time_attendancerate)
Glasgow_rates2018to2023 <- rbind(Glasgow_rates2018to2023, GlasgowOct23Time_attendancerate)
Glasgow_rates2018to2023 <- rbind(Glasgow_rates2018to2023, GlasgowNov23Time_attendancerate)
Glasgow_rates2018to2023 <- rbind(Glasgow_rates2018to2023, GlasgowDec23Time_attendancerate)
Glasgow_rates2018to2023 <- rbind(Glasgow_rates2018to2023, GlasgowJan24Time_attendancerate)

str(Glasgow_rates2018to2023)
#changing attendancerate to a numeric
Glasgow_rates2018to2023$attendancerate <- as.numeric(Glasgow_rates2018to2023$attendancerate) 

#graph of actual Glasgow attendance rates followed by predicted
Glasgow_predict_rates2018to2023 <- ggplot(data=Glasgow_rates2018to2023, aes(x=Month, y=attendancerate))+
  geom_point()+
  geom_line()+
  labs(x="Year",
       y="Attendance rate")
save_plot("Output/Glasgow_predict_rates2018to2023.svg", fig=Glasgow_predict_rates2018to2023, width=14, height=12)

#Orkney and Glasgow in the same graph for model based on time
Orkney_Glasgow_rates2018to2023 <- rbind(Glasgow_rates2018to2023, Orkney_rates2018to2023)

Orkney_Glasgow_predict_rates2018to2023 <- ggplot(data=Orkney_Glasgow_rates2018to2023, aes(x=Month, y=attendancerate, ggroup=NHSBoardName, color=NHSBoardName))+
  geom_point()+
  geom_line()+
  labs(x="Year", 
       y="Attendance rate", 
       col="NHS Board Name")
save_plot("Output/Orkney_Glasgow_predict_rates2018to2023.svg", fig=Orkney_Glasgow_predict_rates2018to2023, width=14, height=12)

Orkney_Glasgow_predict_rates2018to2023 <- ggplot(data=Orkney_Glasgow_rates2018to2023, aes(x=Month, y=attendancerate, ggroup=NHSBoardName, color=NHSBoardName))+
  geom_point()+
  geom_line()+
  geom_vline(xintercept = as.Date("2023-08-01"), color="orange", linewidth=.5)+
  annotate(geom="text", x=as.Date("2023-03-01"), y=0.0325, size=3, label="Actual")+
  annotate(geom="text", x=as.Date("2024-01-01"), y=0.0325, size=3, label="Predicted")+
  geom_segment(aes(x=as.Date("2023-06-01"), y=0.0335, xend = as.Date("2022-12-01"), yend = 0.0335), arrow=arrow(length = unit(0.3, 'cm')), color='black')+
  
  geom_segment(aes(x=as.Date("2023-10-01"), y=0.0335, xend = as.Date("2024-04-01"), yend = 0.0335), arrow=arrow(length = unit(0.3, 'cm')), color='black')+#annotate(geom="text", x=as.Date("2023-03-01"), y=0.0340, size=3, label= sprintf('\u2191'))+
  labs(x="Year", 
       y="Attendance rate", 
       col="NHS Board Name")
save_plot("Output/Orkney_Glasgow_predict_rates2018to2023vline.svg", fig=Orkney_Glasgow_predict_rates2018to2023, width=18, height=12)

#_________________________Orkney and Glasgow in the same graph for model based on month
#Orkney attendance rates 2018-2023
#adding new Month column based on assumption that end of one month is first day of next month
Orkney_rates2018to2023month <- ae_byboard2018to2023 %>% filter(NHSBoardName=="NHS Orkney") %>% mutate(Month = ae_byboard2018to2023_proportionsnewdate$Month)
#removing the Month ending dateand year column
Orkney_rates2018to2023month <- Orkney_rates2018to2023month %>% select(-MonthEndingDate, -Year)
#Adding a new column for year
Orkney_rates2018to2023month$Year <- as.numeric(format(Orkney_rates2018to2023month$Month, "%Y"))
#Getting population estimate data for 2018-2023
Orkney_Updatespopulation2018to2023month <- HBUpdatespopulation_estimate_HBname %>% filter(HBName=="NHS Orkney")
Orkney_Updatespopulation2018to2023month <- Orkney_Updatespopulation2018to2023month %>% filter(Year=="2018"|Year=="2019"|Year=="2020"|Year=="2021"|Year=="2022"|Year=="2023")
#combining the dataframe for attendance and Orkney population in 2018-2023
Orkney_rates2018to2023month <- merge(Orkney_rates2018to2023month, Orkney_Updatespopulation2018to2023month, by=c("Year"))
#adding a new column containing the attendance rate
Orkney_rates2018to2023month <- Orkney_rates2018to2023month %>% mutate(attendancerate = Attendances/AllAges)
#removing unnecessary columns
Orkney_rates2018to2023month <- Orkney_rates2018to2023month %>% select(-HBName, -HB, -Sex, -Year)
#creating a dataframe for the attendance rate predicted for Aug 2023 and Sept 2023 using model based on month
OrkneyAug23Month_attendancerate <- c(NHSBoardName = "NHS Orkney", Attendances = 465, Month = "2023-08-01", AllAges= 22731, attendancerate=0.0204566)
OrkneySept23Month_attendancerate <- c(NHSBoardName = "NHS Orkney", Attendances = 501, Month = "2023-09-01", AllAges= 22731, attendancerate=0.0220404)
OrkneyOct23Month_attendancerate <- c(NHSBoardName = "NHS Orkney", Attendances = 544, Month = "2023-10-01", AllAges= 22731, attendancerate=0.0239321)
OrkneyNov23Month_attendancerate <- c(NHSBoardName = "NHS Orkney", Attendances = 424, Month = "2023-11-01", AllAges= 22731, attendancerate=0.0186529)
OrkneyDec23Month_attendancerate <- c(NHSBoardName = "NHS Orkney", Attendances = 306, Month = "2023-12-01", AllAges= 22731, attendancerate=0.0134618)
OrkneyJan24Month_attendancerate <- c(NHSBoardName = "NHS Orkney", Attendances = 400, Month = "2024-01-01", AllAges= 22731, attendancerate=0.0175971)

#Combining the dataframe containing actual rates with the prediction for Sept and Aug 2023 
Orkney_rates2018to2023month <- rbind(Orkney_rates2018to2023month, OrkneyAug23Month_attendancerate)
Orkney_rates2018to2023month <- rbind(Orkney_rates2018to2023month, OrkneySept23Month_attendancerate)
Orkney_rates2018to2023month <- rbind(Orkney_rates2018to2023month, OrkneyOct23Month_attendancerate)
Orkney_rates2018to2023month <- rbind(Orkney_rates2018to2023month, OrkneyNov23Month_attendancerate)
Orkney_rates2018to2023month <- rbind(Orkney_rates2018to2023month, OrkneyDec23Month_attendancerate)
Orkney_rates2018to2023month <- rbind(Orkney_rates2018to2023month, OrkneyJan24Month_attendancerate)

str(Orkney_rates2018to2023)
#changing attendancerate to a numeric
Orkney_rates2018to2023month$attendancerate <- as.numeric(Orkney_rates2018to2023month$attendancerate) 


#Glasgow attendance rates 2018-2023
#adding new Month column based on assumption that end of one month is first day of next month
Glasgow_rates2018to2023month <- ae_byboard2018to2023 %>% filter(NHSBoardName=="NHS Greater Glasgow & Clyde") %>% mutate(Month = ae_byboard2018to2023_proportionsnewdate$Month)
#removing the Month ending date and year column
Glasgow_rates2018to2023month <- Glasgow_rates2018to2023month %>% select(-MonthEndingDate, -Year)
#Adding a new column for year
Glasgow_rates2018to2023month$Year <- as.numeric(format(Glasgow_rates2018to2023month$Month, "%Y"))
#Getting population estimate data for 2018-2023
Glasgow_Updatespopulation2018to2023month <- HBUpdatespopulation_estimate_HBname %>% filter(HBName=="NHS Greater Glasgow and Clyde")
Glasgow_Updatespopulation2018to2023month <- Glasgow_Updatespopulation2018to2023month %>% filter(Year=="2018"|Year=="2019"|Year=="2020"|Year=="2021"|Year=="2022"|Year=="2023")
#combining the dataframe for attendance and population in 2018-2023
Glasgow_rates2018to2023month <- merge(Glasgow_rates2018to2023month, Glasgow_Updatespopulation2018to2023month, by=c("Year"))
#adding a new column containing the attendance rate
Glasgow_rates2018to2023month <- Glasgow_rates2018to2023month %>% mutate(attendancerate = Attendances/AllAges)
#removing unnecessary columns
Glasgow_rates2018to2023month <- Glasgow_rates2018to2023month %>% select(-HBName, -HB, -Sex, -Year)
#creating a dataframe for the attendance rate predicted for Aug 2023 and Sept 2023 using  model based on month
GlasgowAug23Month_attendancerate <- c(NHSBoardName = "NHS Greater Glasgow & Clyde", Attendances = 27556, Month = "2023-08-01", AllAges= 1192485, attendancerate=0.0231080)
GlasgowSept23Month_attendancerate <- c(NHSBoardName = "NHS Greater Glasgow & Clyde", Attendances = 30279, Month = "2023-09-01", AllAges= 1192485, attendancerate=0.0253915)
GlasgowOct23Month_attendancerate <- c(NHSBoardName = "NHS Greater Glasgow & Clyde", Attendances = 32849, Month = "2023-10-01", AllAges= 1192485, attendancerate=0.0275467)
GlasgowNov23Month_attendancerate <- c(NHSBoardName = "NHS Greater Glasgow & Clyde", Attendances = 25608, Month = "2023-11-01", AllAges= 1192485, attendancerate=0.0214745)
GlasgowDec23Month_attendancerate <- c(NHSBoardName = "NHS Greater Glasgow & Clyde", Attendances = 18470, Month = "2023-12-01", AllAges= 1192485, attendancerate=0.0154887)
GlasgowJan24Month_attendancerate <- c(NHSBoardName = "NHS Greater Glasgow & Clyde", Attendances = 24004, Month = "2024-01-01", AllAges= 1192485, attendancerate=0.0201294)

#Combining the dataframe containing actual rates with the prediction for Sept and Aug 2023 
Glasgow_rates2018to2023month <- rbind(Glasgow_rates2018to2023month, GlasgowAug23Month_attendancerate)
Glasgow_rates2018to2023month <- rbind(Glasgow_rates2018to2023month, GlasgowSept23Month_attendancerate)
Glasgow_rates2018to2023month <- rbind(Glasgow_rates2018to2023month, GlasgowOct23Month_attendancerate)
Glasgow_rates2018to2023month <- rbind(Glasgow_rates2018to2023month, GlasgowNov23Month_attendancerate)
Glasgow_rates2018to2023month <- rbind(Glasgow_rates2018to2023month, GlasgowDec23Month_attendancerate)
Glasgow_rates2018to2023month <- rbind(Glasgow_rates2018to2023month, GlasgowJan24Month_attendancerate)

str(Glasgow_rates2018to2023)
#changing attendancerate to a numeric
Glasgow_rates2018to2023month$attendancerate <- as.numeric(Glasgow_rates2018to2023month$attendancerate) 

#Orkney and Glasgow in the same graph for model based on time
Orkney_Glasgow_rates2018to2023month <- rbind(Glasgow_rates2018to2023month, Orkney_rates2018to2023month)

Orkney_Glasgow_predict_rates2018to2023month <- ggplot(data=Orkney_Glasgow_rates2018to2023month, aes(x=Month, y=attendancerate, ggroup=NHSBoardName, color=NHSBoardName))+
  geom_point()+
  geom_line()+
  labs(x="Year", 
       y="Attendance rate", 
       col="NHS Board Name")
save_plot("Output/Orkney_Glasgow_predict_rates2018to2023month.svg", fig=Orkney_Glasgow_predict_rates2018to2023month, width=14, height=12)

Orkney_Glasgow_predict_rates2018to2023month <- ggplot(data=Orkney_Glasgow_rates2018to2023month, aes(x=Month, y=attendancerate, ggroup=NHSBoardName, color=NHSBoardName))+
  geom_point()+
  geom_line()+
  geom_vline(xintercept = as.Date("2023-08-01"), color="orange", linewidth=.5)+
  annotate(geom="text", x=as.Date("2023-03-01"), y=0.0325, size=3, label="Actual")+
  annotate(geom="text", x=as.Date("2024-01-01"), y=0.0325, size=3, label="Predicted")+
  geom_segment(aes(x=as.Date("2023-06-01"), y=0.0335, xend = as.Date("2022-12-01"), yend = 0.0335), arrow=arrow(length = unit(0.3, 'cm')), color='black')+
  
  geom_segment(aes(x=as.Date("2023-10-01"), y=0.0335, xend = as.Date("2024-04-01"), yend = 0.0335), arrow=arrow(length = unit(0.3, 'cm')), color='black')+#annotate(geom="text", x=as.Date("2023-03-01"), y=0.0340, size=3, label= sprintf('\u2191'))+
  labs(x="Year", 
       y="Attendance rate", 
       col="NHS Board Name")
save_plot("Output/Orkney_Glasgow_predict_rates2018to2023vlinemonth.svg", fig=Orkney_Glasgow_predict_rates2018to2023month, width=18, height=12)



#_______________end


#Orkney graph comparing GLM based on month, time and actual PHS values
Orkney_Predictvsactual_Aug23Jan24 <- read_xlsx(here("Rawdata", "2024-01-01-Orkney_Aug23Jan24predictions.xlsx"))
Orkney_Predictvsactual_Aug23Jan24$Month <- as.Date(Orkney_Predictvsactual_Aug23Jan24$Month) 
str(Orkney_Predictvsactual_Aug23Jan24)

Orkney_Predictvsactual_Aug23toJan24 <- ggplot(data=Orkney_Predictvsactual_Aug23Jan24, aes(x=Month, y=Attendances, ggroup=Model, color=Model))+
  geom_line()+
  labs(x="Month", 
       y="Attendances",
       col="Key:")+
  theme(legend.position = "bottom")
save_plot("Output/Orkney_predictvsactual_Aug23Jan24.svg", fig=Orkney_Predictvsactual_Aug23toJan24, width=12, height=12)


#Glasgow graph comparing GLM based on month, time and actual PHS values
Glasgow_Predictvsactual_Aug23Jan24 <- read_xlsx(here("Rawdata", "2024-01-01-Glasgow_Aug23Jan24predictions.xlsx"))
Glasgow_Predictvsactual_Aug23Jan24$Month <- as.Date(Glasgow_Predictvsactual_Aug23Jan24$Month) 
str(Glasgow_Predictvsactual_Aug23Jan24)

Glasgow_Predictvsactual_Aug23toJan24 <- ggplot(data=Glasgow_Predictvsactual_Aug23Jan24, aes(x=Month, y=Attendances, ggroup=Model, color=Model))+
  geom_line()+
  labs(x="Month", 
       y="Attendances", 
       col="Key:")+
  theme(legend.position = "bottom")
save_plot("Output/Glasgow_predictvsactual_Aug23Jan24.svg", fig=Glasgow_Predictvsactual_Aug23toJan24, width=12, height=12)
