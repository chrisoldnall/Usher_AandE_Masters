#ScottishCovidGLM3_predictions

#install.packages("tidyverse")
#install.packages("here")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("sf")
#install.packages("sjPlot")
#install.packages("pscl")

library(tidyverse)
library(here)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)
library(scales)
library(sjPlot)
library(pscl)

#PREDICT

#Creating the predictions models for the various GLMs

glm_Coviddate_predictions <-predict(Covidglm_Coviddate, type = "response")
summary(glm_Coviddate_predictions)

glm_Coviddatesex_predictions <-predict(Covidglm_Coviddatesex, type = "response")
summary(glm_Coviddatesex_predictions)

glm_Coviddatesexage_predictions <-predict(Covidglm_Coviddatesexage, type = "response")
summary(glm_Coviddatesexage_predictions)

glm_CoviddatesexageSIMD_predictions <-predict(Covidglm_CoviddatesexageSIMD, type = "response")
summary(glm_CoviddatesexageSIMD_predictions)

glm_CoviddatesexageSIMDday_predictions <-predict(Covidglm_CoviddatesexageSIMDday, type = "response")
summary(glm_CoviddatesexageSIMDday_predictions)

glm_CoviddatesexageSIMDdaytype_predictions <-predict(Covidglm_CoviddatesexageSIMDdaytype, type = "response")
summary(glm_CoviddatesexageSIMDdaytype_predictions)

glm_CoviddatesexageSIMDdaytypehour_predictions <-predict(Covidglm_CoviddatesexageSIMDdaytypehour, type = "response")
summary(glm_CoviddatesexageSIMDdaytypehour_predictions)

glm_CoviddatesexageSIMDdaytypehourHB_predictions <-predict(Covidglm_CoviddatesexageSIMDdaytypehourHB, type = "response")
summary(glm_CoviddatesexageSIMDdaytypehourHB_predictions)

glm_CoviddatesexageSIMDdaytypehourHBmonth_predictions <- predict(Covidglm_CoviddatesexageSIMDdaytypehourHBmonth, type = "response")
summary(glm_CoviddatesexageSIMDdaytypehourHBmonth_predictions)

glm_CoviddatesexageSIMDdaytypehourHBTime_predictions <- predict(Covidglm_CoviddatesexageSIMDdaytypehourHBTime, type = "response")
summary(glm_CoviddatesexageSIMDdaytypehourHBTime_predictions)


#creating a dataframe with population for all Scotland by month for 2018-2022 to join with Covid_monthlyae_glmprop to draw a graph for fitted versus actual
#includes removing S92000003 which is the population for NHS Scotland

#Loading population estimate (only till 2022) csv file
Scotpopulation2018to2022Covidglmplot <- read_csv(here("Rawdata", "20240809Popestimate_HB2019_1981to2022.csv"))

#Filtering out total Scottish estimates (HB S92000003) for each year, leaving just individual HBs in the list
Scotpopulation2018to2022Covidglmplot <- Scotpopulation2018to2022Covidglmplot %>%
  filter(!HB%in%"S92000003") 
#Excluding population by sex, only the totals for each HB each year
Scotpopulation2018to2022Covidglmplot <- Scotpopulation2018to2022Covidglmplot %>%
  filter(Sex=="All")
#Only getting the total population estimates for 2018-2022
Scotpopulation2018to2022Covidglmplot <- Scotpopulation2018to2022Covidglmplot %>% 
  group_by(Year) %>% 
  summarise(AllAges=sum(AllAges)) %>% 
  filter(Year=="2018"|Year=="2019"|Year=="2020"|Year=="2021"|Year=="2022") %>% 
  select(Year, AllAges) 

#Selecting only the columns needed for glm plot and adding a column for year to join with the population data
Covid_monthlyae_glmpropglmplot <- Covid_monthlyae_glmprop %>% 
  select(date, NumberOfAttendances) 
str(Covid_monthlyae_glmpropglmplot )

#this didn't work because date was already in date format
#Covid_monthlyae_glmpropglmplot$Year <- format(Covid_monthlyae_glmprop$date, format="%Y/%m/%d","%Y")
#used this insted
Covid_monthlyae_glmpropglmplot$Year <- as.numeric(format(Covid_monthlyae_glmprop$date,'%Y'))

#combining population data with attendance data to create the glm plot
Covid_monthlyae_glmpropglmplot <- 
  merge(Scotpopulation2018to2022Covidglmplot, Covid_monthlyae_glmpropglmplot, by=c("Year"))

#combining the glm predictions for glm_xxx_prediction data with actual
Covid_monthlyae_glmpropglmplot <- Covid_monthlyae_glmpropglmplot %>% 
  mutate("glmCoviddateprediction" = glm_Coviddate_predictions/AllAges, 
         "glmCoviddatesexprediction" = glm_Coviddatesex_predictions/AllAges, 
         "glmCoviddatesexageprediction" = glm_Coviddatesexage_predictions/AllAges,
         "glmCoviddatesexageSIMDprediction" = glm_CoviddatesexageSIMD_predictions/AllAges,
         "glmCoviddatesexageSIMDdayprediction" = glm_CoviddatesexageSIMDday_predictions/AllAges,
         "glmCoviddatesexageSIMDdaytypeprediction" = glm_CoviddatesexageSIMDdaytype_predictions/AllAges,
         "glmCoviddatesexageSIMDdaytypehourprediction" = glm_CoviddatesexageSIMDdaytypehour_predictions/AllAges,
         "glmCoviddatesexageSIMDdaytypehourHBprediction" = glm_CoviddatesexageSIMDdaytypehourHB_predictions/AllAges,
         "glmCoviddatesexageSIMDdaytypehourHBmonthprediction" = glm_CoviddatesexageSIMDdaytypehourHBmonth_predictions/AllAges,
         "glmCoviddatesexageSIMDdaytypehourHBTimeprediction" = glm_CoviddatesexageSIMDdaytypehourHBTime_predictions/AllAges,
         "rates" = NumberOfAttendances/AllAges)


#Creating individual line charts for each model

glmCoviddateprediction <- ggplot(data=Covid_monthlyae_glmpropglmplot, aes(x=date))+
  labs(x="Time", y= "Attendance rate", 
       #title = "glmCoviddateprediction"
  )+
  geom_line(aes(y= rates), color = "black")+
  geom_line(aes(y=glmCoviddateprediction), color = "yellow4")
save_plot("Output/glmCoviddateprediction.svg", fig = glmCoviddateprediction, width = 5, height = 5)


glmCoviddatesexprediction <- ggplot(data=Covid_monthlyae_glmpropglmplot, aes(x=date))+
  labs(x="Time", y= "Attendance rate", 
       #title = "glmCoviddateprediction"
  )+
  geom_line(aes(y= rates), color = "black")+
  geom_line(aes(y=glmCoviddatesexprediction), color = "red")
save_plot("Output/glmCoviddatesexprediction.svg", fig = glmCoviddatesexprediction, width = 5, height = 5)


glmCoviddatesexageprediction <- ggplot(data=Covid_monthlyae_glmpropglmplot, aes(x=date))+
  labs(x="Time", y= "Attendance rate", 
       #title = "glmCoviddatesexageprediction"
  )+
  geom_line(aes(y= rates), color = "black")+
  geom_line(aes(y=glmCoviddatesexageprediction), color = "blue")
save_plot("Output/glmCoviddatesexageprediction.svg", fig = glmCoviddatesexageprediction, width = 5, height = 5)


glmCoviddatesexageSIMDprediction <- ggplot(data=Covid_monthlyae_glmpropglmplot, aes(x=date))+
  labs(x="Time", y= "Attendance rate", 
       #title = "glmCoviddatesexageSIMDprediction"
  )+
  geom_line(aes(y= rates), color = "black")+
  geom_line(aes(y=glmCoviddatesexageSIMDprediction), color = "green")
save_plot("Output/glmCoviddatesexageSIMDprediction.svg", fig = glmCoviddatesexageSIMDprediction, width = 5, height = 5)


glmCoviddatesexageSIMDdayprediction <- ggplot(data=Covid_monthlyae_glmpropglmplot, aes(x=date))+
  labs(x="Time", y= "Attendance rate", 
       #title = "glmCoviddatesexageSIMDdayprediction"
  )+
  geom_line(aes(y= rates), color = "black")+
  geom_line(aes(y=glmCoviddatesexageSIMDdayprediction), color = "yellow")
save_plot("Output/glmCoviddatesexageSIMDdayprediction.svg", fig = glmCoviddatesexageSIMDdayprediction, width = 5, height = 5)


glmCoviddatesexageSIMDdaytypeprediction <- ggplot(data=Covid_monthlyae_glmpropglmplot, aes(x=date))+
  labs(x="Time", y= "Attendance rate", 
       #title = "glmCoviddatesexageSIMDdaytypeprediction"
  )+
  geom_line(aes(y= rates), color = "black")+
  geom_line(aes(y=glmCoviddatesexageSIMDdaytypeprediction), color = "orange")
save_plot("Output/glmCoviddatesexageSIMDdaytypeprediction.svg", fig = glmCoviddatesexageSIMDdaytypeprediction, width = 5, height = 5)


glmCoviddatesexageSIMDdaytypehourprediction <- ggplot(data=Covid_monthlyae_glmpropglmplot, aes(x=date))+
  labs(x="Time", y= "Attendance rate", 
       #title = "glmCoviddatesexageSIMDdaytypehourprediction"
  )+
  geom_line(aes(y= rates), color = "black")+
  geom_line(aes(y=glmCoviddatesexageSIMDdaytypehourprediction), color = "purple")
save_plot("Output/glmCoviddatesexageSIMDdaytypehourprediction.svg", fig = glmCoviddatesexageSIMDdaytypehourprediction, width = 5, height = 5)


glmCoviddatesexageSIMDdaytypehourHBprediction <- ggplot(data=Covid_monthlyae_glmpropglmplot, aes(x=date))+
  labs(x="Time", y= "Attendance rate", 
       #title = "glmCoviddatesexageSIMDdaytypehourHBprediction"
  )+
  geom_line(aes(y= rates), color = "black")+
  geom_line(aes(y=glmCoviddatesexageSIMDdaytypehourHBprediction), color = "brown")
save_plot("Output/glmCoviddatesexageSIMDdaytypehourHBprediction.svg", fig = glmCoviddatesexageSIMDdaytypehourHBprediction, width = 5, height = 5)


glmCoviddatesexageSIMDdaytypehourHBmonthprediction <- ggplot(data=Covid_monthlyae_glmpropglmplot, aes(x=date))+
  labs(x="Time", y= "Attendance rate", 
       #title = "glmCoviddatesexageSIMDdaytypehourHBmonthprediction"
  )+
  geom_line(aes(y= rates), color = "black")+
  geom_line(aes(y=glmCoviddatesexageSIMDdaytypehourHBmonthprediction), color = "salmon")
save_plot("Output/glmCoviddatesexageSIMDdaytypehourHBmonthprediction.svg", fig = glmCoviddatesexageSIMDdaytypehourHBmonthprediction, width = 5, height = 5)


glmCoviddatesexageSIMDdaytypehourHBTimeprediction <- ggplot(data=Covid_monthlyae_glmpropglmplot, aes(x=date))+
  labs(x="Time", y= "Attendance rate", 
       #title = "glmCoviddatesexageSIMDdaytypehourHBTimeprediction"
  )+
  geom_line(aes(y= rates), color = "black")+
  geom_line(aes(y=glmCoviddatesexageSIMDdaytypehourHBTimeprediction), color = "magenta")
save_plot("Output/glmCoviddatesexageSIMDdaytypehourHBTimeprediction.svg", fig = glmCoviddatesexageSIMDdaytypehourHBTimeprediction, width = 5, height = 5)



