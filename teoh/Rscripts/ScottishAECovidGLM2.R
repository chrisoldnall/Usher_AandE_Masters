#ScottishCovidGLM2_GLM

###created the code for GLM including AIC and McFadden but not ran it 

#install.packages("tidyverse")
#install.packages("here")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("sf")
#install.packages("sjPlot")
#install.packages("pscl")
#install.packages("flexmix")

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
library(flexmix)

#TEST FOR NORMALITY

#Total number of attendances calculated from glmdemographicstotal and glmwhentotal are the same.

#The dataframe Covid_monthlyae_glmdemographicstotal was created in ScottishAECovidGLM.R
#Histogram of total number of attendances Jan 2018-Dec 2022- distribution skewed to the right, not normal
HistogramCovid_monthlyae_glmdemographicstotal <- Covid_monthlyae_glmdemographicstotal %>% 
  ggplot(aes(x=NumberOfAttendances))+
  geom_histogram()+
  labs(x= "Number of A&E Attendances", 
       y = "Count")
save_plot("Output/HistogramCovid_monthlyae_glmdemographicstotal.svg", fig = HistogramCovid_monthlyae_glmdemographicstotal, width = 14, height = 12)

#Refer to Data analysis for epidemiology - Week 2 Statistical inference in R (part 1) - 'Assumption checking and data transformation'
#Q_Q plot Jan 2018-Dec 2022
QQplotglmdemographicstotal <- Covid_monthlyae_glmdemographicstotal %>% 
  ggplot(aes(sample=NumberOfAttendances)) +
  stat_qq() +
  stat_qq_line(color=2)+
  labs(x= "Standard normalised theoretical distribution", 
       y = "Data distribution")
save_plot("Output/QQplotglmdemographicstotal.svg", fig = QQplotglmdemographicstotal, width = 14, height = 12)

#Kolmogorov-Smirnov test of normality Jan 2018-Dec 2022
Covid_monthlyae_glmdemographicstotal %>% 
  pull(NumberOfAttendances) %>% 
  ks.test(., "pnorm", mean=mean(.), sd=sd(.))
#Results returned when it was ran against Jan 2018-June 2024:
#D = 0.16274, p-value = 0.02844
#after limiting data to Jan 2018-Dec 2022
#D = 0.15665, p-value = 0.09424
#alternative hypothesis: two-sided


#GLM including data for COVID

#Using Coviddate1 as reference
Covidglm_Coviddate <- glm(NumberOfAttendances ~ Coviddate2 + Coviddate3 + Coviddate4,
                    family = poisson(link = "log"), 
                    data = Covid_monthlyae_glmprop)
summary(Covidglm_Coviddate)

#Using UnknownSex as reference
Covidglm_Coviddatesex <- glm(NumberOfAttendances ~ Coviddate2 + Coviddate3 + Coviddate4 + Male + Female,
               family = poisson(link = "log"), 
               data = Covid_monthlyae_glmprop)
summary(Covidglm_Coviddatesex)

#Using UnknownAge and Seventyfiveplus as reference. When only used unknown age, it returned Coefficients:(1 not defined because of singularities)
Covidglm_Coviddatesexage <- glm(NumberOfAttendances ~ Coviddate2 + Coviddate3 + Coviddate4 + Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour,
                  family = poisson(link = "log"), 
                  data = Covid_monthlyae_glmprop)
summary(Covidglm_Coviddatesexage)

#Using UnknownSIMD as reference
Covidglm_CoviddatesexageSIMD <- glm(NumberOfAttendances ~ Coviddate2 + Coviddate3 + Coviddate4 + Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMD1 + SIMD2 + SIMD3 + SIMD4 + SIMD5,
                      family = poisson(link = "log"), 
                      data = Covid_monthlyae_glmprop)
summary(Covidglm_CoviddatesexageSIMD)

#Using Monday as reference
Covidglm_CoviddatesexageSIMDday <- glm(NumberOfAttendances ~ Coviddate2 + Coviddate3 + Coviddate4 + Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMD1 + SIMD2 + SIMD3 + SIMD4 + SIMD5 + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday,
                                    family = poisson(link = "log"), 
                                    data = Covid_monthlyae_glmprop)
summary(Covidglm_CoviddatesexageSIMDday)

#Using MIU/Other as reference
Covidglm_CoviddatesexageSIMDdaytype <- glm(NumberOfAttendances ~ Coviddate2 + Coviddate3 + Coviddate4 + Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMD1 + SIMD2 + SIMD3 + SIMD4 + SIMD5 + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED,
                                       family = poisson(link = "log"), 
                                       data = Covid_monthlyae_glmprop)
summary(Covidglm_CoviddatesexageSIMDdaytype)

#Using Onetotwoam as reference
Covidglm_CoviddatesexageSIMDdaytypehour <- glm(NumberOfAttendances ~ Coviddate2 + Coviddate3 + Coviddate4 + Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMD1 + SIMD2 + SIMD3 + SIMD4 + SIMD5 + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED + Midnighttoone + Twotothreeam + Threetofouram + Fourtofiveam + Fivetosixam + Sixtosevenam + Seventoeightam + Eighttonineam + Ninetotenam + Tentoelevenam + Eleventonoon + Noontoonepm + Onetotwopm + Twotothreepm + Threetofourpm + Fourtofivepm + Fivetosixpm + Sixtosevenpm + Seventoeightpm + Eighttoninepm + Ninetotenpm + Tentoelevenpm + Eleventomidnight,
                                           family = poisson(link = "log"), 
                                           data = Covid_monthlyae_glmprop)
summary(Covidglm_CoviddatesexageSIMDdaytypehour)

#Using NHSAyshireandArran as reference
Covidglm_CoviddatesexageSIMDdaytypehourHB <- glm(NumberOfAttendances ~ Coviddate2 + Coviddate3 + Coviddate4 + Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMD1 + SIMD2 + SIMD3 + SIMD4 + SIMD5 + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED + Midnighttoone + Twotothreeam + Threetofouram + Fourtofiveam + Fivetosixam + Sixtosevenam + Seventoeightam + Eighttonineam + Ninetotenam + Tentoelevenam + Eleventonoon + Noontoonepm + Onetotwopm + Twotothreepm + Threetofourpm + Fourtofivepm + Fivetosixpm + Sixtosevenpm + Seventoeightpm + Eighttoninepm + Ninetotenpm + Tentoelevenpm + Eleventomidnight + NHSBorders + NHSFife + NHSShetland + NHSLanarkshire + NHSDumfriesandGalloway+ NHSForthValley + NHSGrampian + NHSWesternIsles + NHSOrkney + NHSTayside + NHSGreaterGlasgowandClyde + NHSHighland + NHSLothian,
                                               family = poisson(link = "log"), 
                                               data = Covid_monthlyae_glmprop)
summary(Covidglm_CoviddatesexageSIMDdaytypehourHB)


##Using month
#Using January as reference
Covidglm_CoviddatesexageSIMDdaytypehourHBmonth <- glm(NumberOfAttendances ~ Coviddate2 + Coviddate3 + Coviddate4 + Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMD1 + SIMD2 + SIMD3 + SIMD4 + SIMD5 + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED + Midnighttoone + Twotothreeam + Threetofouram + Fourtofiveam + Fivetosixam + Sixtosevenam + Seventoeightam + Eighttonineam + Ninetotenam + Tentoelevenam + Eleventonoon + Noontoonepm + Onetotwopm + Twotothreepm + Threetofourpm + Fourtofivepm + Fivetosixpm + Sixtosevenpm + Seventoeightpm + Eighttoninepm + Ninetotenpm + Tentoelevenpm + Eleventomidnight + NHSBorders + NHSFife + NHSShetland + NHSLanarkshire + NHSDumfriesandGalloway+ NHSForthValley + NHSGrampian + NHSWesternIsles + NHSOrkney + NHSTayside + NHSGreaterGlasgowandClyde + NHSHighland + NHSLothian + Feb + March + April + May + June + July + August + Sept + Oct + Nov + Dec,
                                                 family = poisson(link = "log"), 
                                                 data = Covid_monthlyae_glmprop)
summary(Covidglm_CoviddatesexageSIMDdaytypehourHBmonth)

##Using time
Covidglm_CoviddatesexageSIMDdaytypehourHBTime <- glm(NumberOfAttendances ~ Coviddate2 + Coviddate3 + Coviddate4 + Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMD1 + SIMD2 + SIMD3 + SIMD4 + SIMD5 + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED + Midnighttoone + Twotothreeam + Threetofouram + Fourtofiveam + Fivetosixam + Sixtosevenam + Seventoeightam + Eighttonineam + Ninetotenam + Tentoelevenam + Eleventonoon + Noontoonepm + Onetotwopm + Twotothreepm + Threetofourpm + Fourtofivepm + Fivetosixpm + Sixtosevenpm + Seventoeightpm + Eighttoninepm + Ninetotenpm + Tentoelevenpm + Eleventomidnight + NHSBorders + NHSFife + NHSShetland + NHSLanarkshire + NHSDumfriesandGalloway+ NHSForthValley + NHSGrampian + NHSWesternIsles + NHSOrkney + NHSTayside + NHSGreaterGlasgowandClyde + NHSHighland + NHSLothian + Time,
                                                      family = poisson(link = "log"), 
                                                      data = Covid_monthlyae_glmprop)
summary(Covidglm_CoviddatesexageSIMDdaytypehourHBTime)

#Hui wasn't able to run this because sin_month and cos_month were too computationally intensive for her computer
##Using sin and cos of month
Covidglm_CoviddatesexageSIMDdaytypehourHBTimesincos <- glm(NumberOfAttendances ~ Coviddate2 + Coviddate3 + Coviddate4 + Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMD1 + SIMD2 + SIMD3 + SIMD4 + SIMD5 + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED + Midnighttoone + Twotothreeam + Threetofouram + Fourtofiveam + Fivetosixam + Sixtosevenam + Seventoeightam + Eighttonineam + Ninetotenam + Tentoelevenam + Eleventonoon + Noontoonepm + Onetotwopm + Twotothreepm + Threetofourpm + Fourtofivepm + Fivetosixpm + Sixtosevenpm + Seventoeightpm + Eighttoninepm + Ninetotenpm + Tentoelevenpm + Eleventomidnight + NHSBorders + NHSFife + NHSShetland + NHSLanarkshire + NHSDumfriesandGalloway+ NHSForthValley + NHSGrampian + NHSWesternIsles + NHSOrkney + NHSTayside + NHSGreaterGlasgowandClyde + NHSHighland + NHSLothian + Time + sin_term + cos_term,
                                                     family = poisson(link = "log"), 
                                                     data = Covid_monthlyae_glmprop)
summary(Covidglm_CoviddatesexageSIMDdaytypehourHBTimesincos)


#For the coefficients here, we can exponentiate them and this tells us the % increase in attendances for a 1% increase in this group
#to exponentiate model using time
coefficients <- coef(Covidglm_CoviddatesexageSIMDdaytypehourHBTime)
exp_coefficients <- exp(coefficients)
exp_coefficients_df <- data.frame(Estimate = exp_coefficients)
print(exp_coefficients_df)

##Model using time, no hours
Covidglm_CoviddatesexageSIMDdaytypeHBTimenohour <- glm(NumberOfAttendances ~ Coviddate2 + Coviddate3 + Coviddate4 + Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMD1 + SIMD2 + SIMD3 + SIMD4 + SIMD5 + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED + NHSBorders + NHSFife + NHSShetland + NHSLanarkshire + NHSDumfriesandGalloway+ NHSForthValley + NHSGrampian + NHSWesternIsles + NHSOrkney + NHSTayside + NHSGreaterGlasgowandClyde + NHSHighland + NHSLothian + Time,
                                                     family = poisson(link = "log"), 
                                                     data = Covid_monthlyae_glmprop)
summary(Covidglm_CoviddatesexageSIMDdaytypeHBTimenohour)

#printing the coefficients for the model using time no hour to make it easier to copy and paste into the report
coefficients <- coef(Covidglm_CoviddatesexageSIMDdaytypeHBTimenohour)
coefficients_df <- data.frame(Estimate = coefficients)
print(coefficients_df)

#to exponentiate model using time, no hours
exp_coefficients <- exp(coefficients)
exp_coefficients_df <- data.frame(Estimate = exp_coefficients)
print(exp_coefficients_df)

#to get the confidence interval for model using time, no hours
conf_intervals <- confint(Covidglm_CoviddatesexageSIMDdaytypeHBTimenohour)
exp_conf_intervals <- exp(conf_intervals)
exp_conf_intervals

#For the coefficients here, we can exponentiate them and this tells us the % increase in attendances for a 1% increase in this group
#exp_coef_Covidglm_CoviddatesexageSIMDdaytypehourHBmonth <- exp(coef(Covidglm_CoviddatesexageSIMDdaytypehourHBmonth))
#exp_coef_Covidglm_CoviddatesexageSIMDdaytypehourHBmonth

#exp_coef_Covidglm_CoviddatesexageSIMDdaytypehourHBTime <- exp(coef(Covidglm_CoviddatesexageSIMDdaytypehourHBTime))
#exp_coef_Covidglm_CoviddatesexageSIMDdaytypehourHBTime


#To calculate the AIC
glm_Coviddate_aic <- AIC(Covidglm_Coviddate)
glm_Coviddatesex_aic <- AIC(Covidglm_Coviddatesex)
glm_Coviddatesexage_aic <- AIC(Covidglm_Coviddatesexage)
glm_CoviddatesexageSIMD_aic <- AIC(Covidglm_CoviddatesexageSIMD)
glm_CoviddatesexageSIMDday_aic <- AIC(Covidglm_CoviddatesexageSIMDday)
glm_CoviddatesexageSIMDdaytype_aic <- AIC(Covidglm_CoviddatesexageSIMDdaytype)
glm_CoviddatesexageSIMDdaytypehour_aic <- AIC(Covidglm_CoviddatesexageSIMDdaytypehour)
glm_CoviddatesexageSIMDdaytypehourHB_aic <- AIC(Covidglm_CoviddatesexageSIMDdaytypehourHB)
glm_CoviddatesexageSIMDdaytypehourHBmonth_aic <- AIC(Covidglm_CoviddatesexageSIMDdaytypehourHBmonth)
glm_CoviddatesexageSIMDdaytypehourHBTime_aic <- AIC(Covidglm_CoviddatesexageSIMDdaytypehourHBTime)
glm_CoviddatesexageSIMDdaytypeHBTimenohour_aic <- AIC(Covidglm_CoviddatesexageSIMDdaytypeHBTimenohour)

#McFadden's R-squared
#with reference to https://www.statology.org/glm-r-squared/
#method one using the package pscl  
pR2(Covidglm_CoviddatesexageSIMDdaytypehourHBmonth)['McFadden']
pR2(Covidglm_CoviddatesexageSIMDdaytypehourHBTime)['McFadden']
pR2(Covidglm_CoviddatesexageSIMDdaytypeHBTimenohour)['McFadden']

#BIC
#with reference to https://www.statology.org/bic-in-r/
BIC(Covidglm_CoviddatesexageSIMDdaytypehourHBTime)
#1057.675
BIC(Covidglm_CoviddatesexageSIMDdaytypeHBTimenohour)
#7461.634

#log likelihood
#with reference to https://www.statology.org/interpret-log-likelihood/
logLik(Covidglm_CoviddatesexageSIMDdaytypehourHBTime)
#'log Lik.' -406.0071 (df=60)
logLik(Covidglm_CoviddatesexageSIMDdaytypeHBTimenohour)
#'log Lik.' -3655.072 (df=37)


#Including sin and cos month into the modelling
#Chris created a new proportion table with sin_month and cos_month as it was the heavy computations were beyond the capabilities of Hui's computer
#Loading A&E demographic csv file
Covid_AE_Prop_Time_Pivot_GLM <- read_csv(here("Rawdata", "AE_Prop_Time_Pivot_GLM_Data.csv"))

#renaming the headers
Covid_AE_Prop_Time_Pivot_GLM <- Covid_AE_Prop_Time_Pivot_GLM %>% 
  rename(c("Undereighteen" = "Under 18",
           "EighteentoTwentyfour" = "18-24", 
           "TwentyfivetoThirtynine" = "25-39",
           "FortytoSixtyfour" = "40-64",
           "SixtyfivetoSeventyfour" = "65-74",
           "Seventyfiveplus" = "75 plus",
           "NHSAyrshireandArran" = "S08000015",
           "NHSBorders" = "S08000016",
           "NHSDumfriesandGalloway" = "S08000017",
           "NHSForthValley" = "S08000019",
           "NHSGrampian" = "S08000020",
           "NHSHighland" = "S08000022",
           "NHSLothian" = "S08000024",
           "NHSOrkney" = "S08000025",
           "NHSShetland" = "S08000026",
           "NHSWesternIsles" = "S08000028",
           "NHSFife" = "S08000029",
           "NHSTayside" = "S08000030",
           "NHSGreaterGlasgowandClyde" = "S08000031",
           "NHSLanarkshire" = "S08000032"))

##Using sin and cos of month
Covidglm_CovidperiodsexageSIMDdaytypeHBTimesincosnohour <- glm(TotalAttendances ~ CovidPeriod + Male + Female + Undereighteen + EighteentoTwentyfour + TwentyfivetoThirtyNine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMD1 + SIMD2 + SIMD3 + SIMD4 + SIMD5 + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED + NHSBorders + NHSFife + NHSShetland + NHSLanarkshire + NHSDumfriesandGalloway+ NHSForthValley + NHSGrampian + NHSWesternIsles + NHSOrkney + NHSTayside + NHSGreaterGlasgowandClyde + NHSHighland + NHSLothian + Time + sin_month + cos_month,
                                                           family = poisson(link = "log"), 
                                                           data = Covid_AE_Prop_Time_Pivot_GLM)
summary(Covidglm_CovidperiodsexageSIMDdaytypeHBTimesincosnohour)

#For the coefficients here, we can exponentiate them and this tells us the % increase in attendances for a 1% increase in this group
exp_coef_Covidglm_CovidperiodsexageSIMDdaytypeHBTimesincosnohour <- exp(coef(Covidglm_CovidperiodsexageSIMDdaytypeHBTimesincosnohour))
exp_coef_Covidglm_CovidperiodsexageSIMDdaytypeHBTimesincosnohour

#To calculate the AIC
glm_CovidperiodsexageSIMDdaytypeHBTimesincosnohour_aic <- AIC(Covidglm_CovidperiodsexageSIMDdaytypeHBTimesincosnohour)
glm_CovidperiodsexageSIMDdaytypeHBTimesincosnohour_aic
#7709.403

#McFadden's Rsquared
pR2(Covidglm_CovidperiodsexageSIMDdaytypeHBTimesincosnohour)['McFadden']
#0.954261

#BIC
BIC(Covidglm_CovidperiodsexageSIMDdaytypeHBTimesincosnohour)
#7786.893

#log likelihood
logLik(Covidglm_CovidperiodsexageSIMDdaytypeHBTimesincosnohour)
#'log Lik.' -3817.701 (df=37)

#Exponentiated coefficient times the coefficient to get the graphs 
#Didn't use this
#Covidglm_Covidperiod <- glm(TotalAttendances ~ C(CovidPeriod),
#                            family = poisson(link = "log"), 
#                            data = Covid_AE_Prop_Time_Pivot_GLM)
#summary(Covidglm_Covidperiod)

#Didn't use this
#Covidglm_sex <- glm(TotalAttendances ~ Male + Female,
#                            family = poisson(link = "log"), 
#                            data = Covid_AE_Prop_Time_Pivot_GLM)
#summary(Covidglm_sex)

#coefficients from the model for male and female. Unknown was the reference category.
#Male                      -2.731e+00  5.495e-01  -4.969 6.71e-07 ***
#Female                    -3.578e+00  5.480e-01  -6.529 6.61e-11 ***


Neteffect_sex <- Covid_AE_Prop_Time_Pivot_GLM %>% 
  select(Month, Male, Female, UnknownSex)

#Calculating the coefficient times proportion for males and females
Maleprop <- -2.731
Neteffect_sex$coeffpropmale <-  Neteffect_sex$Male*Maleprop

Femaleprop <-  -3.578
Neteffect_sex$coeffpropfemale <-  Neteffect_sex$Male*Femaleprop


#To create a date column
#creating a new column for Year, monthnumeric and day using data from the Month column
Neteffect_sex$Year <- substr(Neteffect_sex$Month, 1,4)
Neteffect_sex$monthnumeric <- substr(Neteffect_sex$Month, 5,6)
Neteffect_sex$day <- "01"

#converting from character to numeric variable
Neteffect_sex$Year <- as.numeric(Neteffect_sex$Year)
Neteffect_sex$monthnumeric <- as.numeric(Neteffect_sex$monthnumeric)
Neteffect_sex$day <- as.numeric(Neteffect_sex$day)

#making a date column using the Year, monthnumeric and day columns
Neteffect_sex<- Neteffect_sex %>% 
  mutate(date=make_date(Year, monthnumeric, day))

#sum of the coefficient times proportion for males and females
Neteffect_sex$coeffpropmaleplusfemale<- Neteffect_sex$coeffpropmale + Neteffect_sex$coeffpropfemale

#exponentiating the sum of the coefficient times proportion for males and females
Neteffect_sex$expcoeffpropmaleplusfemale <- exp(Neteffect_sex$coeffpropmaleplusfemale)

str(Neteffect_sex)
Neteffect_sex$expcoeffpropmaleplusfemale <- as.numeric(Neteffect_sex$expcoeffpropmaleplusfemale)

#drawing the graph of Net effect of sex on total attendances over time
#code returned error message Error in aes(x = Month, y = expcoeffpropmaleplusfemale) + geom_line() +  : 
#non-numeric argument to binary operator
Neteffect_sex_plot <- ggplot(data=Neteffect_sex, aes(x=date, y=expcoeffpropmaleplusfemale)+
                               geom_line()+
                               labs(title="Net effect of sex on total attendances over time", 
                                    x = "Date", 
                                    y = "Net effect on attendances"))


