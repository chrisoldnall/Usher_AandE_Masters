#ScottishCovidGLM2_GLM

###created the code for GLM including AIC and McFadden but not ran it 

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

#TEST FOR NORMALITY

#Total number of attendances calculated from glmdemographicstotal and glmwhentotal are the same.

#The dataframe Covid_monthlyae_glmdemographicstotal was created in ScottishAECovidGLM.R
#Histogram of total number of attendances Jan 2018-Dec 2022- distribution skewed to the right, not normal
HistogramCovid_monthlyae_glmdemographicstotal <- Covid_monthlyae_glmdemographicstotal %>% 
  ggplot(aes(x=NumberOfAttendances))+
  geom_histogram()
save_plot("Output/HistogramCovid_monthlyae_glmdemographicstotal.svg", fig = HistogramCovid_monthlyae_glmdemographicstotal, width = 14, height = 12)

#Refer to Data analysis for epidemiology - Week 2 Statistical inference in R (part 1) - 'Assumption checking and data transformation'
#Q_Q plot Jan 2018-Dec 2022
QQplotglmdemographicstotal <- Covid_monthlyae_glmdemographicstotal %>% 
  ggplot(aes(sample=NumberOfAttendances)) +
  stat_qq() +
  stat_qq_line(color=2)
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
