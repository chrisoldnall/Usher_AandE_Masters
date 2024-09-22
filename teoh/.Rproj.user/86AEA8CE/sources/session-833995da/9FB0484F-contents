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

#Total number of attendance calculated from glmdemographicstotal and glmwhentotal are the same.

#Histogram of total number of attendances - distribution skewed to the right, not normal
HistogramCovid_monthlyae_glmdemographicstotal <- Covid_monthlyae_glmdemographicstotal %>% 
  ggplot(aes(x=NumberOfAttendances))+
  geom_histogram()
save_plot("Output/HistogramCovid_monthlyae_glmdemographicstotal.svg", fig = HistogramCovid_monthlyae_glmdemographicstotal, width = 14, height = 12)

#Refer to Data analysis for epidemiology - Week 2 Statistical inference in R (part 1) - 'Assumption checking and data transformation'
#Q_Q plot
QQplotglmdemographicstotal <- Covid_monthlyae_glmdemographicstotal %>% 
  ggplot(aes(sample=NumberOfAttendances)) +
  stat_qq() +
  stat_qq_line(color=2)
save_plot("Output/QQplotglmdemographicstotal.svg", fig = QQplotglmdemographicstotal, width = 14, height = 12)

#Kolmogorov-Smirnov test of normality
Covid_monthlyae_glmdemographicstotal %>% 
  pull(NumberOfAttendances) %>% 
  ks.test(., "pnorm", mean=mean(.), sd=sd(.))
#Results returned:
#D = 0.16274, p-value = 0.02844
#alternative hypothesis: two-sided

#GLM including data for COVID

#Coviddate2 had a typo as Covidadate2 - has been corrected in the script
#Covid_monthlyae_glmprop <- Covid_monthlyae_glmprop %>% 
#  rename("Coviddate2" = "Covidadate2")

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
exp_coef_Covidglm_CoviddatesexageSIMDdaytypehourHBmonth <- exp(coef(Covidglm_CoviddatesexageSIMDdaytypehourHBmonth))
exp_coef_Covidglm_CoviddatesexageSIMDdaytypehourHBmonth

exp_coef_Covidglm_CoviddatesexageSIMDdaytypehourHBTime <- exp(coef(Covidglm_CoviddatesexageSIMDdaytypehourHBTime))
exp_coef_Covidglm_CoviddatesexageSIMDdaytypehourHBTime


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


#McFadden's R-squared
#with reference to https://www.statology.org/glm-r-squared/
#method one using the package pscl  
pR2(Covidglm_CoviddatesexageSIMDdaytypehourHBmonth)['McFadden']
pR2(Covidglm_CoviddatesexageSIMDdaytypehourHBTime)['McFadden']

