#ScottishCovidGLM2

###creating the code for GLM but not ran it ###to add month and time

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
##NOT RAN THIS YET

#Using Coviddate1 as reference
Covidglm_Coviddate <- glm(NumberOfAttendances ~ Coviddate2 + Coviddate3 + Coviddate4,
                    family = poisson(link = "log"), 
                    data = Covid_monthlyae_glmprop)
summary(Covidglm_Coviddate)

#Using UnknownSex as reference
Covidglm_Coviddatesex <- glm(NumberOfAttendances ~ Coviddate2 + Coviddate3 + Coviddate4 + Male + Female,
               family = poisson(link = "log"), 
               data = Covid_monthlyae_glmprop)
summary(Covidglm_sex)

#Using UnkknownAge as reference
Covidglm_Coviddatesexage <- glm(NumberOfAttendances ~ Coviddate2 + Coviddate3 + Coviddate4 + Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + Seventyfiveplus,
                  family = poisson(link = "log"), 
                  data = Covid_monthlyae_glmprop)
summary(Covidglm_Coviddatesexage)

#Using UnknownSIMD as reference
Covidglm_CoviddatesexageSIMD <- glm(NumberOfAttendances ~ Coviddate2 + Coviddate3 + Coviddate4 + Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + Seventyfiveplus + SIMD1 + SIMD2 + SIMD3 + SIMD4 + SIMD5,
                      family = poisson(link = "log"), 
                      data = Covid_monthlyae_glmprop)
summary(Covidglm_CoviddatesexageSIMD)

#Using Monday as reference
Covidglm_CoviddatesexageSIMDday <- glm(NumberOfAttendances ~ Coviddate2 + Coviddate3 + Coviddate4 + Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + Seventyfiveplus + SIMD1 + SIMD2 + SIMD3 + SIMD4 + SIMD5 + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday,
                                    family = poisson(link = "log"), 
                                    data = Covid_monthlyae_glmprop)
summary(Covidglm_CoviddatesexageSIMDday)

#Using MIU/Other as reference
Covidglm_CoviddatesexageSIMDdaytype <- glm(NumberOfAttendances ~ Coviddate2 + Coviddate3 + Coviddate4 + Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + Seventyfiveplus + SIMD1 + SIMD2 + SIMD3 + SIMD4 + SIMD5 + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED,
                                       family = poisson(link = "log"), 
                                       data = Covid_monthlyae_glmprop)
summary(Covidglm_CoviddatesexageSIMDdaytype)

#Using Onetotwoam as reference
Covidglm_CoviddatesexageSIMDdaytypehour <- glm(NumberOfAttendances ~ Coviddate2 + Coviddate3 + Coviddate4 + Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + Seventyfiveplus + SIMD1 + SIMD2 + SIMD3 + SIMD4 + SIMD5 + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED + Midnighttoone + Twotothreeam + Threetofouram + Fourtofiveam + Fivetosixam + Sixtosevenam + Seventoeightam + Eighttonineam + Ninetotenam + Tentoelevenam + Eleventonoon + Noontoonepm + Onetotwopm + Twotothreepm + Threetofourpm + Fourtofivepm + Fivetosixpm + Sixtosevenpm + Seventoeightpm + Eighttoninepm + Ninetotenpm + Tentoelevenpm + Eleventomidnight,
                                           family = poisson(link = "log"), 
                                           data = Covid_monthlyae_glmprop)
summary(Covidglm_CoviddatesexageSIMDdaytypehour)

#Using NHSAyshireandArran as reference
Covidglm_CoviddatesexageSIMDdaytypehourHB <- glm(NumberOfAttendances ~ Coviddate2 + Coviddate3 + Coviddate4 + Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + Seventyfiveplus + SIMD1 + SIMD2 + SIMD3 + SIMD4 + SIMD5 + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED + Midnighttoone + Twotothreeam + Threetofouram + Fourtofiveam + Fivetosixam + Sixtosevenam + Seventoeightam + Eighttonineam + Ninetotenam + Tentoelevenam + Eleventonoon + Noontoonepm + Onetotwopm + Twotothreepm + Threetofourpm + Fourtofivepm + Fivetosixpm + Sixtosevenpm + Seventoeightpm + Eighttoninepm + Ninetotenpm + Tentoelevenpm + Eleventomidnight + NHSBorders + NHSFife + NHSShetland + NHSLanarkshire + NHSDumfriesandGalloway+ NHSForthValley + NHSGrampian + NHSWesternIsles + NHSOrkney + NHSTayside + NHSGreaterGlasgowandClyde + NHSHighland + NHSLothian,
                                               family = poisson(link = "log"), 
                                               data = Covid_monthlyae_glmprop)
summary(Covidglm_CoviddatesexageSIMDdaytypehourHB)


###to add month and time