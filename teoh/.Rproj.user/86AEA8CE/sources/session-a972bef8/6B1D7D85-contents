#Scottish A&E data - glm

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

#Refer to Data analysis for epidemiology - Week 2 Statistical inference in R (part 1) - 'Assumption checking and data transformation'
#Q_Q plot
QQplotTotalae <- ae_monthly_total %>% 
  ggplot(aes(sample=NumberOfAttendancesAll)) +
  stat_qq() +
  stat_qq_line(color=2)
save_plot("Output/QQplotTotalae.svg", fig = QQplotTotalae, width = 14, height = 12)

#Kolmogorov-Smirnov test of normality
ae_monthly_total %>% 
  pull(NumberOfAttendancesAll) %>% 
  ks.test(., "pnorm", mean=mean(.), sd=sd(.))
#Results returned:
#D = 0.12929, p-value = 0.003155
#alternative hypothesis: two-sided


#GLM model

glm_sex <- glm(Total_attendances ~ Male + Female,
               family = poisson(link = "log"), 
               data = sexagesimddayhourdepttypeHB_proportions)
summary(glm_sex)

glm_sexage <- glm(Total_attendances ~ Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour,
                  family = poisson(link = "log"), 
                  data = sexagesimddayhourdepttypeHB_proportions)
summary(glm_sexage)


glm_sexageSIMD <- glm(Total_attendances ~ Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMDOne + SIMDTwo + SIMDThree + SIMDFour + SIMDFive,
                      family = poisson(link = "log"), 
                      data = sexagesimddayhourdepttypeHB_proportions)
summary(glm_sexageSIMD)

glm_sexageSIMDday <- glm(Total_attendances ~ Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMDOne + SIMDTwo + SIMDThree + SIMDFour + SIMDFive + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday,
                         family = poisson(link = "log"), 
                         data = sexagesimddayhourdepttypeHB_proportions)
summary(glm_sexageSIMDday)

glm_sexageSIMDdaytype <- glm(Total_attendances ~ Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMDOne + SIMDTwo + SIMDThree + SIMDFour + SIMDFive + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED,
                             family = poisson(link = "log"), 
                             data = sexagesimddayhourdepttypeHB_proportions)
summary(glm_sexageSIMDdaytype)

glm_sexageSIMDdaytypehour <- glm(Total_attendances ~ Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMDOne + SIMDTwo + SIMDThree + SIMDFour + SIMDFive + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED + Midnighttoone + Twotothreeam + Threetofouram + Fourtofiveam + Fivetosixam + Sixtosevenam + Seventoeightam + Eighttonineam + Ninetotenam + Tentoelevenam + Eleventonoon + Noontoonepm + Onetotwopm + Twotothreepm + Threetofourpm + Fourtofivepm + Fivetosixpm + Sixtosevenpm + Seventoeightpm + Eighttoninepm + Ninetotenpm + Tentoelevenpm + Eleventomidnight, 
                                 family = poisson(link = "log"), 
                                 data = sexagesimddayhourdepttypeHB_proportions)
summary(glm_sexageSIMDdaytypehour)


glm_sexageSIMDdaytypehourHB <- glm(Total_attendances ~ Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMDOne + SIMDTwo + SIMDThree + SIMDFour + SIMDFive + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED + Midnighttoone + Twotothreeam + Threetofouram + Fourtofiveam + Fivetosixam + Sixtosevenam + Seventoeightam + Eighttonineam + Ninetotenam + Tentoelevenam + Eleventonoon + Noontoonepm + Onetotwopm + Twotothreepm + Threetofourpm + Fourtofivepm + Fivetosixpm + Sixtosevenpm + Seventoeightpm + Eighttoninepm + Ninetotenpm + Tentoelevenpm + Eleventomidnight + NHSBorders + NHSFife + NHSShetland + NHSLanarkshire + NHSDumfriesandGalloway+ NHSForthValley + NHSGrampian + NHSWesternIsles + NHSOrkney + NHSTayside + NHSGreaterGlasgowandClyde + NHSHighland + NHSLothian, 
                                   family = poisson(link = "log"), 
                                   data = sexagesimddayhourdepttypeHB_proportions)
summary(glm_sexageSIMDdaytypehourHB)

glm_sexageSIMDdaytypehourHBmonth <- glm(Total_attendances ~ Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMDOne + SIMDTwo + SIMDThree + SIMDFour + SIMDFive + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED + Midnighttoone + Twotothreeam + Threetofouram + Fourtofiveam + Fivetosixam + Sixtosevenam + Seventoeightam + Eighttonineam + Ninetotenam + Tentoelevenam + Eleventonoon + Noontoonepm + Onetotwopm + Twotothreepm + Threetofourpm + Fourtofivepm + Fivetosixpm + Sixtosevenpm + Seventoeightpm + Eighttoninepm + Ninetotenpm + Tentoelevenpm + Eleventomidnight + NHSBorders + NHSFife + NHSShetland + NHSLanarkshire + NHSDumfriesandGalloway+ NHSForthValley + NHSGrampian + NHSWesternIsles + NHSOrkney + NHSTayside + NHSGreaterGlasgowandClyde + NHSHighland + NHSLothian + Feb + March + April + May + June + July + August + Sept + Oct + Nov + Dec, 
                                        family = poisson(link = "log"), 
                                        data = sexagesimddayhourdepttypeHBmonth_proportions)
summary(glm_sexageSIMDdaytypehourHBmonth)

#didn't use this in the end because month and Time are related (not independent) and therefore results returned coefficients: (1 not defined because of singularities) 
glm_sexageSIMDdaytypehourHBmonthTime <- glm(Total_attendances ~ Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMDOne + SIMDTwo + SIMDThree + SIMDFour + SIMDFive + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED + Midnighttoone + Twotothreeam + Threetofouram + Fourtofiveam + Fivetosixam + Sixtosevenam + Seventoeightam + Eighttonineam + Ninetotenam + Tentoelevenam + Eleventonoon + Noontoonepm + Onetotwopm + Twotothreepm + Threetofourpm + Fourtofivepm + Fivetosixpm + Sixtosevenpm + Seventoeightpm + Eighttoninepm + Ninetotenpm + Tentoelevenpm + Eleventomidnight + NHSBorders + NHSFife + NHSShetland + NHSLanarkshire + NHSDumfriesandGalloway+ NHSForthValley + NHSGrampian + NHSWesternIsles + NHSOrkney + NHSTayside + NHSGreaterGlasgowandClyde + NHSHighland + NHSLothian + Feb + March + April + May + June + July + August + Sept + Oct + Nov + Dec + Time, 
                                            family = poisson(link = "log"), 
                                            data = sexagesimddayhourdepttypeHBmonthTime_proportions)
summary(glm_sexageSIMDdaytypehourHBmonthTime)

#decided to use month instead of Time because Time (instead of month) returned a larger AIC number (3412.9 instead of 1043.8), to my surprise as Chris expected it to return the same AIC/results 
glm_sexageSIMDdaytypehourHBTime <- glm(Total_attendances ~ Male + Female + Under18 + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMDOne + SIMDTwo + SIMDThree + SIMDFour + SIMDFive + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED + Midnighttoone + Twotothreeam + Threetofouram + Fourtofiveam + Fivetosixam + Sixtosevenam + Seventoeightam + Eighttonineam + Ninetotenam + Tentoelevenam + Eleventonoon + Noontoonepm + Onetotwopm + Twotothreepm + Threetofourpm + Fourtofivepm + Fivetosixpm + Sixtosevenpm + Seventoeightpm + Eighttoninepm + Ninetotenpm + Tentoelevenpm + Eleventomidnight + NHSBorders + NHSFife + NHSShetland + NHSLanarkshire + NHSDumfriesandGalloway+ NHSForthValley + NHSGrampian + NHSWesternIsles + NHSOrkney + NHSTayside + NHSGreaterGlasgowandClyde + NHSHighland + NHSLothian + Time, 
                                       family = poisson(link = "log"), 
                                       data = sexagesimddayhourdepttypeHBTime_proportions)
summary(glm_sexageSIMDdaytypehourHBTime)


#For the coefficients here, we can exponentiate them and this tells us the % increase in attendances for a 1% increase in this group
exp_coef_glm_sexageSIMDdaytypehourHBmonth <- exp(coef(glm_sexageSIMDdaytypehourHBmonth))
exp_coef_glm_sexageSIMDdaytypehourHBmonth

modelsex_aic <- AIC(modelsex)
modelsexage_aic <- AIC(modelsexage)
modelsexageSIMD_aic <- AIC(modelsexageSIMD)
modelsexageSIMDday_aic <- AIC(modelsexageSIMDday)
modelsexageSIMDdaytype_aic <- AIC(modelsexageSIMDdaytype)
modelsexageSIMDdaytypehour_aic <- AIC(modelsexageSIMDdaytypehour)
modelsexageSIMDdaytypehourHB_aic <- AIC(modelsexageSIMDdaytypehourHB)
##THE LM FOR MONTH RETURNED WEIRD RESULTS SO LEFT THIS
modelsexageSIMDdaytypehourHBmonth_aic <- AIC(modelsexageSIMDdaytypehourHBmonth)


glm_sex_aic <- AIC(glm_sex)
glm_sexage_aic <- AIC(glm_sexage)
glm_sexageSIMD_aic <- AIC(glm_sexageSIMD)
glm_sexageSIMDday_aic <- AIC(glm_sexageSIMDday)
glm_sexageSIMDdaytype_aic <- AIC(glm_sexageSIMDdaytype)
glm_sexageSIMDdaytypehour_aic <- AIC(glm_sexageSIMDdaytypehour)
glm_sexageSIMDdaytypehourHB_aic <- AIC(glm_sexageSIMDdaytypehourHB)
glm_sexageSIMDdaytypehourHBmonth_aic <- AIC(glm_sexageSIMDdaytypehourHBmonth)
glm_sexageSIMDdaytypehourHBTime_aic <- AIC(glm_sexageSIMDdaytypehourHBTime)

# Create a named vector of AIC values
##HAVEN'T ADDED LM for MONTH IN HERE BECAUSE THE lm RETURNED WEIRD RESULTS, but did add GLM for month
aic_values_lmglm <- c(modelsex_aic, modelsexage_aic, modelsexageSIMD_aic, modelsexageSIMDday_aic, modelsexageSIMDdaytype_aic, modelsexageSIMDdaytypehour_aic, modelsexageSIMDdaytypehourHB_aic, glm_sex_aic, glm_sexage_aic, glm_sexageSIMD_aic, glm_sexageSIMDday_aic, glm_sexageSIMDdaytype_aic, glm_sexageSIMDdaytypehour_aic, glm_sexageSIMDdaytypehourHB_aic, glm_sexageSIMDdaytypehourHBmonth_aic)
names(aic_values_lmglm) <- c("LMsex", "LMsexage", "LMsexageSIMD", "LMsexageSIMDday", "LMsexageSIMDdaytype", "LMsexageSIMDdaytypehour", "LMsexageSIMDdaytypehourHB", "GLMsex", "GLMsexage", "GLMsexageSIMD", "GLMsexageSIMDday", "GLMsexageSIMDdaytype", "GLMsexageSIMDdaytypehour", "GLMsexageSIMDdaytypehourHB", "GLMsexageSIMDdaytypehourHBmonth")
# Print the AIC values for comparison
print(aic_values_lmglm)
# Find the model with the lowest AIC
min_aic_model_lmglm <- names(aic_values_lmglm)[which.min(aic_values_lmglm)]
# Print the model with the lowest AIC
cat("Model with the lowest AIC is:", min_aic_model_lmglm, "with an AIC of", min(aic_values_lmglm), "\n")
#Model with the lowest AIC is: LMsexageSIMDdaytypehourHB with an AIC of 1299.850 
#GLM with lowest AIC was GLMsexageSIMDdaytypehourHB with an AIC of 3501.943 


# Create a named vector of glm AIC values
aic_values_glm <- c(glm_sex_aic, glm_sexage_aic, glm_sexageSIMD_aic, glm_sexageSIMDday_aic, glm_sexageSIMDdaytype_aic, glm_sexageSIMDdaytypehour_aic, glm_sexageSIMDdaytypehourHB_aic, glm_sexageSIMDdaytypehourHBmonth_aic)
names(aic_values_glm) <- c("GLMsex", "GLMsexage", "GLMsexageSIMD", "GLMsexageSIMDday", "GLMsexageSIMDdaytype", "GLMsexageSIMDdaytypehour", "GLMsexageSIMDdaytypehourHB", "GLMsexageSIMDdaytypehourHBmonth")
# Print the AIC values for comparison
print(aic_values_glm)
# Find the model with the lowest AIC
min_aic_model_glm <- names(aic_values_glm)[which.min(aic_values_glm)]
# Print the model with the lowest AIC
cat("Model with the lowest AIC is:", min_aic_model_glm, "with an AIC of", min(aic_values_glm), "\n")

