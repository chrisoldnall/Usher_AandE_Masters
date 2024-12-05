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

Covid_AE_Prop_Time_Pivot_GLM <- read_csv("/Users/chrisoldnall/Library/Mobile Documents/com~apple~CloudDocs/PhD/Masters Supervision/1. Hui Pheng Teoh/GitHub/Usher_AandE_Masters/Usher_AandE_Masters/data/ProcessedData/AE_Prop_Time_Pivot_GLM_Data.csv")

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

#Make CovidPeriod a categorical factor
Covid_AE_Prop_Time_Pivot_GLM$CovidPeriod <- as.factor(Covid_AE_Prop_Time_Pivot_GLM$CovidPeriod)

##Using sin and cos of month
Covidglm_CovidperiodsexageSIMDdaytypeHBTimesincosnohour <- glm(TotalAttendances ~ C(CovidPeriod) + Male + Female + UnknownSex + UnknownAge + Seventyfiveplus + Undereighteen + EighteentoTwentyfour + TwentyfivetoThirtynine + FortytoSixtyfour + SixtyfivetoSeventyfour + SIMD1 + SIMD2 + SIMD3 + SIMD4 + SIMD5 + UnknownSIMD + Monday + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday + ED + MIU + NHSAyrshireandArran + NHSBorders + NHSFife + NHSShetland + NHSLanarkshire + NHSDumfriesandGalloway+ NHSForthValley + NHSGrampian + NHSWesternIsles + NHSOrkney + NHSTayside + NHSGreaterGlasgowandClyde + NHSHighland + NHSLothian + Time + sin_month + cos_month,
                                                           family = poisson(link = "log"), 
                                                           data = Covid_AE_Prop_Time_Pivot_GLM)
summary(Covidglm_CovidperiodsexageSIMDdaytypeHBTimesincosnohour)

#For the coefficients here, we can exponentiate them and this tells us the % increase in attendances for a 1% increase in this group
exp_coef_Covidglm_CovidperiodsexageSIMDdaytypeHBTimesincosnohour <- exp(coef(Covidglm_CovidperiodsexageSIMDdaytypeHBTimesincosnohour))
exp_coef_Covidglm_CovidperiodsexageSIMDdaytypeHBTimesincosnohour

#To calculate the AIC
glm_CovidperiodsexageSIMDdaytypeHBTimesincosnohour_aic <- AIC(Covidglm_CovidperiodsexageSIMDdaytypeHBTimesincosnohour)
glm_CovidperiodsexageSIMDdaytypeHBTimesincosnohour_aic
#7220.731

#McFadden's Rsquared
pR2(Covidglm_CovidperiodsexageSIMDdaytypeHBTimesincosnohour)['McFadden']
#0.9572123

#BIC
BIC(Covidglm_CovidperiodsexageSIMDdaytypeHBTimesincosnohour)
#7302.411

#log likelihood
logLik(Covidglm_CovidperiodsexageSIMDdaytypeHBTimesincosnohour)
#'log Lik.' -3571.366 (df=39)

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

##SEX
#Net effect of sex on attendances
Neteffect_sex <- Covid_AE_Prop_Time_Pivot_GLM %>% 
  select(Month, Male, Female, UnknownSex)

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

#calculate the difference in proportions each month
#when using diff() the number of rows reduces by one so couldn't display in the same dataframe
#diff(Neteffect_sex$Male)
#Neteffect_sex$Femalediff <- diff(Neteffect_sex$Female)
#Neteffect_sex$UnknownSexdiff <- diff(Neteffect_sex$UnknownSex)

#creating a dataframe to calculate the difference in proportions
Neteffect_sexdiff <- Neteffect_sex %>% 
  select(Male, Female, UnknownSex)

#to calculate the difference in proportions
Neteffect_sexdiff <- sapply(Neteffect_sexdiff, diff)

#to add a row for the first line of zero
firstdiffline <- c(0, 0, 0)

#to combine the line of zero with the proportion differences
Neteffect_sexdiff <- rbind(firstdiffline, Neteffect_sexdiff) 

#changing column names so it is clear it is for diff
colnames(Neteffect_sexdiff)<- c("Malediff", "Femalediff","UnknownSexdiff")

#Combine original Neteffect_sex dataframe to contain the proportion differences
Neteffect_sex <- cbind(Neteffect_sex, Neteffect_sexdiff)

#Calculate diff for male and female. Then times the coefficient. Then add it and then overall exponentiation. 
#Calculating the coefficient times proportion for males, females and unknownsex
Malecoeff <- -0.9047
Neteffect_sex$coeffpropmale <-  Neteffect_sex$Malediff*Malecoeff

Femalecoeff <-  -0.4119
Neteffect_sex$coeffpropfemale <-  Neteffect_sex$Femalediff*Femalecoeff

UnknownSexcoeff <-  NA
Neteffect_sex$coeffpropunknownsex <-  Neteffect_sex$UnknownSexdiff*UnknownSexcoeff


#sum of the coefficient times proportion for males and females
Neteffect_sex$coeffpropmaleplusfemaleplusunknown<- Neteffect_sex$coeffpropmale + Neteffect_sex$coeffpropfemale + Neteffect_sex$coeffpropunknownsex

#exponentiating the sum of the coefficient times proportion for males and females
Neteffect_sex$expcoeffpropmaleplusfemaleplusunknown <- exp(Neteffect_sex$coeffpropmaleplusfemaleplusunknown)

#drawing the graph of Net effect of sex on total attendances over time

Neteffect_sex_plot <- ggplot(data=Neteffect_sex, aes(x=date, y=expcoeffpropmaleplusfemaleplusunknown))+
                               geom_line()+
                               labs(title="Net effect of sex on total attendances over time", 
                                    x = "Date", 
                                    y = "Net effect on attendances")
save_plot("Output/Neteffect_sex_plot.svg", fig=Neteffect_sex_plot, width=14, height=12)

##AGE
#Net effect of age on attendances
Neteffect_age <- Covid_AE_Prop_Time_Pivot_GLM %>% 
  select(Month, EighteentoTwentyfour, TwentyfivetoThirtynine, FortytoSixtyfour, SixtyfivetoSeventyfour, Seventyfiveplus, Undereighteen, UnknownAge)

#To create a date column
#creating a new column for Year, monthnumeric and day using data from the Month column
Neteffect_age$Year <- substr(Neteffect_age$Month, 1,4)
Neteffect_age$monthnumeric <- substr(Neteffect_age$Month, 5,6)
Neteffect_age$day <- "01"

#converting from character to numeric variable
Neteffect_age$Year <- as.numeric(Neteffect_age$Year)
Neteffect_age$monthnumeric <- as.numeric(Neteffect_age$monthnumeric)
Neteffect_age$day <- as.numeric(Neteffect_age$day)

#making a date column using the Year, monthnumeric and day columns
Neteffect_age<- Neteffect_age %>% 
  mutate(date=make_date(Year, monthnumeric, day))

#creating a dataframe to calculate the difference in proportions
Neteffect_agediff <- Neteffect_age %>% 
  select(EighteentoTwentyfour, TwentyfivetoThirtynine, FortytoSixtyfour, SixtyfivetoSeventyfour, Seventyfiveplus, Undereighteen, UnknownAge)

#to calculate the difference in proportions
Neteffect_agediff <- sapply(Neteffect_agediff, diff)

#to add a row for the first line of zero
firstdiffline_age <- c(0, 0, 0, 0, 0, 0, 0)

#to combine the line of zero with the proportion differences
Neteffect_agediff <- rbind(firstdiffline_age, Neteffect_agediff) 

#changing column names so it is clear it is for diff
colnames(Neteffect_agediff)<- c("EighteentoTwentyfourdiff", "TwentyfivetoThirtyninediff", "FortytoSixtyfourdiff", "SixtyfivetoSeventyfourdiff", "Seventyfiveplusdiff", "Undereighteendiff", "UnknownAgediff")

#Combine original Neteffect_sex dataframe to contain the proportion differences
Neteffect_age <- cbind(Neteffect_age, Neteffect_agediff)

#Calculate diff for male and female. Then times the coefficient. Then add it and then overall exponentiation. 
#Calculating the coefficient times proportion for males, females and unknownsex
Undereighteencoeff <- 2.594
Neteffect_age$coeffpropUndereighteen <- Neteffect_age$Undereighteendiff*Undereighteencoeff

EighteentoTwentyfourcoeff <- 2.478
Neteffect_age$coeffpropEighteentoTwentyfour <-Neteffect_age$EighteentoTwentyfourdiff*EighteentoTwentyfourcoeff

TwentyfivetoThirtyninecoeff <- -3.873
Neteffect_age$coeffpropTwentyfivetoThirtynine <- Neteffect_age$TwentyfivetoThirtyninediff*TwentyfivetoThirtyninecoeff

FortytoSixtyfourcoeff <- 2.599
Neteffect_age$coeffpropFortytoSixtyfour <- Neteffect_age$FortytoSixtyfourdiff*FortytoSixtyfourcoeff

#SixtyfivetoSeventyfourcoeff <- NA
#Neteffect_age$coeffpropSixtyfivetoSeventyfour <- Neteffect_age$SixtyfivetoSeventyfourdiff*SixtyfivetoSeventyfourcoeff

Seventyfivepluscoeff <- 4.365
Neteffect_age$coeffpropSeventyfiveplus <- Neteffect_age$Seventyfiveplusdiff*Seventyfivepluscoeff

#UnknownAgecoeff <- NA
#Neteffect_age$coeffpropUnknownAge <- Neteffect_age$UnknownAgediff*UnknownAgecoeff

#sum of the coefficient times proportion for all ages
Neteffect_age$coeffpropallages<- Neteffect_age$coeffpropUndereighteen + Neteffect_age$coeffpropEighteentoTwentyfour + Neteffect_age$coeffpropTwentyfivetoThirtynine + Neteffect_age$coeffpropFortytoSixtyfour + 
  #Neteffect_age$coeffpropSixtyfivetoSeventyfour + 
  Neteffect_age$coeffpropSeventyfiveplus 
#+ Neteffect_age$coeffpropUnknownAge

#exponentiating the sum of the coefficient times proportion for all ages
Neteffect_age$expcoeffpropallages <- exp(Neteffect_age$coeffpropallages)

#drawing the graph of Net effect of sex on total attendances over time

Neteffect_age_plot <- ggplot(data=Neteffect_age, aes(x=date, y=expcoeffpropallages))+
  geom_line()+
  labs(title="Net effect of age on total attendances over time", 
       x = "Date", 
       y = "Net effect on attendances")
save_plot("Output/Neteffect_age_plot.svg", fig=Neteffect_age_plot, width=14, height=12)


##SIMD

#Net effect of SIMD on attendances
Neteffect_simd <- Covid_AE_Prop_Time_Pivot_GLM %>% 
  select(Month, SIMD1, SIMD2, SIMD3, SIMD4, SIMD5, UnknownSIMD)

#To create a date column
#creating a new column for Year, monthnumeric and day using data from the Month column
Neteffect_simd$Year <- substr(Neteffect_simd$Month, 1,4)
Neteffect_simd$monthnumeric <- substr(Neteffect_simd$Month, 5,6)
Neteffect_simd$day <- "01"

#converting from character to numeric variable
Neteffect_simd$Year <- as.numeric(Neteffect_simd$Year)
Neteffect_simd$monthnumeric <- as.numeric(Neteffect_simd$monthnumeric)
Neteffect_simd$day <- as.numeric(Neteffect_simd$day)

#making a date column using the Year, monthnumeric and day columns
Neteffect_simd<- Neteffect_simd %>% 
  mutate(date=make_date(Year, monthnumeric, day))

#calculate the difference in proportions each month
#when using diff() the number of rows reduces by one so couldn't display in the same dataframe

#creating a dataframe to calculate the difference in proportions
Neteffect_simddiff <- Neteffect_simd %>% 
  select(SIMD1, SIMD2, SIMD3, SIMD4, SIMD5, UnknownSIMD)

#to calculate the difference in proportions
Neteffect_simddiff <- sapply(Neteffect_simddiff, diff)

#to add a row for the first line of zero
firstdiffline_simd <- c(0, 0, 0, 0, 0, 0)

#to combine the line of zero with the proportion differences
Neteffect_simddiff <- rbind(firstdiffline_simd, Neteffect_simddiff) 

#changing column names so it is clear it is for diff
colnames(Neteffect_simddiff)<- c("SIMD1diff", "SIMD2diff", "SIMD3diff", "SIMD4diff", "SIMD5diff", "UnknownSIMDdiff")

#Combine original Neteffect_simd dataframe to contain the proportion differences
Neteffect_simd <- cbind(Neteffect_simd, Neteffect_simddiff)

#Calculate diff for each simd. Then times the coefficient. Then add it and then overall exponentiation. 
#Calculating the coefficient times proportion for each simd
SIMD1coeff <- -5.219
Neteffect_simd$coeffpropSIMD1 <-  Neteffect_simd$SIMD1diff*SIMD1coeff

SIMD2coeff <-  -3.753
Neteffect_simd$coeffpropSIMD2 <-  Neteffect_simd$SIMD2diff*SIMD2coeff

SIMD3coeff <-  -8.404
Neteffect_simd$coeffpropSIMD3 <-  Neteffect_simd$SIMD3diff*SIMD3coeff

SIMD4coeff <-  -13.32
Neteffect_simd$coeffpropSIMD4 <-  Neteffect_simd$SIMD4diff*SIMD4coeff

SIMD5coeff <-  -0.7546
Neteffect_simd$coeffpropSIMD5 <-  Neteffect_simd$SIMD5diff*SIMD5coeff

#UnknownSIMDcoeff <-  NA
#Neteffect_simd$coeffpropUnknownSIMD <-  Neteffect_simd$UnknownSIMDdiff*UnknownSIMDcoeff

#sum of the coefficient times proportion for all SIMD
Neteffect_simd$coeffpropallSIMD <- Neteffect_simd$coeffpropSIMD1 + Neteffect_simd$coeffpropSIMD2 + Neteffect_simd$coeffpropSIMD3 + Neteffect_simd$coeffpropSIMD4 + Neteffect_simd$coeffpropSIMD5
#+ Neteffect_simd$coeffpropUnknownSIMD

#exponentiating the sum of the coefficient times proportion for simd
Neteffect_simd$expcoeffpropallSIMD <- exp(Neteffect_simd$coeffpropallSIMD)

#drawing the graph of Net effect of simd on total attendances over time

Neteffect_simd_plot <- ggplot(data=Neteffect_simd, aes(x=date, y=expcoeffpropallSIMD))+
  geom_line()+
  labs(title="Net effect of SIMD on total attendances over time", 
       x = "Date", 
       y = "Net effect on attendances")
save_plot("Output/Neteffect_simd_plot.svg", fig=Neteffect_simd_plot, width=14, height=12)


#DEPARTMENT TYPE (ED or MIU)

#Net effect of department type on attendances
Neteffect_type <- Covid_AE_Prop_Time_Pivot_GLM %>% 
  select(Month, ED, MIU)

#To create a date column
#creating a new column for Year, monthnumeric and day using data from the Month column
Neteffect_type$Year <- substr(Neteffect_type$Month, 1,4)
Neteffect_type$monthnumeric <- substr(Neteffect_type$Month, 5,6)
Neteffect_type$day <- "01"

#converting from character to numeric variable
Neteffect_type$Year <- as.numeric(Neteffect_type$Year)
Neteffect_type$monthnumeric <- as.numeric(Neteffect_type$monthnumeric)
Neteffect_type$day <- as.numeric(Neteffect_type$day)

#making a date column using the Year, monthnumeric and day columns
Neteffect_type<- Neteffect_type %>% 
  mutate(date=make_date(Year, monthnumeric, day))

#calculate the difference in proportions each month
#when using diff() the number of rows reduces by one so couldn't display in the same dataframe

#creating a dataframe to calculate the difference in proportions
Neteffect_typediff <- Neteffect_type %>% 
  select(ED, MIU)

#to calculate the difference in proportions
Neteffect_typediff <- sapply(Neteffect_typediff, diff)

#to add a row for the first line of zero
firstdiffline_type <- c(0, 0)

#to combine the line of zero with the proportion differences
Neteffect_typediff <- rbind(firstdiffline_type, Neteffect_typediff) 

#changing column names so it is clear it is for diff
colnames(Neteffect_typediff)<- c("EDdiff", "MIUdiff")

#Combine original Neteffect_type dataframe to contain the proportion differences
Neteffect_type <- cbind(Neteffect_type, Neteffect_typediff)

#Calculate diff for each type. Then times the coefficient. Then add it and then overall exponentiation. 
#Calculating the coefficient times proportion for each type
EDcoeff <- -1.367
Neteffect_type$coeffpropED <-  Neteffect_type$EDdiff*EDcoeff

#MIUcoeff <-  NA
#Neteffect_type$coeffpropMIU<-  Neteffect_type$MIUdiff*MIUcoeff

#sum of the coefficient times proportion for each department type
Neteffect_type$coeffpropalltype <- Neteffect_type$coeffpropED
#+ Neteffect_type$coeffMIU

#exponentiating the sum of the coefficient times proportion for type
Neteffect_type$expcoeffpropalltype <- exp(Neteffect_type$coeffpropalltype)

#drawing the graph of Net effect of department type on total attendances over time

Neteffect_type_plot <- ggplot(data=Neteffect_type, aes(x=date, y=expcoeffpropalltype))+
  geom_line()+
  labs(title="Net effect of department type on total attendances over time", 
       x = "Date", 
       y = "Net effect on attendances")
save_plot("Output/Neteffect_type_plot.svg", fig=Neteffect_type_plot, width=14, height=12)


#HEALTH BOARD
#Net effect of health board on attendances
Neteffect_hb <- Covid_AE_Prop_Time_Pivot_GLM %>% 
  select(Month, NHSAyrshireandArran, NHSBorders, NHSDumfriesandGalloway, NHSForthValley, NHSGrampian, NHSHighland, NHSLothian, NHSOrkney, NHSShetland, NHSWesternIsles, NHSFife, NHSTayside, NHSGreaterGlasgowandClyde, NHSLanarkshire)

#To create a date column
#creating a new column for Year, monthnumeric and day using data from the Month column
Neteffect_hb$Year <- substr(Neteffect_hb$Month, 1,4)
Neteffect_hb$monthnumeric <- substr(Neteffect_hb$Month, 5,6)
Neteffect_hb$day <- "01"

#converting from character to numeric variable
Neteffect_hb$Year <- as.numeric(Neteffect_hb$Year)
Neteffect_hb$monthnumeric <- as.numeric(Neteffect_hb$monthnumeric)
Neteffect_hb$day <- as.numeric(Neteffect_hb$day)

#making a date column using the Year, monthnumeric and day columns
Neteffect_hb<- Neteffect_hb %>% 
  mutate(date=make_date(Year, monthnumeric, day))

#calculate the difference in proportions each month
#when using diff() the number of rows reduces by one so couldn't display in the same dataframe

#creating a dataframe to calculate the difference in proportions
Neteffect_hbdiff <- Neteffect_hb %>% 
  select(NHSAyrshireandArran, NHSBorders, NHSDumfriesandGalloway, NHSForthValley, NHSGrampian, NHSHighland, NHSLothian, NHSOrkney, NHSShetland, NHSWesternIsles, NHSFife, NHSTayside, NHSGreaterGlasgowandClyde, NHSLanarkshire)

#to calculate the difference in proportions
Neteffect_hbdiff <- sapply(Neteffect_hbdiff, diff)

#to add a row for the first line of zero
firstdiffline_hb <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

#to combine the line of zero with the proportion differences
Neteffect_hbdiff <- rbind(firstdiffline_hb, Neteffect_hbdiff) 

#changing column names so it is clear it is for diff
colnames(Neteffect_hbdiff)<- c("NHSAyrshireandArrandiff", "NHSBordersdiff", "NHSDumfriesandGallowaydiff", "NHSForthValleydiff", "NHSGrampiandiff", "NHSHighlanddiff", "NHSLothiandiff", "NHSOrkneydiff", "NHSShetlanddiff", "NHSWesternIslesdiff", "NHSFifediff", "NHSTaysidediff", "NHSGreaterGlasgowandClydediff", "NHSLanarkshirediff")

#Combine original Neteffect_hb dataframe to contain the proportion differences
Neteffect_hb <- cbind(Neteffect_hb, Neteffect_hbdiff)

#Calculate diff for each hb. Then times the coefficient. Then add it and then overall exponentiation. 
#Calculating the coefficient times proportion for each hb
NHSAyrshireandArrancoeff <- 15.47
Neteffect_hb$coeffpropNHSAyrshireandArran <-  Neteffect_hb$NHSAyrshireandArrandiff*NHSAyrshireandArrancoeff

NHSBorderscoeff <-  -7.949
Neteffect_hb$coeffpropNHSBorders <-  Neteffect_hb$NHSBordersdiff*NHSBorderscoeff

NHSFifecoeff <-  4.307
Neteffect_hb$coeffpropNHSFife <-  Neteffect_hb$NHSFifediff*NHSFifecoeff

NHSDumfriesandGallowaycoeff <-  0.4295
Neteffect_hb$coeffpropNHSDumfriesandGalloway <-  Neteffect_hb$NHSDumfriesandGallowaydiff*NHSDumfriesandGallowaycoeff

NHSForthValleycoeff <-  18.46
Neteffect_hb$coeffpropNHSForthValley <-  Neteffect_hb$NHSForthValleydiff*NHSForthValleycoeff

NHSGrampiancoeff <-  -1.458
Neteffect_hb$coeffpropNHSGrampian <-  Neteffect_hb$NHSGrampiandiff*NHSGrampiancoeff

NHSHighlandcoeff <-  7.609
Neteffect_hb$coeffpropNHSHighland <-  Neteffect_hb$NHSHighlanddiff*NHSHighlandcoeff

#NHSLothiancoeff <-  NA
#Neteffect_hb$coeffpropNHSLothian <-  Neteffect_hb$NHSLothiandiff*NHSLothiancoeff

NHSOrkneycoeff <-  59.08
Neteffect_hb$coeffpropNHSOrkney <-  Neteffect_hb$NHSOrkneydiff*NHSOrkneycoeff

NHSShetlandcoeff <-  -20.99
Neteffect_hb$coeffpropNHSShetland <-  Neteffect_hb$NHSShetlanddiff*NHSShetlandcoeff

NHSWesternIslescoeff <-  11.69
Neteffect_hb$coeffpropNHSWesternIsles <-  Neteffect_hb$NHSWesternIslesdiff*NHSWesternIslescoeff

NHSTaysidecoeff <-  13.75
Neteffect_hb$coeffpropNHSTayside <-  Neteffect_hb$NHSTaysidediff*NHSTaysidecoeff

NHSGreaterGlasgowandClydecoeff <-  14.48
Neteffect_hb$coeffpropNHSGreaterGlasgowandClyde <-  Neteffect_hb$NHSGreaterGlasgowandClydediff*NHSGreaterGlasgowandClydecoeff

NHSLanarkshirecoeff <-  10.06
Neteffect_hb$coeffpropNHSLanarkshire <-  Neteffect_hb$NHSLanarkshirediff*NHSLanarkshirecoeff

#sum of the coefficient times proportion for all SIMD
Neteffect_hb$coeffpropallhb <- Neteffect_hb$coeffpropNHSAyrshireandArran + Neteffect_hb$coeffpropNHSBorders + Neteffect_hb$coeffpropNHSFife + Neteffect_hb$coeffpropNHSDumfriesandGalloway + Neteffect_hb$coeffpropNHSForthValley + Neteffect_hb$coeffpropNHSGrampian + Neteffect_hb$coeffpropNHSHighland + Neteffect_hb$coeffpropNHSOrkney + Neteffect_hb$coeffpropNHSShetland + Neteffect_hb$coeffpropNHSWesternIsles + Neteffect_hb$coeffpropNHSTayside + Neteffect_hb$coeffpropNHSGreaterGlasgowandClyde + Neteffect_hb$coeffpropNHSLanarkshire
#+ Neteffect_hb$coeffpropNHSLothian

#exponentiating the sum of the coefficient times proportion for simd
Neteffect_hb$expcoeffpropallhb <- exp(Neteffect_hb$coeffpropallhb)

#drawing the graph of Net effect of health board on total attendances over time

Neteffect_hb_plot <- ggplot(data=Neteffect_hb, aes(x=date, y=expcoeffpropallhb))+
  geom_line()+
  labs(title="Net effect of health board on total attendances over time", 
       x = "Date", 
       y = "Net effect on attendances")
save_plot("Output/Neteffect_hb_plot.svg", fig=Neteffect_hb_plot, width=14, height=12)


#DAY OF THE WEEK

#Net effect of day of the week on attendances
Neteffect_day <- Covid_AE_Prop_Time_Pivot_GLM %>% 
  select(Month, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday)

#To create a date column
#creating a new column for Year, monthnumeric and day using data from the Month column
Neteffect_day$Year <- substr(Neteffect_day$Month, 1,4)
Neteffect_day$monthnumeric <- substr(Neteffect_day$Month, 5,6)
Neteffect_day$day <- "01"

#converting from character to numeric variable
Neteffect_day$Year <- as.numeric(Neteffect_day$Year)
Neteffect_day$monthnumeric <- as.numeric(Neteffect_day$monthnumeric)
Neteffect_day$day <- as.numeric(Neteffect_day$day)

#making a date column using the Year, monthnumeric and day columns
Neteffect_day<- Neteffect_day %>% 
  mutate(date=make_date(Year, monthnumeric, day))

#calculate the difference in proportions each month
#when using diff() the number of rows reduces by one so couldn't display in the same dataframe

#creating a dataframe to calculate the difference in proportions
Neteffect_daydiff <- Neteffect_day %>% 
  select(Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday)

#to calculate the difference in proportions
Neteffect_daydiff <- sapply(Neteffect_daydiff, diff)

#to add a row for the first line of zero
firstdiffline_day <- c(0, 0, 0, 0, 0, 0, 0)

#to combine the line of zero with the proportion differences
Neteffect_daydiff <- rbind(firstdiffline_day, Neteffect_daydiff) 

#changing column names so it is clear it is for diff
colnames(Neteffect_daydiff)<- c("Mondaydiff", "Tuesdaydiff", "Wednesdaydiff", "Thursdaydiff", "Fridaydiff", "Saturdaydiff", "Sundaydiff")

#Combine original Neteffect_day dataframe to contain the proportion differences
Neteffect_day <- cbind(Neteffect_day, Neteffect_daydiff)

#Calculate diff for each day. Then times the coefficient. Then add it and then overall exponentiation. 
#Calculating the coefficient times proportion for each day
Mondaycoeff <- 1.244
Neteffect_day$coeffpropMonday <-  Neteffect_day$Mondaydiff*Mondaycoeff

Tuesdaycoeff <- -0.6717
Neteffect_day$coeffpropTuesday <-  Neteffect_day$Tuesdaydiff*Tuesdaycoeff

Wednesdaycoeff <- 0.8354
Neteffect_day$coeffpropWednesday <-  Neteffect_day$Wednesdaydiff*Wednesdaycoeff

Thursdaycoeff <- 0.3252
Neteffect_day$coeffpropThursday <-  Neteffect_day$Thursdaydiff*Thursdaycoeff

Fridaycoeff <- 0.3131
Neteffect_day$coeffpropFriday <-  Neteffect_day$Fridaydiff*Fridaycoeff

Saturdaycoeff <- -0.3669
Neteffect_day$coeffpropSaturday <-  Neteffect_day$Saturdaydiff*Saturdaycoeff

#Sundaycoeff <- NA
#Neteffect_day$coeffpropSunday <-  Neteffect_day$Sundaydiff*Sundaycoeff


#sum of the coefficient times proportion for all days of the week
Neteffect_day$coeffpropallday <- Neteffect_day$coeffpropMonday + Neteffect_day$coeffpropTuesday + Neteffect_day$coeffpropWednesday + Neteffect_day$coeffpropThursday + Neteffect_day$coeffpropFriday + Neteffect_day$coeffpropSaturday
#+ Neteffect_day$coeffpropSunday

#exponentiating the sum of the coefficient times proportion for day
Neteffect_day$expcoeffpropallday <- exp(Neteffect_day$coeffpropallday)

#drawing the graph of Net effect of day of the week on total attendances over time

Neteffect_day_plot <- ggplot(data=Neteffect_day, aes(x=date, y=expcoeffpropallday))+
  geom_line()+
  labs(title="Net effect of day of the week on total attendances over time", 
       x = "Date", 
       y = "Net effect on attendances")
save_plot("Output/Neteffect_day_plot.svg", fig=Neteffect_day_plot, width=14, height=12)
