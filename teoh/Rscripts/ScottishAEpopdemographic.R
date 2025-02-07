#Scottish A&E data - Illustration of population demographics
#ScottishAECovidpopdemographic

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

#POPULATION DEMOGRAPHICS

#Loading population estimate (only till 2022) csv file
Covid_HBpopulation_demographic <- read_csv(here("Rawdata", "20240809Popestimate_HB2019_1981to2022.csv"))

#Filtering out total Scottish estimates for 2022. Note HB=S92000003 is for all of Scotland.
Covid_HBpopulation_demographic <- Covid_HBpopulation_demographic %>%
  filter(Year=="2022")

#Population breakdown by sex in 2022 for S92000003 (whole of Scotland)
Covid_HBpopulation_demographic_sex <- 
  Covid_HBpopulation_demographic %>%
  select(Year, HB, Sex, AllAges) %>% 
    filter(HB=="S92000003", !Sex=="All")

#Population breakdown by age ranges under18, 18-24, 25-39, 40-64, 65-74, 75plus
Covid_HBpopulation_demographic_age <- Covid_HBpopulation_demographic %>%
  filter(HB=="S92000003", Sex=="All")

#sum of under 18s = 1006212, 18.312%
sum(Covid_HBpopulation_demographic_age$Age0,Covid_HBpopulation_demographic_age$Age1,Covid_HBpopulation_demographic_age$Age2, Covid_HBpopulation_demographic_age$Age3,Covid_HBpopulation_demographic_age$Age4,Covid_HBpopulation_demographic_age$Age5,Covid_HBpopulation_demographic_age$Age6,Covid_HBpopulation_demographic_age$Age7,Covid_HBpopulation_demographic_age$Age8,Covid_HBpopulation_demographic_age$Age9,Covid_HBpopulation_demographic_age$Age10,Covid_HBpopulation_demographic_age$Age11,Covid_HBpopulation_demographic_age$Age12,Covid_HBpopulation_demographic_age$Age13,Covid_HBpopulation_demographic_age$Age14,Covid_HBpopulation_demographic_age$Age15,Covid_HBpopulation_demographic_age$Age16,Covid_HBpopulation_demographic_age$Age17)
sum((1006212/(1006212+466541+1044051+1880687+599485+497910))*100)

#sum of 18-24 = 466541, 8.490%
sum(Covid_HBpopulation_demographic_age$Age18,Covid_HBpopulation_demographic_age$Age19,Covid_HBpopulation_demographic_age$Age20, Covid_HBpopulation_demographic_age$Age21,Covid_HBpopulation_demographic_age$Age22,Covid_HBpopulation_demographic_age$Age23,Covid_HBpopulation_demographic_age$Age24)
sum((466541/(1006212+466541+1044051+1880687+599485+497910))*100)

#sum of 25-39 = 1044051, 19.000%
sum(Covid_HBpopulation_demographic_age$Age25,Covid_HBpopulation_demographic_age$Age26,Covid_HBpopulation_demographic_age$Age27, Covid_HBpopulation_demographic_age$Age28,Covid_HBpopulation_demographic_age$Age29,Covid_HBpopulation_demographic_age$Age30,Covid_HBpopulation_demographic_age$Age31,Covid_HBpopulation_demographic_age$Age32,Covid_HBpopulation_demographic_age$Age33,Covid_HBpopulation_demographic_age$Age34,Covid_HBpopulation_demographic_age$Age35,Covid_HBpopulation_demographic_age$Age36,Covid_HBpopulation_demographic_age$Age37,Covid_HBpopulation_demographic_age$Age38,Covid_HBpopulation_demographic_age$Age39)
sum((1044051/(1006212+466541+1044051+1880687+599485+497910))*100)

#sum of 40-64 = 1880687, 34.226%
sum(Covid_HBpopulation_demographic_age$Age0,Covid_HBpopulation_demographic_age$Age40,Covid_HBpopulation_demographic_age$Age41, Covid_HBpopulation_demographic_age$Age42,Covid_HBpopulation_demographic_age$Age43,Covid_HBpopulation_demographic_age$Age44,Covid_HBpopulation_demographic_age$Age45,Covid_HBpopulation_demographic_age$Age46,Covid_HBpopulation_demographic_age$Age47,Covid_HBpopulation_demographic_age$Age48,Covid_HBpopulation_demographic_age$Age49,Covid_HBpopulation_demographic_age$Age50,Covid_HBpopulation_demographic_age$Age51,Covid_HBpopulation_demographic_age$Age52,Covid_HBpopulation_demographic_age$Age53,Covid_HBpopulation_demographic_age$Age54,Covid_HBpopulation_demographic_age$Age55,Covid_HBpopulation_demographic_age$Age56, Covid_HBpopulation_demographic_age$Age57, Covid_HBpopulation_demographic_age$Age58, Covid_HBpopulation_demographic_age$Age59, Covid_HBpopulation_demographic_age$Age60, Covid_HBpopulation_demographic_age$Age61, Covid_HBpopulation_demographic_age$Age62, Covid_HBpopulation_demographic_age$Age63, Covid_HBpopulation_demographic_age$Age64)
sum((1880687/(1006212+466541+1044051+1880687+599485+497910))*100)

#sum of 65-74 = 599485, 3.630%
sum(Covid_HBpopulation_demographic_age$Age65,Covid_HBpopulation_demographic_age$Age66,Covid_HBpopulation_demographic_age$Age67, Covid_HBpopulation_demographic_age$Age68,Covid_HBpopulation_demographic_age$Age69,Covid_HBpopulation_demographic_age$Age70,Covid_HBpopulation_demographic_age$Age71,Covid_HBpopulation_demographic_age$Age72,Covid_HBpopulation_demographic_age$Age73,Covid_HBpopulation_demographic_age$Age74)
sum((199485/(1006212+466541+1044051+1880687+599485+497910))*100)

#sum of 75 plus = 497910, 9.061%
sum(Covid_HBpopulation_demographic_age$Age75,Covid_HBpopulation_demographic_age$Age76,Covid_HBpopulation_demographic_age$Age77, Covid_HBpopulation_demographic_age$Age78,Covid_HBpopulation_demographic_age$Age79,Covid_HBpopulation_demographic_age$Age80,Covid_HBpopulation_demographic_age$Age81,Covid_HBpopulation_demographic_age$Age82,Covid_HBpopulation_demographic_age$Age83,Covid_HBpopulation_demographic_age$Age84,Covid_HBpopulation_demographic_age$Age85,Covid_HBpopulation_demographic_age$Age86,Covid_HBpopulation_demographic_age$Age87,Covid_HBpopulation_demographic_age$Age88,Covid_HBpopulation_demographic_age$Age89,Covid_HBpopulation_demographic_age$Age90plus)
sum((497910/(1006212+466541+1044051+1880687+599485+497910))*100)

#SIMD - data from National Records Scotland population estimates by SIMD in 2020 by NHS board area for 2021
#Loading population estimate by SIMD
Covid_HBpopulation_demographic_SIMD <- read_excel(here("Rawdata", "population-estimates_by_scottish_index_of_multiple_deprivation_simd_2020_decile_by_nhs_board_area_2001_2021.xlsx"), sheet = "2021", skip = 3)
Covid_HBpopulation_demographic_SIMD <- Covid_HBpopulation_demographic_SIMD %>% 
  select("Health board area", "SIMD decile", "Total") 

Covid_HBpopulation_demographic_SIMD <- Covid_HBpopulation_demographic_SIMD %>% 
  group_by(`SIMD decile`) %>% 
  summarise(Total=sum(Total))

colnames(Covid_HBpopulation_demographic_SIMD)<-c("SIMDdecile", "Total", "SIMDquintile")

#Population estimates in 2020 were published as deciles (SIMD 1-10), therefore I combined them to make it into quintiles (SIMD1-5)
Covid_HBpopulation_demographic_SIMD$SIMDquintile <- 
  ifelse((Covid_HBpopulation_demographic_SIMD$SIMDdecile=="1"), '1',
         ifelse((Covid_HBpopulation_demographic_SIMD$SIMDdecile=="2"), '1',
         ifelse((Covid_HBpopulation_demographic_SIMD$SIMDdecile=="3"), '2',
        ifelse((Covid_HBpopulation_demographic_SIMD$SIMDdecile=="4"), '2',
         ifelse((Covid_HBpopulation_demographic_SIMD$SIMDdecile=="5"), '3',
         ifelse((Covid_HBpopulation_demographic_SIMD$SIMDdecile=="6"), '3',
         ifelse((Covid_HBpopulation_demographic_SIMD$SIMDdecile=="7"), '4',
         ifelse((Covid_HBpopulation_demographic_SIMD$SIMDdecile=="8"), '4',
         ifelse((Covid_HBpopulation_demographic_SIMD$SIMDdecile=="9"), '5',
         ifelse((Covid_HBpopulation_demographic_SIMD$SIMDdecile=="10"), '5', 'Scotlandtotal'))))))))))

Covid_HBpopulation_demographic_SIMD <- Covid_HBpopulation_demographic_SIMD %>% 
  group_by(SIMDquintile) %>% 
  summarise(Total=sum(Total))

Covid_HBpopulation_demographic_SIMD$Percentage <- Covid_HBpopulation_demographic_SIMD %>% 
  sum((Covid_HBpopulation_demographic_SIMD$Total/10959800)*100)

#%SIMD1 =19.21524
sum((2105952/10959800)*100)
#%SIMD2 =19.27798
sum((2112828/10959800)*100)
#%SIMD3 =19.75948
sum((2165600/10959800)*100)
#%SIMD4 =21.10048
sum((2312570/10959800)*100)
#%SIMD5 =20.64682
sum((2262850/10959800)*100)
