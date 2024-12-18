# Usher A&E Scotland Masters Project
Welcome to the repository which will hold all work relating to the Usher masters project on A&amp;E data in Scotland.

AE = Accident and emergency

ED = Emergency department

MIU = Minor injuries unit

Scottish A&E data.R  - contains all the code which were subsequently broken down into the following:

1. ScottishAEattendances - code to analyse overall A&E attendance numbers
2. ScottishAEagegrp - analysis on A&E attendances and age
3. ScottishAEdeprivation - analysis on A&E attendances and deprivation
4. ScottishAEwaitingtimes - analysis on A&E waiting times
5. ScottishAEattendancesheatmap - creation of A&E attendance heatmaps
6. ScottishAEpopestimate_attedancerate - analysis for A&E attendances (using population data). Where the code refers to Updatedpopulation, it includes population estimates for 2022 and 2023 based on Chris's model 
7. ScottishAEdischargedestination - analysis on discharge destination
8. ScottishAElogisticregression - analysis on logistic regression
9. ScottishAEglm - modelling using GLM
10. ScottishAEpredict - predictions using the GLM model
11. ScottishAEChoropleth - creation of choropleths on A&E attendances and waiting times

For COVID-19 work
- ScottishAECovidattendancerate - run this first to get the attendance rates used in the Choropleth
- ScottishAECovidDescriptiveStatistics - basic analysis of the data, restricted by date
- ScottishAECovidChoropleth - to create the attendance rate and waiting times choropleth for 2018, 2020 and 2022
- ScottishAECovidGLM - to be run first before the other GLMs. To create the dataframe containing the proportions
- ScottishAECovidGLM2 - to be run before ScottishAECovidGLM3. to create the GLM models
- ScottishAECovidGLM3 - to create the predictions/graphs
