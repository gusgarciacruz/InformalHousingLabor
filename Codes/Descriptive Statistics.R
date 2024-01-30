###Authors: García-Badillo-Aristizábal (2024)
###Title: Labor informality and housing informality in space: in search of the missing links


#Descriptive statistics

data <- read.csv ("García-Badillo-Aristizábal.csv")
attach(data)

#Table 1. Descriptive statistics of informality variables
tapply(Housing_informality, year, mean)
tapply(Labor_informality, year, mean)
tapply(Housing_informality, year, sd)
tapply(Labor_informality, year, sd)
tapply(Housing_informality, year, median)
tapply(Labor_informality, year, median)
tapply(Housing_informality, year, min)
tapply(Labor_informality, year, min)
tapply(Housing_informality, year, max)
tapply(Labor_informality, year, max)
tapply(Housing_informality, year, quantile, p=0.25)
tapply(Labor_informality, year, quantile, p=0.25)
tapply(Housing_informality, year, quantile, p=0.75)
tapply(Labor_informality, year, quantile, p=0.75)

#Table 2. Descriptive statistics of control variables
tapply(Female_household, year, mean)
tapply(Tertiary_education, year, mean)
tapply(Unemployment, year, mean)
tapply(Children, year, mean)
tapply(Density, year, mean)

tapply(Female_household, year, sd)
tapply(Tertiary_education, year, sd)
tapply(Unemployment, year, sd)
tapply(Children, year, sd)
tapply(Density, year, sd)

tapply(Female_household, year, median)
tapply(Tertiary_education, year, median)
tapply(Unemployment, year, median)
tapply(Children, year, median)
tapply(Density, year, median)

tapply(Female_household, year, min)
tapply(Tertiary_education, year, min)
tapply(Unemployment, year, min)
tapply(Children, year, min)
tapply(Density, year, min)

tapply(Female_household, year, max)
tapply(Tertiary_education, year, max)
tapply(Unemployment, year, max)
tapply(Children, year, max)
tapply(Density, year, max)

tapply(Female_household, year, quantile, p=0.25)
tapply(Tertiary_education, year, quantile, p=0.25)
tapply(Unemployment, year, quantile, p=0.25)
tapply(Children, year, quantile, p=0.25)
tapply(Density, year, quantile, p=0.25)

tapply(Female_household, year, quantile, p=0.75)
tapply(Tertiary_education, year, quantile, p=0.75)
tapply(Unemployment, year, quantile, p=0.75)
tapply(Children, year, quantile, p=0.75)
tapply(Density, year, quantile, p=0.75)
