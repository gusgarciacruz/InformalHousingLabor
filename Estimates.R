###Authors: García-Badillo-Aristizábal (2024)
###Title: Labor informality and housing informality in space: in search of the missing links

# clean environment
rm(list=ls())
library(pacman)
p_load(leaflet, rgdal, spdep, spsur, sphet, systemfit, Matrix, dplyr, sf)

# [Preliminary] Distance calculation #
setwd("")

Medellin = st_read("Medellin_Map.shp")
plot(Medellin)

# create city centers  
Medellin_cbd = st_as_sf(x = read.table(text="-75.57157 6.25672"),
                       coords = c(1,2), crs = "+proj=longlat +datum=WGS84")

# put everything in the same projection
Medellin_cbd = Medellin_cbd %>% st_transform(st_crs(Medellin))

# view data
leaflet(Medellin) %>% addTiles() %>% 
  addPolygons(color="green",fill=NA,weight=2) %>% addCircleMarkers(data=Medellin_cbd,col="red",weight=3)

# create distances
Medellin$dist_CBD = st_distance(Medellin,Medellin_cbd) 

# 
Medellin$dist_CBD = as.numeric(Medellin$dist_CBD)*0.000621371 
dist_CBD=subset(x = Medellin)
summary(Medellin$dist_CBD)
sd(Medellin$dist_CBD, na.rm = TRUE)

#Import data
setwd("")
data<-read.csv("DataGarcia_Badillo_Aristizabal.csv")

#[1] Create population density variable
data$Density<-data$Population/data$Area

#[2]
data$Distance<-as.double(data$dist_CBD)


#Import map
setwd("")
med<-readOGR("Medellin_Map.shp", layer="Medellin_Map")
plot(med)
View(med)

#Create the contact matrix (Order 1)
nb<-poly2nb(med,queen=T)
#Standardize the matrix w
We<-nb2listw(nb, glist=NULL, style="W")


#Create the contact matrix (Order 2)
W2 <- nblag(nb, 2)

#Standardize the matrix w
#The nblag function creates two objects. 
#The second one that has the information about the second-order neighbors is taken.
We2<-nb2listw(W2[[2]], glist=NULL, style="W")

##Create the spatially lagged instruments
#First order lag
data$wDistance<-lag.listw(We, zero.policy=T, data$Distance)
data$welev<-lag.listw(We, zero.policy=T, data$elevation)

#Second order lag
data$w2Distance<-lag.listw(We2, zero.policy=T, data$Distance)
data$w2elev<-lag.listw(We2, zero.policy=T, data$elevation)


###
#Model for calculating LM tests
#Instrumental variables estimation without spatial effects

ols1<-lm(Labor_informality~ Distance + Tertiary_education + Female_household  + Unemployment + Density + Children, data=data)
summary(ols1)
data$Labor_informality_hat<-fitted(ols1)

ols2<-lm(Housing_informality~ Housing_informality_2011 + Tertiary_education + Female_household  + Unemployment + Density + Children, data=data)
summary(ols2)
data$Housing_informality_hat<-fitted(ols2)

##
tformula1 <- Housing_informality | Labor_informality  ~ 
  Labor_informality_hat + Tertiary_education + Female_household  + Unemployment + Density + Children| 
  Housing_informality_hat          + Tertiary_education + Female_household  + Unemployment + Density + Children 

#LM Test
lmtestspsur(formula = tformula1, data = data, listw = We)


###
#Create equation
tformula2 <- Housing_informality | Labor_informality  ~ 
   Tertiary_education + Female_household  + Unemployment + Density + Children | 
   Tertiary_education + Female_household  + Unemployment + Density + Children                 

################################################################
## Endogenous regressors: Housing_informality , Labor_informality

## A sim model with endogenous regressors 
#spsim <- spsurgs3sls(formula = tformula2, data = data,
#                     type = "sim", listw = We)
#summary(spsim)

## A IV model with endogenous regressors 
#spciv <- spsurgs3sls(formula = tformula2, data = data,
#                     type = "sim", listw = We)
#summary(spciv)
#################################################################


## A IV model with endogenous regressors (1 and 2 Columns)
spciv <- spsurgs3sls(formula = tformula2, data = data,
                     type = "iv", listw = We,
                     endog = ~ Labor_informality | Housing_informality, 
                     instruments = ~ Distance | Housing_informality_2011)

summary(spciv)


## A SLM model with endogenous regressor (3 and 4 Columns)
spcslm <- spsurgs3sls(formula = tformula2, data = data,
                      type = "slm", listw = We,
                      endog = ~ Labor_informality | Housing_informality, 
                      instruments = ~ Distance | Housing_informality_2011)
summary(spcslm)

## A SEM model with endogenous regressors (5 and 6 Columns)
spcsem <- spsurgs3sls(formula = tformula2, data = data,
                      type = "sem", listw = We,
                      endog = ~ Labor_informality | Housing_informality, 
                      instruments = ~ Distance | Housing_informality_2011)
summary(spcsem)


## A SARAR model with endogenous regressors (7 and 8 Columns)
spsarar <- spsurgs3sls(formula = tformula2, data = data,
                       type = "sarar", listw = We,
                       endog = ~ Labor_informality | Housing_informality, 
                       instruments = ~ Distance | Housing_informality_2011)
summary(spsarar)


## A SDM model with endogenous regressors (9 and 10 Columns)
spsdm <- spsurgs3sls(formula = tformula2, data = data,
                     type = "sdm", listw = We,
                     endog = ~ Labor_informality | Housing_informality, 
                     instruments = ~ Distance| Housing_informality_2011,
                     Durbin= ~ Labor_informality + Tertiary_education + Female_household  + Unemployment + Density + Children |
                       Housing_informality + Tertiary_education + Female_household  + Unemployment + Density + Children) 
summary(spsdm)

#Spillover efeccts
#SLM
impacts_spcslm <- impactspsur(spcslm, listw = We, R = 1000)
summary(impacts_spcslm[[1]], zstats = TRUE, short = TRUE)
summary(impacts_spcslm[[2]], zstats = TRUE, short = TRUE)


#SARAR
impacts_spcsarar <- impactspsur(spsarar, listw = We, R = 1000)
summary(impacts_spcsarar[[1]], zstats = TRUE, short = TRUE)
summary(impacts_spcsarar[[2]], zstats = TRUE, short = TRUE)

#SDM
impacts_spsdm <- impactspsur(spsdm, listw = We, R = 1000)
summary(impacts_spsdm[[1]], zstats = TRUE, short = TRUE)
summary(impacts_spsdm[[2]], zstats = TRUE, short = TRUE)
