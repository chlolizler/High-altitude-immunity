#Packages
library(lubridate)
library(tidyverse)
library(dplyr)
library(mgcv)
library(MuMIn)

#The following process was done for each individual in the trials. The saline controls were grouped into a separate analysis
#Data for each individual was separated into pre- and post-injection

#Load both pre- and post-injection data
setwd("/Users/chloebutler/Desktop/Tb analysis/pre-inj Tb/")
prA <- read.csv("pre-A.csv", header = TRUE)
setwd("/Users/chloebutler/Desktop/Tb analysis/post-inj Tb/")
poA <- read.csv("post-A.csv", header = TRUE)


#Turn time of day into a decimal
prA$Time2 <- hour(prA$DT) + minute(prA$DT)/60 + second(prA$DT)/3600
poA$Time2 <- hour(poA$DT) + minute(poA$DT)/60 + second(poA$DT)/3600

#Use the pre-injection data to make a gam that'll predict body temperature  
prAgamm <- gam(Temp ~ s(Time2), family = "gaussian", data = prA, 
               method = "REML", 
               na.action = na.exclude)

#Use post-injection data as the "actual" values after LPS exposure
postInjPredictA <- poA$Time2
postInjPredictdfA <- as.data.frame(postInjPredictA)
colnames(postInjPredictdfA) <-"Time2"

predicteddataA <- predict(prAgamm, postInjPredictdfA)


#subtract the predicted gam values from the actual post-injected values to get residual/"difference" values
resultsA <- data.frame(
  ID = poA$Tag.ID,
  Pop = poA$Population,
  actual = poA$Temp,   
  predicted = predicteddataA,  
  difference = poA$Temp- predicteddataA,  
  TRI = poA$TRI
)

head(resultsA)

hist(resultsA$difference)


#once done for all individuals, they were grouped by population


