#Load packages
library(ggplot2)
library(lubridate)
library(tidyverse)
library(dplyr)

#Set working directory
setwd("/Users/chloebutler/Desktop/Tb analysis/clean sorted body temp data/")
V <- read.csv("V_sorted.csv", header = TRUE)
setwd("/Users/chloebutler/Downloads/")
trialdate <- read.csv("trialdate.csv", header = TRUE)

V$Trial <- 3
tempV <- merge(V, trialdate, by="Trial", all.x=FALSE, all.y=TRUE)

subset_V <- filter(tempV, keep == "Keep")



subset_V$DT <- as.POSIXct(subset_V$DT, tz = "", format = "%Y-%m-%d %H:%M:%S")

# Filter for the 10 days before injection
subset_VV <- subset_V %>%
  filter(DT >= as.POSIXct("2023-05-01 00:00:00") & DT <= as.POSIXct("2023-05-11 23:59:59"))

# Create a new variable for time in hours
subset_VV$Time <- as.numeric(format(subset_VV$DT,"%H")) + as.numeric(format(subset_VV$DT,"%M"))/60 + as.numeric(format(subset_VV$DT,"%OS"))/3600

xmin<- min(subset_VV$DT)
xmax <- max(subset_VV$DT)

# Plotting the 10 days before primary injection
ggplot(data = subset_VV) +
  geom_line(aes(x = DT, y = Temp)) + 
  scale_y_continuous(limits = c(30, 40)) + 
  xlim(xmin, xmax) +
  geom_abline(slope = 0, intercept = 36.5, col = "red") 


# Filter for 4 hour rest window
subset_VVV <- subset_V %>%
  filter(DT >= as.POSIXct("2023-05-10 08:00:00") & DT <= as.POSIXct("2023-05-10 12:00:00"))

# Collect min, max, mean, and median temp values for the window
ymin <- min(subset_VVV$Temp)
ymax <-max(subset_VVV$Temp)
ymedian <- median(subset_VVV$Temp)
ymean <- mean(subset_VVV$Temp)




