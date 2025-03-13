## ROLLING WINDOW FOR EACH PIT TAG

#Packages
library(zoo)
library(ggplot2)
library(lubridate)

#Read in data 
setwd("/Users/chloebutler/Downloads") 
Tb <- read.csv("rawtemp.csv", header= TRUE)

Tb$Tag.ID <- as.factor(Tb$Tag.ID)

#Subset data to be just one individual's pit tag
d <- subset(Tb, Tb$Tag.ID =="2A8F2C83")


d$DT <- paste(d$Date, d$Time)
sorted_dE <- d[order(d$DT), ]


#Assign window length
window_width <- 11

test <- c(1:length(sorted_dE$Temp))
n_iterations <- length(sorted_dE$Temp) - 4  
# number of iterations. subtract 9 so that we have 10 values in each window

keep_or_discard <- character(n_iterations)
sd_val_storage <- numeric(n_iterations)
median_val_storage <- numeric(n_iterations)

# Loop through each window
for (i in test) {
  # take the values in the window
  window_values <- sorted_dE$Temp[i:(i + window_width - 1)]
  window_values <- as.numeric(window_values)
  
  # calc median and standard deviation for the window, excluding missing values
  median_val <- median(window_values, na.rm = TRUE)
  median_val <- as.numeric(median_val)
  sd_val <- sd(window_values, na.rm = TRUE)
  
  sd_val_storage[i] <- sd_val
  median_val_storage[i] <- median_val
}

for (i in test) {
  # take the values in the window
  window_values <- sorted_dE$Temp[i:(i + window_width - 1)]
  window_values <- as.numeric(window_values)
  
  # calc median and standard deviation for the window, excluding missing values
  median_val <- median(window_values, na.rm = TRUE)
  median_val <- as.numeric(median_val)
  
  low_bound <- median_val - (1.5*median(sd_val_storage[-which(is.na(sd_val_storage))]))
  high_bound <- median_val + (1.5*median(sd_val_storage[-which(is.na(sd_val_storage))]))
  
  test_value <- window_values[6]
  if (!is.na(test_value) && !is.na(median_val)) {
    if (test_value > low_bound && test_value < high_bound) {
      keep_or_discard[i+5] <- "Keep"
    } else {
      keep_or_discard[i+5] <- "Discard"
    }
  } else {
    keep_or_discard[i+5] <- "discard"  # Discard if any missing value
  }
}
keep_or_discard1 <- keep_or_discard[-c(32441:32445)]
sorted_dE$keep <- keep_or_discard1
summary(as.factor(keep_or_discard)) ## number of keep/discards



##Histograms of keep vs discarded values
hist(as.numeric(sorted_dE$Temp[which(keep_or_discard=="Keep")]))
hist(as.numeric(sorted_dE$Temp[which(keep_or_discard=="Discard")]))
hist(as.numeric(sorted_dE$Temp))



#set up injection date & time difference DT from inj.
sorted_dE$DT <- as.POSIXct(sorted_dE$DT, format = "%m/%d/%Y %H:%M:%S")
sorted_dE$Temp <- as.numeric(sorted_dE$Temp)
inj.d <- rep(as.POSIXct('3/13/2023 18:00:00', format="%m/%d/%Y %H:%M:%S"))

xE <- cbind(sorted_dE,inj.d)
keep_or_discard2 <- keep_or_discard[-c(32441:32445)]
xE$keep <- keep_or_discard2
xE$timediff <- as.numeric(difftime(xE$DT, xE$inj.d, units = "hours"))



##graph unsorted data w/ ggplot 
xE$Temp <- as.numeric(xE$Temp)

graph <- ggplot(data = xE) +
  geom_line(aes(x = DT, y = Temp)) +
  scale_y_continuous(limits = c(34,40)) +
  geom_abline(slope = 0, intercept = 36.5, col = "red") 
graph



##graph sorted "keep" data w/ ggplot 
xE$Temp <- as.numeric(xE$Temp)

graph <- ggplot(data = xE[which(sorted_dE$keep=="Keep"),]) +
  geom_line(aes(x = DT, y = Temp)) +
  scale_y_continuous(limits = c(34,40)) +
  geom_abline(slope = 0, intercept = 36.5, col = "red") 
graph


#Include only 168 hrs before & after injection
E168 <- subset(xE, timediff < 168 & timediff > -168)
E168$Temp <- as.numeric(E168$Temp)
E168$Tag.ID<-as.factor(E168$Tag.ID) 
E168$Time<-as.factor(E168$Time) 

sortedtimediffplot <- ggplot(data = E168[which(E168$keep=="Keep"),]) +
  geom_line(aes(x = DT, y = Temp)) +
  scale_y_continuous(limits = c(34,40)) +
  geom_abline(slope = 0, intercept = 36.5, col = "red") 
sortedtimediffplot



