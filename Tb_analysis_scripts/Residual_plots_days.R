#Packages
library(ggplot2)
library(lubridate)
library(tidyverse)
library(dplyr)
library(mgcv)
library(MuMIn)
library(ggeffects)
library(ggpubr)

#Load residuals data data
setwd("/Users/chloebutler/Desktop/Tb analysis/residuals")
LOW <- read.csv("LOWresid.csv", header = TRUE)
HIGH <- read.csv("HIGHresid.csv", header = TRUE)
SALmerge <- read.csv("salineresid.csv", header = TRUE)


#splitting lowlanders into daily blocks
KNfirstday <- LOW %>% 
  filter(TRI >= 0 & TRI <= 24)
KNsecondday <- LOW %>% 
  filter(TRI >= 24 & TRI <= 48)
KNthirdday <- LOW %>% 
  filter(TRI >= 48 & TRI <= 72)
KNfourthday <- LOW %>% 
  filter(TRI >= 72 & TRI <= 96)
KNfifthday <- LOW %>% 
  filter(TRI >= 96 & TRI <= 120)
KNsixthday <- LOW %>% 
  filter(TRI >= 120 & TRI <= 144)

#splitting lowlanders into two day blocks
KNfirst2days <- LOW %>% 
  filter(TRI >= 0 & TRI <= 48)
KNsecond2days<- LOW %>% 
  filter(TRI >48 & TRI <= 96)
KNthird2days<- LOW %>% 
  filter(TRI >96 & TRI <= 144)

#splitting highlanders into daily blocks
MEfirstday <- HIGH %>% 
  filter(TRI >= 0 & TRI <= 24)
MEsecondday <- HIGH %>% 
  filter(TRI >= 24 & TRI <= 48)
MEthirdday <- HIGH %>% 
  filter(TRI >= 48 & TRI <= 72)
MEfourthday <- HIGH %>% 
  filter(TRI >= 72 & TRI <= 96)
MEfifthday <- HIGH %>% 
  filter(TRI >= 96 & TRI <= 120)
MEsixthday <- HIGH %>% 
  filter(TRI >= 120 & TRI <= 144)

#splitting highlanders into two day blocks
MEfirst2days <- HIGH %>% 
  filter(TRI >= 0 & TRI <= 48)
MEsecond2days<- HIGH %>% 
  filter(TRI >48 & TRI <= 96)
MEthird2days<- HIGH %>% 
  filter(TRI >96 & TRI <= 144)



## Daily Plots

# Day 1
day1 <- ggplot() +
  geom_histogram(data= KNfirstday, aes(x = difference, y = after_stat(count / sum(count) * 100)), position = "identity", color= "black", fill = "#3CCFCC", alpha = .6, bins = 30) +
  geom_histogram(data= MEfirstday, aes(x = difference, y = after_stat(count / sum(count) * 100)), position = "identity", color = "black",fill= "#D9DB44", alpha = .6, bins = 30) +
  labs(title = "Day 1 Post-Injection",
       x = "Difference in Temperature (Actual-Expected)",
       y = "Percentage of Observations") +
  scale_x_continuous(limits= c(-4,4)) +
  scale_y_continuous(limits = c(0,20)) + 
  geom_vline(xintercept = 0.3572717, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = 0.007361411, color = "orange", linetype = "dashed", size = 1) +
  theme(panel.background = element_rect(fill = "white"))
day1


# Day 2
day2 <- ggplot() +
  geom_histogram(data= KNsecondday, aes(x = difference, y = after_stat(count / sum(count) * 100)), position = "identity", color= "black", fill = "#3CCFCC", alpha = .6, bins = 30) +
  geom_histogram(data= MEsecondday, aes(x = difference, y = after_stat(count / sum(count) * 100)), position = "identity", color = "black",fill= "#D9DB44", alpha = .6, bins = 30) +
  labs(title = "Day 2 Post-Injection",
       x = "Difference in Temperature (Actual-Expected)",
       y = "Percentage of Observations") +
  scale_x_continuous(limits= c(-4,4)) +
  scale_y_continuous(limits = c(0,20)) +
  geom_vline(xintercept = 0.76945782, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = 0.0021988, color = "orange", linetype = "dashed", size = 1) +
  theme(panel.background = element_rect(fill = "white"))
day2

# Day 3
day3 <- ggplot() +
  geom_histogram(data= KNthirdday, aes(x = difference, y = after_stat(count / sum(count) * 100)), position = "identity", color= "black", fill = "#3CCFCC", alpha = .6, bins = 30) +
  geom_histogram(data= MEthirdday, aes(x = difference, y = after_stat(count / sum(count) * 100)), position = "identity", color = "black",fill= "#D9DB44", alpha = .6, bins = 30) +
  labs(title = "Day 3 Post-Injection",
       x = "Difference in Temperature (Actual-Expected)",
       y = "Percentage of Observations") +
  scale_x_continuous(limits= c(-4,4)) +
  scale_y_continuous(limits = c(0,20)) +
  geom_vline(xintercept = 0.27086772, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = -0.1547599, color = "orange", linetype = "dashed", size = 1) +
  theme(panel.background = element_rect(fill = "white"))
day3

# Day 4
day4 <- ggplot() +
  geom_histogram(data= KNfourthday, aes(x = difference, y = after_stat(count / sum(count) * 100)), position = "identity", color= "black", fill = "#3CCFCC", alpha = .6, bins = 30) +
  geom_histogram(data= MEfourthday, aes(x = difference, y = after_stat(count / sum(count) * 100)), position = "identity", color = "black",fill= "#D9DB44", alpha = .6, bins = 30) +
  labs(title = "Day 4 Post-Injection",
       x = "Difference in Temperature (Actual-Expected)",
       y = "Percentage of Observations") +
  scale_x_continuous(limits= c(-4,4)) +
  scale_y_continuous(limits = c(0,20)) +
  geom_vline(xintercept = -0.1001205, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = -0.3481681, color = "orange", linetype = "dashed", size = 1) +
  theme(panel.background = element_rect(fill = "white"))
day4

# Day 5
day5 <- ggplot() +
  geom_histogram(data= KNfifthday, aes(x = difference, y = after_stat(count / sum(count) * 100)), position = "identity", color= "black", fill = "#3CCFCC", alpha = .6, bins = 30) +
  geom_histogram(data= MEfifthday, aes(x = difference, y = after_stat(count / sum(count) * 100)), position = "identity", color = "black",fill= "#D9DB44", alpha = .6, bins = 30) +
  labs(title = "Day 5 Post-Injection",
       x = "Difference in Temperature (Actual-Expected)",
       y = "Percentage of Observations") +
  scale_x_continuous(limits= c(-4,4)) +
  scale_y_continuous(limits = c(0,20)) +
  geom_vline(xintercept = -0.122386, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = -0.1636941, color = "orange", linetype = "dashed", size = 1) +
  theme(panel.background = element_rect(fill = "white"))
day5

#Day 6
day6 <- ggplot() +
  geom_histogram(data= KNsixthday, aes(x = difference, y = after_stat(count / sum(count) * 100)), position = "identity", color= "black", fill = "#3CCFCC", alpha = .6, bins = 30) +
  geom_histogram(data= MEsixthday, aes(x = difference, y = after_stat(count / sum(count) * 100)), position = "identity", color = "black",fill= "#D9DB44", alpha = .6, bins = 30) +
  labs(title = "Day 6 Post-Injection",
       x = "Difference in Temperature (Actual-Expected)",
       y = "Percentage of Observations") +
  scale_x_continuous(limits= c(-4,4)) +
  scale_y_continuous(limits = c(0,20)) +
  geom_vline(xintercept = -0.0299821, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = -0.1637686, color = "orange", linetype = "dashed", size = 1) +
  theme(panel.background = element_rect(fill = "white"))

day6

panel <- ggarrange(
  day1, day2, day3, day4,day5, day6, labels = c("A", "B", "C", "D", "E","F"), legend = "bottom"
)
panel
ggsave("Tbdailyresid.pdf", plot = panel, device = "pdf")

#Two day plots
library(ggplot2)
# Day 1-2

a <- ggplot() +
  geom_histogram(data= KNfirst2days, aes(x = difference, y = after_stat(count / sum(count) * 100)), position = "identity", color= "black", fill = "#3CCFCC", alpha = .6, bins = 30) +
  geom_histogram(data= MEfirst2days, aes(x = difference, y = after_stat(count / sum(count) * 100)), position = "identity", color = "black",fill= "#D9DB44", alpha = .6, bins = 30) +
  labs(title = "Days 1-2 Post-Injection",
       x = "Difference in Temperature (Actual-Expected)",
       y = "Percentage of Observations") +
  scale_x_continuous(limits= c(-4,4)) +
  scale_y_continuous(limits = c(0,20)) +
  geom_vline(xintercept = 0.5424417, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = 0.06633955, color = "orange", linetype = "dashed", size = 1) +
  theme(panel.background = element_rect(fill = "white"))

a


#Day 3-4
b <- ggplot() +
  geom_histogram(data= KNsecond2days, aes(x = difference, y = after_stat(count / sum(count) * 100)), position = "identity", color= "black", fill = "#3CCFCC", alpha = .6, bins = 30) +
  geom_histogram(data= MEsecond2days, aes(x = difference, y = after_stat(count / sum(count) * 100)), position = "identity", color = "black",fill= "#D9DB44", alpha = .6, bins = 30) +
  labs(title = "Days 3-4 Post-Injection",
       x = "Difference in Temperature (Actual-Expected)",
       y = "Percentage of Observations") +
  scale_x_continuous(limits= c(-4,4)) +
  scale_y_continuous(limits = c(0,20)) +
  geom_vline(xintercept = 0.05675135, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = -0.2814756, color = "orange", linetype = "dashed", size = 1) +
  theme(panel.background = element_rect(fill = "white"))

b


#Day 5-6
c <- ggplot() +
  geom_histogram(data= KNthird2days, aes(x = difference, y = after_stat(count / sum(count) * 100)), position = "identity", color= "black", fill = "#3CCFCC", alpha = .6, bins = 30) +
  geom_histogram(data= MEthird2days, aes(x = difference, y = after_stat(count / sum(count) * 100)), position = "identity", color = "black",fill= "#D9DB44", alpha = .6, bins = 30) +
  labs(title = "Days 5-6 Post-Injection",
       x = "Difference in Temperature (Actual-Expected)",
       y = "Percentage of Observations") +
  scale_x_continuous(limits= c(-4,4)) +
  scale_y_continuous(limits = c(0,20)) +
  geom_vline(xintercept = -0.064011, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = -0.1654412, color = "orange", linetype = "dashed", size = 1) +
  theme(panel.background = element_rect(fill = "white"))

c



