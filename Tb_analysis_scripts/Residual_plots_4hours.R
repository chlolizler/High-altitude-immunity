library(ggpubr)

#Load in dataset
setwd("/Users/chloebutler/Desktop/Tb analysis/residuals")
KN <- read.csv("LOWresid.csv", header = TRUE)
ME <- read.csv("HIGHresid.csv", header = TRUE)

#Filter active & rest phase phases for each day by population
KNday1active <- KN %>% 
  filter(TRI >= 2 & TRI <= 6)
MEday1active <- ME %>% 
  filter(TRI >= 2 & TRI <= 6)

KNday1rest <- KN %>% 
  filter(TRI >= 14 & TRI <= 18)
MEday1rest <- ME %>% 
  filter(TRI >= 14 & TRI <= 18)

KNday2active <- KN %>% 
  filter(TRI >= 26 & TRI <= 30)
MEday2active <- ME %>% 
  filter(TRI >= 26 & TRI <= 30)

KNday2rest <- KN %>% 
  filter(TRI >= 38 & TRI <= 42)
MEday2rest <- ME %>% 
  filter(TRI >= 38 & TRI <= 42)

KNday3active <- KN %>% 
  filter(TRI >= 50 & TRI <= 54)
MEday3active <- ME %>% 
  filter(TRI >= 50 & TRI <= 54)

KNday3rest <- KN %>% 
  filter(TRI >= 62 & TRI <= 66)
MEday3rest <- ME %>% 
  filter(TRI >= 62 & TRI <= 66)

KNday4active <- KN %>% 
  filter(TRI >= 74 & TRI <= 78)
MEday4active <- ME %>% 
  filter(TRI >= 74 & TRI <= 78)

KNday4rest <- KN %>% 
  filter(TRI >= 86 & TRI <= 90)
MEday4rest <- ME %>% 
  filter(TRI >= 86 & TRI <= 90)

#Day 1 active phase plot
day1ACT <- ggplot() +
  geom_histogram(data= KNday1active, aes(x = difference, y = after_stat(count / sum(count))), position = "identity", color= "#3CCFCC", fill = "#3CCFCC", alpha = .6, bins = 30) +
  geom_histogram(data= MEday1active, aes(x = difference, y = after_stat(count / sum(count))), position = "identity", color = "#D9DB44",fill= "#D9DB44", alpha = .6, bins = 30) +
  labs(title = "Day 1 (ACTIVE) Post-Injection",
       x = "Difference in Temperature (Actual-Expected)",
       y = "Density")  + scale_x_continuous(limits= c(-4,4))  
day1ACT


#Day 1 rest phase plot
day1REST <- ggplot() +
  geom_histogram(data= KNday1rest, aes(x = difference, y = after_stat(count / sum(count))), position = "identity", color= "#3CCFCC", fill = "#3CCFCC", alpha = .6, bins = 30) +
  geom_histogram(data= MEday1rest, aes(x = difference, y = after_stat(count / sum(count))), position = "identity", color = "#D9DB44",fill= "#D9DB44", alpha = .6, bins = 30) +
  labs(title = "Day 1 (REST) Post-Injection",
       x = "Difference in Temperature (Actual-Expected)",
       y = "Frequency")  + 
  scale_x_continuous(limits= c(-4,4)) 
day1REST


#Day 2 active phase plot
day2ACT <- ggplot() +
  geom_histogram(data= KNday2active, aes(x = difference, y = after_stat(count / sum(count))), position = "identity", color= "#3CCFCC", fill = "#3CCFCC", alpha = .6, bins = 30) +
  geom_histogram(data= MEday2active, aes(x = difference, y = after_stat(count / sum(count))), position = "identity", color = "#D9DB44",fill= "#D9DB44", alpha = .6, bins = 30) +
  labs(title = "Day 2 (ACTIVE) Post-Injection",
       x = "Difference in Temperature (Actual-Expected)",
       y = "Frequency")  
day2ACT

#Day 2 rest phase plot
day2REST <- ggplot() +
  geom_histogram(data= KNday2rest, aes(x = difference, y = after_stat(count / sum(count))), position = "identity", color= "#3CCFCC", fill = "#3CCFCC", alpha = .6, bins = 30) +
  geom_histogram(data= MEday2rest, aes(x = difference, y = after_stat(count / sum(count))), position = "identity", color = "#D9DB44",fill= "#D9DB44", alpha = .6, bins = 30) +
  labs(title = "Day 2 (REST) Post-Injection",
       x = "Difference in Temperature (Actual-Expected)",
       y = "Frequency")  + 
  scale_x_continuous(limits= c(-4,4))  
day2REST

#Day 3 active phase plot
day3ACT <- ggplot() +
  geom_histogram(data= KNday3active, aes(x = difference, y = after_stat(count / sum(count))), position = "identity", color= "#3CCFCC", fill = "#3CCFCC", alpha = .6, bins = 30) +
  geom_histogram(data= MEday3active, aes(x = difference, y = after_stat(count / sum(count))), position = "identity", color = "#D9DB44",fill= "#D9DB44", alpha = .6, bins = 30) +
  labs(title = "Day 3 (ACTIVE) Post-Injection",
       x = "Difference in Temperature (Actual-Expected)",
       y = "Frequency")  + 
  scale_x_continuous(limits= c(-4,4))  
day3ACT

#Day 3 rest phase plot
day3REST <- ggplot() +
  geom_histogram(data= KNday3rest, aes(x = difference, y = after_stat(count / sum(count))), position = "identity", color= "#3CCFCC", fill = "#3CCFCC", alpha = .6, bins = 30) +
  geom_histogram(data= MEday3rest, aes(x = difference, y = after_stat(count / sum(count))), position = "identity", color = "#D9DB44",fill= "#D9DB44", alpha = .6, bins = 30) +
  labs(title = "Day 3 (REST) Post-Injection",
       x = "Difference in Temperature (Actual-Expected)",
       y = "Frequency")  + 
  scale_x_continuous(limits= c(-4,4))  
day3REST

#Building a panel for the plots
panel <- ggarrange(
  day1ACT, day1REST, day2ACT, day2REST,day3ACT, day3REST,   ncol = 2, nrow = 3, labels = c("A", "B", "C", "D", "E","F"), legend = "bottom"
)
panel



#Finding medians for windows
setwd("/Users/chloebutler/Desktop/Tb analysis/rest window")
df <-read.csv("restwindow.csv", header= TRUE)



median(KNday1active$actual, na.rm = TRUE)
median(KNday1active$difference, na.rm = TRUE)

median(MEday1active$actual, na.rm = TRUE)
median(MEday1active$difference, na.rm = TRUE)

median(KNday2active$actual, na.rm = TRUE)
median(MEday2active$actual, na.rm = TRUE)
