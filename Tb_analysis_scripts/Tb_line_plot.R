#Packages
library(ggplot2)
library(lubridate)
library(tidyverse)
library(dplyr)
library(RColorBrewer)

#Load data
setwd("/Users/chloebutler/Desktop/Tb analysis")
A <- read.csv("fullmergedbindata.csv", header = TRUE) 
B <- read.csv("mergedbindata.csv", header = TRUE) #excludes mice that died

#Making 6 hour time bins
time_bins <- seq(-120, 240, by = 6)

#Create a new column for time bins
B$Time_Bin <- cut(B$TRI, breaks = time_bins, labels = paste(time_bins[-length(time_bins)], time_bins[-1], sep = "-"), include.lowest = TRUE)


#Line plot excluding mice that died w/ se bars
summary <- B %>%
  group_by(Time_Bin, Population) %>%
  summarise(mean_temp = mean(Temp, na.rm = TRUE),
            sd_temp = sd(Temp, na.rm = TRUE))

#Make se bars based on number of individuals (not number of data points)
summary$se_temp <- NA
summary$se_temp[which(summary$Population=="KN")] <- summary$sd_temp / sqrt(12)
summary$se_temp[which(summary$Population=="ME")] <- summary$sd_temp / sqrt(8)
summary <- summary[!is.na(summary$Time_Bin), ]

pop_colors <- c("KN" = "#3CCFCC",  
                "ME" = "#D9DB44")  

X <- ggplot(summary, aes(x = Time_Bin, y = mean_temp, color = Population, group = Population)) +
  geom_line(position=position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin = mean_temp - se_temp, ymax = mean_temp + se_temp), position=position_dodge(width=0.5), size = 0.6) +
  labs(x = "Time Relative to Injection (Hours)", y = "Body Temperature (°C)", color = "Population") +
  geom_vline(xintercept = "0-6", color = "red", size = 0.5) + scale_color_manual(values = pop_colors) + theme(panel.background = element_rect(fill = "white"))

X
ggsave("TblineSEbars.pdf", plot = X, device = "pdf")


#with sd bars

summ <- B %>%
  group_by(Time_Bin, Population) %>%
  summarise(
    mean_temp = mean(Temp, na.rm = TRUE),
    sd_temp = sd(Temp, na.rm = TRUE),  
    se_temp = sd_temp / sqrt(n()),    
    .groups = 'drop'                   
  )

ggplot(summ, aes(x = Time_Bin, y = mean_temp, color = Population, group = Population)) +
  geom_line(size=1) + 
  geom_errorbar(aes(ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp), width = 0.4, size=.6) +
  labs(x = "Time Relative to Injection (Hours)", y = "Mean Temperature (°C)", color = "Population") +
  scale_color_manual(values = pop_colors) + theme(panel.background = element_rect(fill = "white"))






#Line plot excluding mice that died w/ sd bars
sum <- B %>%
  group_by(Time_Bin, Population) %>%
  summarise(
    mean_temp = mean(Temp, na.rm = TRUE),
    sd_temp = sd(Temp, na.rm = TRUE),  
    se_temp = sd_temp / sqrt(n()),    
    .groups = 'drop'                   
  )

ggplot(sum, aes(x = Time_Bin, y = mean_temp, color = Population, group = Population)) +
  geom_line(position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp), position=position_dodge(width=0.5), size = 0.6) +
  labs(x = "Time Relative to Injection (Hours)", y = "Mean Temperature (°C)", color = "Population") +  scale_color_manual(values = pop_colors)  + theme(panel.background = element_rect(fill = "white"))+ scale_y_continuous(limits = c(34, 40))



#Line plot including the 2 mice that died as separate lines w/ se bars
A$Time_Bin <- cut(A$TRI, breaks = time_bins, labels = paste(time_bins[-length(time_bins)], time_bins[-1], sep = "-"), include.lowest = TRUE)

MERGE <- A %>%
  mutate(Population = case_when(
    Tag.ID %in% c("167FCFF7") ~ "FDIC1",
    Tag.ID %in% c("167DDA6B") ~ "FDIC2",
    TRUE ~ Population  # Keep existing Population for other cases
  ))


#Filter data so that we cut off the temp values at the point that they died
filtered <- MERGE %>%
  filter((Tag.ID == "167FCFF7" & TRI < 12) |
           (Tag.ID == "167DDA6B" & TRI < 48) |
           !(Tag.ID %in% c("167FCFF7", "167DDA6B"))) 

summary1 <- filtered %>%
  group_by(Time_Bin, Population) %>%
  summarise(mean_temp = mean(Temp, na.rm = TRUE),
            se_temp = sd(Temp, na.rm = TRUE) / sqrt(n()))
summary1 <- summary1[!is.na(summary1$Time_Bin), ]

ggplot(summary1, aes(x = Time_Bin, y = mean_temp, color = Population, group = Population, linetype = Population)) +
  geom_line(position=position_jitter(w=0.02, h=0), size = 1) +
  geom_errorbar(aes(ymin = mean_temp - se_temp, ymax = mean_temp + se_temp), width = 0.4, size = .6) +
  labs(x = "Time Relative to Injection (Hours)", y = "Mean Temperature (°C)", color = "Population", linetype = "Population") +
  scale_color_manual(values = c("KN" = "#3CCFCC", "ME" = "#D9DB44", "FDIC1" = "black", "FDIC2" = "black")) +
  scale_linetype_manual(values = c("KN" = "solid", "ME" = "solid", "FDIC1" = "solid", "FDIC2" = "dotted")) + theme(panel.background = element_rect(fill = "white")) + 
  scale_y_continuous(limits = c(34, 40))


#Line plot w/ the two mice that died + sd bars
summary5 <- MERGE %>%
  group_by(Time_Bin, Population) %>%
  summarise(
    mean_temp = mean(Temp, na.rm = TRUE),
    sd_temp = sd(Temp, na.rm = TRUE),  
    se_temp = sd_temp / sqrt(n()),    
    .groups = 'drop'                   
  )

sum <- na.omit(summary5)
ggplot(sum, aes(x = Time_Bin, y = mean_temp, color = Population, group = Population, linetype = Population)) +
  geom_line(position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp), position=position_dodge(width=0.5), size = 0.6) + 
  labs(x = "Time Relative to Injection (Hours)", y = "Mean Temperature (°C)", color = "Population", linetype = "Population") +
  scale_color_manual(values = c("KN" = "#3CCFCC", "ME" = "#D9DB44", "FDIC1" = "black", "FDIC2" = "black")) +
  scale_linetype_manual(values = c("KN" = 1, "ME" = 1, "FDIC1" = 1, "FDIC2" = 3)) + 
  theme(panel.background = element_rect(fill = "white")) + scale_y_continuous(limits = c(26, 40))




