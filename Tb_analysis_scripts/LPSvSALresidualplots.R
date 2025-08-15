#This code makes residual histogram plots: one for the overall effect of LPS on the populations and one for the overall effect of saline on the controls. These residuals are calculated by subtracting pre-injection temperatures from post-injection temperature for each individual mouse

#Load packages
library(ggplot2)
library(lubridate)
library(tidyverse)
library(dplyr)
library(mgcv)
library(MuMIn)
library(ggeffects)
library(ggpubr)

#Load residual data for lowlanders, highlanders, and saline control
setwd("/Users/chloebutler/Desktop/Tb analysis/residuals")
LOW <- read.csv("LOWresid.csv", header = TRUE)
HIGH <- read.csv("HIGHresid.csv", header = TRUE)
SALmerge <- read.csv("salineresid.csv", header = TRUE)

#Keep only the first 6 days of residual data
KN6days <- LOW %>% 
  filter(TRI >= 0 & TRI <= 144)
ME6days <- HIGH %>% 
  filter(TRI >= 0 & TRI <= 144)

#Calculate the medians, standard deviations, and 95% confidence intervals for lowlander and highlander data
median(KN6days$difference, na.rm = TRUE)
median(ME6days$difference, na.rm = TRUE)

median_low <- 0.09188455  # Given median value for LOW dataset
median_high <- -0.1766481


sd(KN6days$difference)
LOWCIl <- median_low + 1.96 *0.9312243
LOWCIu <- median_low - 1.96 *0.9312243


sd(ME6days$difference)
HICIl <- median_high + 1.96 *1.184146
HICIu <- median_high - 1.96 *1.184146


#Plot the lowlander and highlander residuals with 95% CI
ggplot() +
  
  # Histograms for LOW and HIGH datasets
  geom_histogram(data = LOW, aes(x = difference, y = after_stat(count / sum(count) * 100), fill = "LOW"),
                 position = "identity", color = "black", alpha = .6, bins = 30) +
  
  geom_histogram(data = HIGH, aes(x = difference, y = after_stat(count / sum(count) * 100), fill = "HIGH"),
                 position = "identity", color = "black", alpha = .6, bins = 30) +
  
  # Points for medians (low and high)
  geom_point(aes(x = median_low, y = 17.5, color = "KN"), size = .7, shape = 19) +  
  geom_point(aes(x = median_high, y = 18, color = "ME"), size = .7, shape = 19) +  
  
  # Error bars for LOW and HIGH
  geom_errorbar(aes(xmin = LOWCIl, xmax = LOWCIu, y = 17.5, color = "KN CI")) +
  geom_errorbar(aes(xmin = HICIl, xmax = HICIu, y = 18, color = "ME CI")) +
  
  # Titles and axis labels
  labs(title = "ALL Post-Injection",
       x = "Difference in Temperature (Actual - Expected)",
       y = "Percentage of Observations")  +
  
  scale_x_continuous(limits = c(-4, 4)) +
  scale_y_continuous(limits = c(0, 20)) +
  scale_fill_manual(values = c("LOW" = "#3CCFCC", "HIGH" = "#D9DB44")) +
  scale_color_manual(values = c("ME" = "orange", "KN" = "blue", 
                                "ME CI" = "orange", "KN CI" = "blue")) +
  guides(color = guide_legend(title = "Population")) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "bottom",  
        legend.title = element_text(size = 10),  
        legend.text = element_text(size = 8))  




#Saline plot
#Calculate median, standard deviation, and 95% CI
median(SALmerge$difference[SALmerge$ID == "167DBF14"], na.rm = TRUE)
median(SALmerge$difference[SALmerge$ID == "167DCF7F"], na.rm = TRUE)
median(SALmerge$difference[SALmerge$ID == "2A8F4DC9"], na.rm = TRUE)
median(SALmerge$difference, na.rm = TRUE)


median(SALmerge$difference, na.rm = TRUE)
median_SAL <- -0.08141556 

sd(SALmerge$difference)
SALCIl <- median_high + 1.96 *0.6148143
SALCIu <- median_high - 1.96 *0.6148143


#Saline plot for 6 days post-injection w/ 95% CI
ggplot() +   
  geom_histogram(data = SALmerge, aes(x = difference, y = after_stat(count / sum(count) * 100), fill = ID), position = "identity", color = "black", 
                 alpha = .6, 
                 bins = 30) +    
  labs(title = "Saline mice OVERALL",
       x = "Difference in Temperature (Actual-Expected)",
       y = "Percentage of Observations")  +    
  scale_y_continuous(limits = c(0, 20)) +  
  scale_x_continuous(limits = c(-4, 4)) +  
  scale_fill_manual(values = c("167DBF14" = "#A8D8A8", 
                               "167DCF7F" = "#A2C2E5", 
                               "2A8F4DC9" = "#FFB3C1")) +  
  geom_point(aes(x = median_SAL, y = 17.5), color = "violet", size = 0.7, shape=19) +
  geom_errorbar(aes(xmin = SALCIl, xmax = SALCIu, y = 17.5), color = 'violet') +  # Increase size and add cap width
  theme(panel.background = element_rect(fill = "white")) 



