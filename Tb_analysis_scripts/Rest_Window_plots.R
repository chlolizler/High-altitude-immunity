#Load packages
library(ggplot2)
library(lubridate)
library(tidyverse)
library(dplyr)
library(ggpubr)

#Load data
setwd("/Users/chloebutler/Desktop/cleaned_data_scripts/data_files")
rest <- read.csv("restwindow_8AM.csv", header = TRUE)

rest <- rest %>%
  mutate(Population = recode(Population, 
                             "KN" = "Lowland", 
                             "ME" = "Highland"))



#Plotting mean, median, min, and max as individual points colored by population
a <- ggplot(rest, aes(x = Day, y = Mean.Tb, color = Population, group = Population))  +
  geom_point() + 
  labs(x = "Time Relative to Injection (Day)", y = "Mean Temperature", color = "Population") +
  theme_minimal() +scale_y_continuous(limits = c(34, 40))

b <- ggplot(rest, aes(x = Day, y = Median.Tb, color = Population, group = Population))  +
  geom_point() + 
  labs(x = "Time Relative to Injection (Day)", y = "Median Temperature", color = "Population") +
  theme_minimal() +scale_y_continuous(limits = c(34, 40))

c <- ggplot(rest, aes(x = Day, y = Min.Tb, color = Population, group = Population))  +
  geom_point() + 
  labs(x = "Time Relative to Injection (Day)", y = "Minimum Temperature", color = "Population") +
  theme_minimal() +scale_y_continuous(limits = c(34, 40))

d <- ggplot(rest, aes(x = Day, y = Max.Tb, color = Population, group = Population))  +
  geom_point() + 
  labs(x = "Time Relative to Injection (Day)", y = "Maximum Temperature", color = "Population") +
  theme_minimal() +scale_y_continuous(limits = c(34, 40))


ggarrange(
  a, b, c, d, labels = c("A", "B", "C", "D"), legend = "bottom"
)



#Making population averages for each day for each of the parameters (mean, median, min, max) and graphing the lines
restmeanavg <- rest %>%
  group_by(Day, Population) %>%
  summarise(mean_temp = mean(Mean.Tb, na.rm = TRUE),
            sd_temp = sd(Mean.Tb, na.rm = TRUE),
            count = n())

aa <- ggplot(restmeanavg, aes(x = Day, y = mean_temp, color = Population, group = Population)) +
  geom_errorbar(aes(ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp), position=position_dodge(width=0.2)) +
  geom_line() + 
  labs(x = "Time Relative to Injection (Days)", y = "Mean Temperature", color = "Population") + 
  scale_color_manual(values = c("Lowland" = "#3CCFCC", "Highland" = "#D9DB44")) +
  scale_y_continuous(limits = c(34, 39))  + theme(panel.background = element_rect(fill = "white"))

restmedianavg <- rest %>%
  group_by(Day, Population) %>%
  summarise(mean_tempmed = mean(Median.Tb, na.rm = TRUE),
            sd_tempmed = sd(Median.Tb, na.rm = TRUE),
            count = n())

bb <- ggplot(restmedianavg, aes(x = Day, y = mean_tempmed, color = Population, group = Population)) +
  geom_errorbar(aes(ymin = mean_tempmed - sd_tempmed, ymax = mean_tempmed + sd_tempmed), position=position_dodge(width=0.2)) +
  geom_line() +  
  labs(x = "Time Relative to Injection (Days)", y = "Median Temperature", color = "Population") +
  scale_color_manual(values = c("Lowland" = "#3CCFCC", "Highland" = "#D9DB44")) +
  scale_y_continuous(limits = c(34, 39)) + theme(panel.background = element_rect(fill = "white"))

restmaxavg <- rest %>%
  group_by(Day, Population) %>%
  summarise(mean_tempmax = mean(Max.Tb, na.rm = TRUE),
            sd_tempmax = sd(Max.Tb, na.rm = TRUE),
            count = n())

cc <- ggplot(restmaxavg, aes(x = Day, y = mean_tempmax, color = Population, group = Population)) +
  geom_errorbar(aes(ymin = mean_tempmax - sd_tempmax, ymax = mean_tempmax + sd_tempmax), position=position_dodge(width=0.2)) +
  geom_line()  +
  labs(x = "Time Relative to Injection (Days)", y = "Maximum Temperature", color = "Population") +
  scale_color_manual(values = c("Lowland" = "#3CCFCC", "Highland" = "#D9DB44")) +
  scale_y_continuous(limits = c(34, 39)) + theme(panel.background = element_rect(fill = "white"))


restminavg <- rest %>%
  group_by(Day, Population) %>%
  summarise(mean_tempmin = mean(Min.Tb, na.rm = TRUE),
            sd_tempmin = sd(Min.Tb, na.rm = TRUE),
            count = n())

dd <- ggplot(restminavg, aes(x = Day, y = mean_tempmin, color = Population, group = Population)) +
  geom_errorbar(aes(ymin = mean_tempmin - sd_tempmin, ymax = mean_tempmin + sd_tempmin), position=position_dodge(width=0.2)) +
  geom_line() + scale_color_manual(values = c("Lowland" = "#3CCFCC", "Highland" = "#D9DB44")) +
  labs(x = "Time Relative to Injection (Days)", y = "Minimim Temperature", color = "Population") +
   scale_y_continuous(limits = c(34, 39)) +  theme(panel.background = element_rect(fill = "white"))

restphase <- ggarrange(
  aa, bb, cc, dd, labels = c("A", "B", "C", "D"), legend = "bottom"
)

restphase

ggsave("restphaseplot_8am.pdf", plot = restphase, device = "pdf")

