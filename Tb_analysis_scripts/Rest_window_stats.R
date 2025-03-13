#Load packages
library(ggplot2)
library(lubridate)
library(tidyverse)
library(dplyr)
library(mgcv)
library(MuMIn)
library(ggeffects)
library(lmerTest)
library(lme4)

#Set working directory
setwd("/Users/chloebutler/Desktop/cleaned_data_scripts/data_files")
df <-read.csv("restwindow_8AM.csv", header= TRUE)

#Generate anovas for mean, median, min, and max resting body temperatures to assess populational differences
b <- lmer(Mean.Tb ~ Population + (1 |ID), data = df)
summary(b)
anova(b)
b$Mean.Tb
mean(df$Mean.Tb[df$Population == "KN"], na.rm = TRUE)
mean(df$Mean.Tb[df$Population == "ME"], na.rm = TRUE)

c <- lmer(Median.Tb ~ Population + (1 |ID), data = df)
summary(c)
anova(c)
mean(df$Median.Tb[df$Population == "KN"], na.rm = TRUE)
mean(df$Median.Tb[df$Population == "ME"], na.rm = TRUE)

d <- lmer(Min.Tb ~ Population + (1 |ID), data = df)
summary(d)
anova(d)
mean(df$Min.Tb[df$Population == "KN"], na.rm = TRUE)
mean(df$Min.Tb[df$Population == "ME"], na.rm = TRUE)

e <- lmer(Max.Tb ~ Population + (1 |ID), data = df)
summary(e)
anova(e)
mean(df$Max.Tb[df$Population == "KN"], na.rm = TRUE)
mean(df$Max.Tb[df$Population == "ME"], na.rm = TRUE)





