library(ggplot2)
library(lubridate)
library(tidyverse)
library(dplyr)
library(car)

#Set working directory
setwd("/Users/chloebutler/Downloads")
CORT <- read.csv("R_cort_ceb.csv", header = TRUE)

#colnames(CORT)[colnames(CORT) == "Population"] <- "Pop"

CORT$dose[CORT$Trial=="4"] <- "1"
CORT$dose[CORT$Trial=="3"] <- "2"
CORT$dose[CORT$Trial=="2"] <- "2"
CORT$dose[CORT$Trial=="1"] <- "2"


#POP + INJ
CORT$cat <- as.factor(paste(CORT$Inj_2, CORT$Pop)) 
level_order <- c('SAL KN','LPS KN','SAL ME', 'LPS ME') 

ggplot(CORT, aes(x=factor(cat, level= level_order), y=CORT$ngperml_CORRECTED, fill=Pop))  +   
  geom_boxplot(outlier.shape = NA) +  geom_jitter(width= 0.2) + xlab("Population x Injection Type") +
  ylab("CORT Level")  +  scale_fill_manual(values = c("KN" = "#3CCFCC", "ME" = "#D9DB44")) +
  theme(panel.background = element_rect(fill = "white"))


f <- lm(CORT$ngperml_CORRECTED ~ Inj_2*Pop, data = CORT[!is.na(CORT$Inj_2),])

f <- lmer(CORT$ngperml_CORRECTED ~ Inj_2*Pop + (1|Plate),  
          data = CORT[!is.na(CORT$Inj_2),])  
anova(f)
