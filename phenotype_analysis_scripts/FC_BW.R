#Packages
library(ggplot2) 
library(tidyverse) 
library(dplyr) 
library("RColorBrewer")

#Read in data + re-assign column names
setwd("/Users/chloebutler/Downloads") 
A <- read.csv("4262BWFC.csv", header = TRUE) 

names(A)[names(A) == "Population"] <- "Pop"
names(A)[names(A) == "Altitude"] <- "Population"

XX <-na.omit(A)
XX$cat <- as.factor(paste(XX$Day, XX$Pop)) 
Z <- XX %>%
  filter( Day < 2)
Z <- Z %>%
  mutate(Food_per_Weight = Food.consump / BW) 
XX <- XX %>%
  mutate(Food_per_Weight = Food.consump / BW) 
XX <- XX %>%
  filter( Day >= 2)
Z$Day<-as.factor(Z$Day)
Z$cat <- as.factor(paste(Z$Day, Z$Pop)) 

#Food Consump day 0-12

level_order <- c('0 KN','0 ME','2 KN', '2 ME', '4 KN', '4 ME', '6 KN', '6 ME', '8 KN', '8 ME', '10 KN', '10 ME', '12 KN', '12 ME') 
c <- ggplot(XX, aes(x=factor(cat,level = level_order), y=Food.consump, fill= Pop))  +   
  geom_boxplot() +  geom_jitter(width = 0.25) +
  ggtitle("Food consumption after primary injection") + xlab("Population x Day") +
  ylab("Food Consumption (g)")  + scale_fill_manual(values = c("KN" = "#3CCFCC", "ME" = "#D9DB44")) + labs(fill = "Population") +theme(panel.background = element_rect(fill = "white"))
c


## FOOD CONSUMPTION

A$Food.consump = as.numeric(A$Food.consump) 
A$Pop<-as.factor(A$Pop) 
A$Population <-as.factor(A$Population) 

#One value was above 20 g, which was excluded from analysis
AA <- A %>%
  filter(Food.consump <= 20.0)

## Food consumption per gram of body weight
#Day 2-12
XXX <- XXX %>%
  mutate(Food_per_Weight = Food.consump / BW)  
level_order2 <- c('2 KN','2 ME', '4 KN','4 ME', '6 KN','6 ME', '8 KN','8 ME', '10 KN','10 ME', '12 KN','12 ME') 
e <- ggplot(XXX, aes(x=factor(cat, level = level_order2), y=Food_per_Weight, fill=Pop)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width= 0.2) +
  ggtitle("Food consumption per gram of body weight after primary injection") +
  xlab("Population x Day") +
  ylab("Food Consumption per Gram of Body Weight (g)") +
  scale_fill_manual(values = c("KN" = "#3CCFCC", "ME" = "#D9DB44")) +
  labs(fill = "Population") +
  theme(panel.background = element_rect(fill = "white"))
e

Z$Day <- as.factor(Z$Day)
level_order5 <- c('0 KN', '0 ME') 
ee <- ggplot(Z, aes(x=factor(cat,level = level_order5), y=Food_per_Weight, fill=Pop))  +   
  geom_boxplot(outlier.shape = NA) +  geom_jitter(width = 0.2) +
  ggtitle("Food per gram of Body weight") + xlab("Population") +
  ylab("Food per gram of body weight (g)") + scale_fill_manual(values = c("KN" = "#3CCFCC", "ME" = "#D9DB44")) + labs(fill = "Population") +theme(panel.background = element_rect(fill = "white"))
ee

## BODY WEIGHT CHANGE
#Body weight change line w/o extreme values
exclude_ids <- c("167DD4DD", "167DDDDC")
X_filter <- XX %>%
  filter(!ID %in% exclude_ids)

#Body weight change boxplot starting at day 2 
level_order1 <- c('2 KN', '2 ME', '4 KN', '4 ME', '6 KN', '6 ME', '8 KN', '8 ME', '10 KN', '10 ME', '12 KN', '12 ME') 
i <- ggplot(X_filter, aes(x=factor(cat,level = level_order1), y=BW, fill=Pop))  +   
  geom_boxplot(outlier.shape = NA) +  geom_jitter(width = 0.2) +
  ggtitle("Body weight after primary injection") + xlab("Population x Day") +
  ylab("Body Weight (g)") + scale_fill_manual(values = c("KN" = "#3CCFCC", "ME" = "#D9DB44")) + labs(fill = "Population") +theme(panel.background = element_rect(fill = "white"))
i

XXXX <- X_filter %>%
  filter( Day < 2)
Z$Day <- as.factor(Z$Day)
level_order8 <- c('0 KN', '0 ME') 
O <- ggplot(Z, aes(x=factor(cat,level = level_order8), y=BW, fill=Pop))  +   
  geom_boxplot(outlier.shape = NA) +  geom_jitter(width=0.2) +
  ggtitle("Body weight after primary injection") + xlab("Population x Day") +
  ylab("Body Weight (g)") + scale_fill_manual(values = c("KN" = "#3CCFCC", "ME" = "#D9DB44")) + labs(fill = "Population") +theme(panel.background = element_rect(fill = "white"))
O


## STATISTICS

#Packages
library(lmerTest)
library(emmeans)
library(car)
library(lme4)

A$Day <- as.numeric(A$Day)
STAT <- A %>% 
  filter(Day >= 2)
A$Day <- as.factor(A$Day)


#Food consumption anova day 0
bb <- lm(Food.consump ~ BW + Population, data = Z)
Anova(bb, type="III")

#Food consumption anova day 2-12
cc <- lmer(Food.consump ~ D0.BW + Population*Day + (1|ID), data = XX)
anova(cc)

##confirming outliers in BW data
install.packages("EnvStats")
library(EnvStats)

#run the Generalized ESD test
esd_result <- rosnerTest(Z$BW, k = 3, alpha = 0.05)
esd_result$all.stats

#identifying outliers w/in the cleaned numeric vector
outlier_idx <- esd_result$all.stats$Obs.Num[esd_result$all.stats$Outlier == TRUE]

#mapping back to row numbers in the original dataset A
exclude_ids <- Z$ID[outlier_idx] ##IDs to exclude are "167DD4DD" & "167DDDDC"

#Body weight anova day 0
Z_filter <- Z %>%
  filter(!ID %in% exclude_ids)
dd <- lm(BW ~ Population, data = Z)
Anova(dd, type= "III")

#Body weight anova days 2-12
kk <- lmer(BW~D0.BW + Population*Day + (1|ID), data = X_filter)
anova(kk)

library(ggpubr)
panel <- ggarrange(
  g,b, i, c, labels = c("A", "B", "C", "D"), legend = "bottom")
panel

library(ggpubr)
panel2 <- ggarrange(
  j,b, k, c, labels = c("A", "B", "C", "D"), legend = "bottom")
panel2

panel3 <- ggarrange(
  i, e, 
  labels = c("A", "B"), 
  legend = "bottom",
  nrow = 2,  # stack plots vertically (2 rows)
  ncol = 1   # 1 column
)
panel3
ggsave("BWFCboxes.pdf", plot = panel3, device = "pdf")

panel4 <- ggarrange(
  O, i, ee, e, labels = c("A", "B", "C", "D"), legend = "bottom")
panel4

panel4 <- ggarrange(
  ee, e, O, i,
  labels = c("A", "B", "C", "D"),
  legend = "bottom",
  widths = c(0.23, 1)  # Adjust the heights so that row 1 (A and C) is smaller
)
panel4

ggsave("BWFC4plot.pdf", plot = panel4, device = "pdf")














