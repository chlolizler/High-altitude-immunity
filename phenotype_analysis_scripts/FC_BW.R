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
  geom_boxplot() +  geom_point() +
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

#Make mean and sd values for food consumption
FCavg = AA %>% dplyr::group_by(Pop, Day) %>%  
  dplyr::mutate(mean = mean(Food.consump, na.rm = TRUE), sd = sd(Food.consump),  
                se = sd/sqrt(n())) 

#Food consumption w/ jitter
a <- ggplot(FCavg, aes(x=Day), group = Pop, color = Pop) +  
  ylab("Average Food Consumption (g)") + geom_line(aes(y = mean, group = Pop, color = Pop), size=2) + 
  scale_color_manual(values = c("KN" = "#3CCFCC", "ME" = "#D9DB44")) + geom_jitter(aes(y= Food.consump, color=Pop)) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se, width=0.1, color= Pop)) +
  theme(panel.background = element_rect(fill = "white"))
a


#Food consumption w/o jitter

b <- ggplot(FCavg, aes(x=Day), group = Pop, color = Pop) +  
  ylab("Average Food Consumption (g)")  + 
  geom_line(aes(y = mean, group = Pop, color = Pop), size=2) + 
  scale_color_manual(values = c("KN" = "#3CCFCC", "ME" = "#D9DB44")) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se, width=0.1, color= Pop), position=position_dodge(width=0.15), size = 1) +
  theme(panel.background = element_rect(fill = "white"))
b

#Food consumption boxplot
#Boxplot for food consumption without day 0
XXX <- XX %>%
  mutate(Day = as.numeric(as.character(Day))) %>%
  filter(Day >= 2 & Day <= 12)
XXX$Day <-as.factor(XXX$Day)

level_order <- c('2 KN', '2 ME', '4 KN', '4 ME', '6 KN', '6 ME', '8 KN', '8 ME', '10 KN', '10 ME', '12 KN', '12 ME') 
d <- ggplot(XXX, aes(x=factor(cat,level = level_order), y=Food.consump, fill=Pop))  +   
  geom_boxplot(outlier.shape = NA) +  geom_point() +
  ggtitle("Food consumption after primary injection") + xlab("Population x Day") +
  ylab("Food Consumption (g)")  +  scale_fill_manual(values = c("KN" = "#3CCFCC", "ME" = "#D9DB44")) + labs(fill = "Population") +theme(panel.background = element_rect(fill = "white"))
d


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

BWCavg = XX %>% dplyr::group_by(Pop, Day) %>%  
  dplyr::mutate(mean = mean(BWC.from.D0, na.rm = TRUE), sd = sd(BWC.from.D0),  
                se = sd/sqrt(n())) 


#Body weight change line w/ jitter
f <- ggplot(BWCavg, aes(x=Day), group = Pop, color = Pop) + 
  ylab("Average Change in Body Weight (g)") + geom_line(aes(y = mean, group = Pop, color = Pop), size=2) +
  geom_jitter(aes(y=BWC.from.D0, color=Pop)) +  scale_color_manual(values = c("KN" = "#3CCFCC", "ME" = "#D9DB44")) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se, width=0.1, color= Pop), position=position_dodge(width=0.15), size = 1)) +
  theme(panel.background = element_rect(fill = "white"))
f


#Body weight change line w/o jitter
g <- ggplot(BWCavg, aes(x=Day), group = Pop, color = Pop) + 
  ylab("Average Change in Body Weight (g)") + geom_line(aes(y = mean, group = Pop, color = Pop), size=2) +
  scale_color_manual(values = c("KN" = "#3CCFCC", "ME" = "#D9DB44")) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se, width=0.1, color= Pop), position=position_dodge(width=0.15), size = 1) +
  theme(panel.background = element_rect(fill = "white"))
g

#Body weight change line w/o extreme values
exclude_ids <- c("167DB1DD", "167DD4DD", "167DDDDC")
X_filter <- XX %>%
  filter(!ID %in% exclude_ids)

bwcavg = X_filter %>% dplyr::group_by(Pop, Day) %>%  
  dplyr::mutate(mean = mean(BWC.from.D0, na.rm = TRUE), sd = sd(BWC.from.D0),  
                se = sd/sqrt(n())) 

h <- ggplot(bwcavg, aes(x=Day), group = Pop, color = Pop) + 
  ylab("Average Change in Body Weight (g)") + geom_line(aes(y = mean, group = Pop, color = Pop), size=2) +
  geom_jitter(aes(y=BWC.from.D0, color=Pop)) +  scale_color_manual(values = c("KN" = "#3CCFCC", "ME" = "#D9DB44")) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se, width=0.1, color= Pop)) +
  theme(panel.background = element_rect(fill = "white"))
h


#Body weight change boxplot starting at day 2 
level_order1 <- c('2 KN', '2 ME', '4 KN', '4 ME', '6 KN', '6 ME', '8 KN', '8 ME', '10 KN', '10 ME', '12 KN', '12 ME') 
i <- ggplot(XXX, aes(x=factor(cat,level = level_order1), y=BWC.from.D0, fill=Pop))  +   
  geom_boxplot(outlier.shape = NA) +  geom_jitter(width = 0.2) +
  ggtitle("Body weight change after primary injection") + xlab("Population x Day") +
  ylab("Body Weight Change (g)") + scale_fill_manual(values = c("KN" = "#3CCFCC", "ME" = "#D9DB44")) + labs(fill = "Population") +theme(panel.background = element_rect(fill = "white"))
i




#BW line
BWavg = XX %>% dplyr::group_by(Pop, Day) %>%  
  dplyr::mutate(mean = mean(BW, na.rm = TRUE), sd = sd(BW),  
                se = sd/sqrt(n())) 

j <- ggplot(BWavg, aes(x=Day), group = Pop, color = Pop) + 
  ylab("Average Body Weight (g)") + geom_line(aes(y = mean, group = Pop, color = Pop), size=2) +
  scale_color_manual(values = c("KN" = "#3CCFCC", "ME" = "#D9DB44")) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se, width=0.1, color= Pop), position=position_dodge(width=0.15), size = 1) +
  theme(panel.background = element_rect(fill = "white"))
j


#Body weight boxplot 
level_order <- c('0 KN', '0 ME', '2 KN', '2 ME', '4 KN', '4 ME', '6 KN', '6 ME', '8 KN', '8 ME', '10 KN', '10 ME', '12 KN', '12 ME') 
k <- ggplot(XX, aes(x=factor(cat,level = level_order), y=BW, fill=Pop))  +   
  geom_boxplot() +  geom_jitter() +
  ggtitle("Body weight after primary injection") + xlab("Population x Day") +
  ylab("Body Weight (g)") + scale_fill_manual(values = c("KN" = "#3CCFCC", "ME" = "#D9DB44")) + labs(fill = "Population") +theme(panel.background = element_rect(fill = "white"))
k

XX$Day <- as.numeric(XX$Day)

XXXX <- XX %>%
  filter( Day < 2)
Z$Day <- as.factor(Z$Day)
level_order8 <- c('0 KN', '0 ME') 
O <- ggplot(Z, aes(x=factor(cat,level = level_order8), y=BW, fill=Pop))  +   
  geom_boxplot(outlier.shape = NA) +  geom_jitter(width=0.2) +
  ggtitle("Body weight after primary injection") + xlab("Population x Day") +
  ylab("Body Weight (g)") + scale_fill_manual(values = c("KN" = "#3CCFCC", "ME" = "#D9DB44")) + labs(fill = "Population") +theme(panel.background = element_rect(fill = "white"))
O

#Body weight boxplot excluding extreme values
level_order <- c('0 KN', '0 ME', '2 KN', '2 ME', '4 KN', '4 ME', '6 KN', '6 ME', '8 KN', '8 ME', '10 KN', '10 ME', '12 KN', '12 ME') 
l <- ggplot(X_filter, aes(x=factor(cat,level = level_order), y=BW, fill=Pop))  +   
  geom_boxplot() +  geom_point() +
  ggtitle("Body weight after primary injection") + xlab("Population x Day") +
  ylab("Body Weight (g)")  +  scale_fill_manual(values = c("KN" = "#3CCFCC", "ME" = "#D9DB44")) + labs(fill = "Population") +theme(panel.background = element_rect(fill = "white"))
l


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
bb <- lm(Food_per_Weight ~ BW + Population, data = Z)
anova(bb)
Anova(bb, type="III")

#Food consumption anova day 2-12
cc <- lmer(Food_per_Weight ~ BW + Population*Day + (1|ID), data = XX)
anova(cc)

#Body weight anova day 0
dd <- lm(D0.BW ~ Population, data = Z)
anova(dd)
Anova(dd, type= "III")


#Body weight anova days 2-12
kk <- lmer(BW~D0.BW + Population*Day + (1|ID), data = XX)
anova(kk)

#Body weight anova excluding extreme values
exclude_ids <- c("167DB1DD", "167DD4DD", "167DDDDC")
Afilter <- A %>%
  filter(!ID %in% exclude_ids)
o <- lmer(BW~D0.BW + Population*Day + (1|ID), data = Afilter)
anova(o)





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














