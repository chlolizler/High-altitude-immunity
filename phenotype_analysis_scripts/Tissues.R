#Packages
library(ggplot2)
library(ggpattern)
library(RColorBrewer)

#Read in data + re-assign column names
setwd("/Users/chloebutler/Desktop/cleaned_data_scripts/data_files")  
B <- read.csv("TissueVals.csv", header = TRUE)  

colnames(B)[colnames(B) == "Population"] <- "Pop"
colnames(B)[colnames(B) == "Altitude"] <- "Population"

B$cat <- as.factor(paste(B$Inj..2, B$Pop)) 


## BLOOD GLUCOSE Boxplot
level_order <- c('SAL KN','LPS KN','SAL ME', 'LPS ME') 
a <- ggplot(B, aes(x=factor(cat,level = level_order), y=blood.glucose, fill=Population))  +   
  geom_boxplot(outlier.shape = NA) +  geom_jitter(width= 0.2) + xlab("Population x Injection Type")  +
  ylab("Blood Glucose (mmol/L)")  +  scale_fill_manual(values = c("Lowland" = "#3CCFCC", "Highland" = "#D9DB44")) +
  theme(panel.background = element_rect(fill = "white"))
a

#B <-na.omit(B)
BB <- B %>%
  mutate(spleen_wt_adj = spleen.weight..g. / BW.end) 


## SPLEEN WEIGHT ADJ Boxplot
level_order <- c('SAL KN','LPS KN','SAL ME', 'LPS ME') 
pattern <- list("none", "solid", "none", "solid")
b <- ggplot(BB, aes(x=factor(cat,level = level_order), y=spleen_wt_adj, fill=Population)) + 
  geom_boxplot(outlier.shape = NA) +  geom_jitter(width= 0.2) + xlab("Population x Injection Type") +
  ylab("Spleen Weight Adjusted (g)")  + scale_fill_manual(values = c("Lowland" = "#3CCFCC", "Highland" = "#D9DB44")) + labs(fill = "Population") +theme(panel.background = element_rect(fill = "white")) + labs(fill = "Population") 
b


ggplot(BB, aes(x=BW.end, y=testes.weight..combined...g., color=Population)) + geom_point() + xlab("Population x Injection Type") +
  ylab("Spleen Weight Adjusted (g)") + labs(fill = "Population") +theme(panel.background = element_rect(fill = "white")) + labs(fill = "Population") 



level_order <- c('SAL KN','LPS KN','SAL ME', 'LPS ME') 
pattern <- list("none", "solid", "none", "solid")
b <- ggplot(BB, aes(x=factor(cat,level = level_order), y=spleen.weight..g., fill=Population)) + 
  geom_boxplot() +  geom_point() + xlab("Population x Injection Type") +
  ylab("Spleen Weight Adjusted (g)")  + scale_fill_manual(values = c("Lowland" = "#3CCFCC", "Highland" = "#D9DB44")) + labs(fill = "Population") +theme(panel.background = element_rect(fill = "white")) + labs(fill = "Population") 
b


## TESTES WEIGHT Boxplot
level_order <- c('SAL KN','LPS KN','SAL ME', 'LPS ME') 
c <- ggplot(B, aes(x=factor(cat,level = level_order), y= testes.weight..combined...g., fill=Population))  +   
  geom_boxplot() +  geom_point() +
  ggtitle("Combined Testes Weight After Secondary Injection") + xlab("Population x Injection Type") +
  ylab("Combined Testes Weight (g)")  + scale_fill_brewer(palette = "Accent") + labs(fill = "Population") +theme(panel.background = element_rect(fill = "white"))
c


## HEMOCUE Boxplot
level_order <- c('SAL KN','LPS KN','SAL ME', 'LPS ME') 
d <- ggplot(B, aes(x=factor(cat,level = level_order), y=Hemacue, fill=Population))  +   
  geom_boxplot() +  geom_point() +
  ggtitle("Hemocue After Secondary Injection") + xlab("Population x Injection Type") +
  ylab("Hemocue (g/dL)")  + scale_fill_brewer(palette = "Accent") + labs(fill = "Population") +theme(panel.background = element_rect(fill = "white"))
d


## HEMATOCRIT Boxplot
level_order <- c('SAL KN','LPS KN','SAL ME', 'LPS ME') 
e <- ggplot(B, aes(x=factor(cat,level = level_order), y=crit.avg, fill=Population))  +   
  geom_boxplot() +  geom_point() +
  ggtitle("Hematocrit After Secondary Injection") + xlab("Population x Injection Type") +
  ylab("Hematocrit Percentage")  + scale_fill_brewer(palette = "Accent") + labs(fill = "Population") +theme(panel.background = element_rect(fill = "white"))
e


## STATISTICS

#Packages
library(lme4)  
library(emmeans)  
library(lmerTest)  

#Blood glucose anova
f <- lmer(blood.glucose ~ Inj..2*Population  + (1|Family),  
          data = B[!is.na(B$Inj..2),])  
anova(f,type="III")



#Spleen weight anova
g <- lm(spleen.weight..g. ~ Inj..2*Population + BW.end,  
          data = B[!is.na(B$Inj..2),])  
Anova(g, type= "III")

kk <- lm(spleen_wt_adj ~ Inj..2*Population, data = BB)  
Anova(kk, type= "III") 


#Testes weight anova
h <- lm(testes.weight..combined...g. ~ Inj..2*Population + BW.end,  
          data = B[!is.na(B$Inj..2),])  
Anova(h, type="III")  
summary(h)
#Hemocue anova
i <- lmer(Hemacue ~ Inj..2*Population + (1|Family),  
          data = B[!is.na(B$Inj..2),])  
anova(i)


#Hematocrit anova
j <- lmer(crit.avg ~ Inj..2*Population + (1|Family),  
          data = B[!is.na(B$Inj..2),])  
anova(j) 


