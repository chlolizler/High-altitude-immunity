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

#Load full residuals data (includes both populations but not saline controls)
setwd("/Users/chloebutler/Desktop/Tb analysis/")
df <-read.csv("mergedresid.csv", header= TRUE)

#Run a linear mixed effect model on the residuals (post-injection - pre-injection temperature values) for each day

#Overall
aa <- lmer(residuals ~ Pop + (1 |ID), data = df)
summary(aa)
anova(aa)

#24 hour periods
po24 <- df[df$TRI > 0 & df$TRI < 24, ]
a <- lmer(residuals ~ Pop + (1 |ID), data = po24)
summary(a)
anova(a)

po48 <- df[df$TRI > 24 & df$TRI < 48, ]
b <- lmer(residuals ~ Pop + (1 |ID), data = po48)
summary(b)
anova(b)

po72 <- df[df$TRI > 48 & df$TRI < 72, ]
c <- lmer(residuals ~ Pop + (1 |ID), data = po72)
summary(c)
anova(c)

po96 <- df[df$TRI > 72 & df$TRI < 96, ]
d <- lmer(residuals ~ Pop + (1 |ID), data = po96)
summary(d)
anova(d)

po120 <- df[df$TRI > 96 & df$TRI < 120, ]
e <- lmer(residuals ~ Pop + (1 |ID), data = po120)
summary(e)
anova(e)

po144 <- df[df$TRI > 120 & df$TRI < 144, ]
f <- lmer(residuals ~ Pop + (1 |ID), data = po144)
summary(f)
anova(f)

po168 <- df[df$TRI > 144 & df$TRI < 168, ]
g <- lmer(residuals ~ Pop + (1 |ID), data = po168)
summary(g)
anova(g)


#Phase-specific linear mixed effect models (divided into day and night)
pos12 <- df[df$TRI > 0 & df$TRI < 12, ]
h <- lmer(residuals ~ Pop + (1 |ID), data = pos12)
summary(h)
anova(h)

pos24 <- df[df$TRI > 12 & df$TRI < 24, ]
i <- lmer(residuals ~ Pop + (1 |ID), data = pos24)
summary(i)
anova(i)

pos36 <- df[df$TRI > 24 & df$TRI < 36, ]
j <- lmer(residuals ~ Pop + (1 |ID), data = pos36)
summary(j)
anova(j)

pos48 <- df[df$TRI > 36 & df$TRI < 48, ]
k <- lmer(residuals ~ Pop + (1 |ID), data = pos48)
summary(k)
anova(k)

pos60 <- df[df$TRI > 48 & df$TRI < 60, ]
l <- lmer(residuals ~ Pop + (1 |ID), data = pos60)
summary(l)
anova(l)

pos72 <- df[df$TRI > 60 & df$TRI < 72, ]
m <- lmer(residuals ~ Pop + (1 |ID), data = pos72)
summary(m)
anova(m)

pos84 <- df[df$TRI > 72 & df$TRI < 84, ]
n <- lmer(residuals ~ Pop + (1 |ID), data = pos84)
summary(n)
anova(n)

pos96 <- df[df$TRI > 84 & df$TRI < 96, ]
o <- lmer(residuals ~ Pop + (1 |ID), data = pos96)
summary(o)
anova(o)

pos108 <- df[df$TRI > 96 & df$TRI < 108, ]
p <- lmer(residuals ~ Pop + (1 |ID), data = pos108)
summary(p)
anova(p)

pos120 <- df[df$TRI > 108 & df$TRI < 120, ]
q <- lmer(residuals ~ Pop + (1 |ID), data = pos120)
summary(q)
anova(q)

pos132 <- df[df$TRI > 120 & df$TRI < 132, ]
r <- lmer(residuals ~ Pop + (1 |ID), data = pos132)
summary(r)
anova(r)

pos144 <- df[df$TRI > 132 & df$TRI < 144, ]
s <- lmer(residuals ~ Pop + (1 |ID), data = pos144)
summary(s)
anova(s)
