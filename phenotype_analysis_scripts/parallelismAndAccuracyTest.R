library(car)

p <- read.csv(file = "ParallelismData.csv", header = TRUE)
test <- lm(avg.abs ~ logconc*curve, data = p)
Anova(test, type = "III")

ggplot(p, aes(x = logconc, y = avg.abs, colour = curve))+
  geom_point()+
  geom_smooth(method = "lm")

a <- read.csv(file = "accuracyData.csv", header = TRUE)

test <- lm(log10(poolwStd_quant) ~ log10(poolwStd_expected), data = a)
test
ggplot(a, aes(x = log10(poolwStd_expected), y = log10(poolwStd_quant)))+
  geom_point()+
  geom_smooth(method = "lm")
