# Load daily residual medians data
setwd("/Users/chloebutler/Desktop/Tb analysis/residuals")
daily <- read.csv("dailyresidmedians.csv", header = TRUE)

# Mann-whitney u test/wilcoxon rank sum test between median residual values. The median values were collected for every individual for the first six days post-injection.

# Overall
kn <- daily$Median[daily$Population == "KN"]
me <- daily$Median[daily$Population == "ME"]

result <- wilcox.test(kn, me, paired= FALSE)

print(result)
# ^significant


# Day 1
firstday <- daily[daily$Window.start == 0, ]
KN1 <- firstday$Median[firstday$Population == "KN"]
ME1 <- firstday$Median[firstday$Population == "ME"]

wilcox.test(KN1, ME1, paired= FALSE)

# Day 2
secondday <- daily[daily$Window.start == 24, ]
KN2 <- secondday$Median[secondday$Population == "KN"]
ME2 <- secondday$Median[secondday$Population == "ME"]

wilcox.test(KN2, ME2, paired= FALSE)

# Day 3
thirdday <- daily[daily$Window.start == 48, ]
KN3 <- thirdday$Median[thirdday$Population == "KN"]
ME3 <- thirdday$Median[thirdday$Population == "ME"]

wilcox.test(KN3, ME3, paired= FALSE)

# Day 4
fourthday <- daily[daily$Window.start == 72, ]
KN4 <- fourthday$Median[fourthday$Population == "KN"]
ME4 <- fourthday$Median[fourthday$Population == "ME"]

wilcox.test(KN4, ME4, paired= FALSE)


# Day 5
fifthday <- daily[daily$Window.start == 96, ]
KN5 <- fifthday$Median[fifthday$Population == "KN"]
ME5 <- fifthday$Median[fifthday$Population == "ME"]

wilcox.test(KN5, ME5, paired= FALSE)

# Day 6
sixthday <- daily[daily$Window.start == 120, ]
KN6 <- sixthday$Median[sixthday$Population == "KN"]
ME6 <- sixthday$Median[sixthday$Population == "ME"]

wilcox.test(KN6, ME6, paired= FALSE)



# Every 2 days

# Load 2 day residual medians data
setwd("/Users/chloebutler/Desktop/")
twodays <- read.csv("twodayresidualmedians.csv", header = TRUE)

# Day 1-2
firsttwoday <- twodays[twodays$Start.Day == 1, ]
KN7 <- firsttwoday$Median[firsttwoday$Population == "KN"]
ME7 <- firsttwoday$Median[firsttwoday$Population == "ME"]

wilcox.test(KN7, ME7, paired= FALSE)


# Day 3-4
secondtwoday <- twodays[twodays$Start.Day == 3, ]
KN8 <- secondtwoday$Median[secondtwoday$Population == "KN"]
ME8 <- secondtwoday$Median[secondtwoday$Population == "ME"]

wilcox.test(KN8, ME8, paired= FALSE)

#Day 5-6
thirdtwoday <- twodays[twodays$Start.Day == 5, ]
KN9 <- thirdtwoday$Median[thirdtwoday$Population == "KN"]
ME9 <- thirdtwoday$Median[thirdtwoday$Population == "ME"]

wilcox.test(KN9, ME9, paired= FALSE)


