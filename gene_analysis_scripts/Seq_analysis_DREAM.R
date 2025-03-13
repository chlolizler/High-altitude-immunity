library('BiocParallel')
library('dplyr')
library('ggplot2')
library('lme4')
library('readxl')
library('variancePartition')
library('edgeR')
library('Matrix')
library("stats")
library('tibble')


######### Combined Analysis -----------------------------------------------------
Sample_Info <- read_xlsx("/Users/chloebutler/Desktop/spleen_RNAdata/Raw_Data/spleen_metadata_NEW.xlsx")
Sample_Info = as.data.frame(Sample_Info)

summary(as.factor(Sample_Info$Inj_2))
summary(as.factor(Sample_Info$Pop))

Sample_Info$Inj_2[Sample_Info$Inj_2=="LPS"] <- "ZLPS"
head(Sample_Info)

rownames(Sample_Info) = Sample_Info$RNA_Name

Sample_Info$GROUP[Sample_Info$Trial=="4"] <- "1"
Sample_Info$GROUP[Sample_Info$Trial=="3"] <- "2"
Sample_Info$GROUP[Sample_Info$Trial=="2"] <- "2"
Sample_Info$GROUP[Sample_Info$Trial=="1"] <- "2"


# Read in Files + QC
# Data munging
Pman_bairdii_rawreads <- read_xlsx("/Users/chloebutler/Downloads/Pman_GCF_readcounts.xlsx")
Pman_bairdii_rawreads <- as.data.frame(Pman_bairdii_rawreads)
Pman_bairdii_rawreads <- `row.names<-`(Pman_bairdii_rawreads, Pman_bairdii_rawreads$Geneid)
Pman_bairdii_rawreads <- Pman_bairdii_rawreads[,-c(1:6)]

Check = Sample_Info$Seq_Name
colnames(Pman_bairdii_rawreads) == Check

colnames(Pman_bairdii_rawreads) = rownames(Sample_Info)
Pman_bairdii_readcounts <- as.matrix(Pman_bairdii_rawreads)
dPman_bairdii_0 <- DGEList(Pman_bairdii_readcounts)
dPman_bairdii_0 <- calcNormFactors(dPman_bairdii_0)

# Number of unfiltered genes
dim(dPman_bairdii_0)

# Filtering
keep <- rowSums(cpm(dPman_bairdii_0) > 1 ) >= 33
dPman_bairdii <- dPman_bairdii_0[keep,]
dim(dPman_bairdii)
plotMDS(dPman_bairdii, col = as.numeric(Sample_Info$Pop), labels = Sample_Info$Pop)
rownames(Sample_Info) == colnames(dPman_bairdii)
Sample_Info$Trial <- as.factor(Sample_Info$Trial)


# Interaction
param = SnowParam(8, "SOCK", progressbar=TRUE)
form <- ~ Pop*Inj_2 + GROUP
#change this to be number of LPS injections instead of trial
vobjDream = voomWithDreamWeights(dPman_bairdii, form, Sample_Info, BPPARAM=param, plot = T)
fitmm = dream( vobjDream, form, Sample_Info, ddf = "Kenward-Roger")
fitmm = eBayes(fitmm)
print(fitmm)

# Count significance 
DE_Pop <- topTable( fitmm, coef='PopME', sort.by = "P", n = Inf, )
DE_Inj_2 <- topTable( fitmm, coef='Inj_2ZLPS', sort.by = "P", n = Inf)
DE_dose <- topTable( fitmm, coef='GROUP2', sort.by = "P", n = Inf)
DE_Ixn <- topTable( fitmm, coef='PopME:Inj_2ZLPS', sort.by = "P", n = Inf)

mergedTable <- merge(rownames_to_column(DE_Pop), rownames_to_column(DE_Inj_2), by="rowname", all.x=FALSE, all.y=TRUE)
mergedTable <- merge(mergedTable, rownames_to_column(DE_Ixn), by="rowname", all.x=FALSE, all.y=TRUE)
#write.csv(mergedTable, "Merged_POP_INJ_INX_DEG.csv")




length(DE_Pop$logFC[which(DE_Pop$adj.P.Val < 0.05)])
length(DE_Inj_2$logFC[which(DE_Inj_2$adj.P.Val < 0.05)])
length(DE_dose$logFC[which(DE_dose$adj.P.Val < 0.05)])
length(DE_Ixn$logFC[which(DE_Ixn$adj.P.Val < 0.05)])


# Summarize Reads
library(emmeans)
Pop <- Sample_Info$Pop
Inj_2 <- Sample_Info$Inj_2
GROUP <- Sample_Info$GROUP
summary <- data.frame()
all_genes <- row.names(DE_Ixn)
for (p in all_genes) {
  gene_id <- p
  test_data <- vobjDream$E[gene_id,]
  test_model <- lm(test_data ~ Pop*Inj_2 + GROUP)
  anova(test_model)
  output <- summary(pairs(emmeans(test_model, ~ Pop*Inj_2+ GROUP), adjust = "BH"))
  output_line <- output$p.value
  output_line[7] <- gene_id
  summary <- rbind(summary, output_line)
}

output_colnames <- output$contrast
colnames(summary) <- output_colnames
corrCounts <- t(vobjDream$E)
corrCounts <- as.data.frame(corrCounts)
corrCounts$Treatment <- Sample_Info$Group
corrCounts$ID <- row.names(corrCounts)
corrCounts$Check <- Sample_Info$RNA_Name

corrCounts$Check == row.names(corrCounts)
MeanCounts <- corrCounts %>%
  group_by(corrCounts$Treatment) %>%
  summarise_at(colnames(corrCounts), funs(mean(., na.rm=TRUE)))
meanCounts <- t(MeanCounts)
meanCounts <- as.data.frame(meanCounts)
colnames(meanCounts) <- meanCounts[1,]
meanCounts <- meanCounts[-1,]
meanCounts$ID <- row.names(meanCounts)
final <- merge(meanCounts, summary, by.x = "ID", by.y = c(7))

length(DE_Pop$logFC[which(DE_Pop$adj.P.Val < 0.05)])
length(DE_Inj_2$logFC[which(DE_Inj_2$adj.P.Val < 0.05)])
length(DE_dose$logFC[which(DE_dose$adj.P.Val < 0.05)])
length(DE_Ixn$logFC[which(DE_Ixn$adj.P.Val < 0.05)])

# Write Out

#write.csv(final, "/Users/chloebutler/Desktop/spleen_RNAdata/R_Output/Spleen_Contrasts_Corrected.csv")
write.csv(DE_Pop, "/Users/chloebutler/Desktop/spleen_RNAdata/R_Output/POP_DE_NoMM.csv")
write.csv(DE_Inj_2, "/Users/chloebutler/Desktop/spleen_RNAdata/R_Output/INJ_DE_NoMM.csv")
write.csv(DE_dose, "/Users/chloebutler/Desktop/spleen_RNAdata/R_Output/INJnumber_DE_NoMM.csv")
write.csv(DE_Ixn, "/Users/chloebutler/Desktop/spleen_RNAdata/R_Output/INX_DE_NoMMc.csv")






