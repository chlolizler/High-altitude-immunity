library(ggplot2)
library(readxl)
library(dplyr) 
library(edgeR)
library(ggrepel)
library(gprofiler2)
library(readr)
getwd()
setwd("/Users/chloebutler/Desktop/spleen_RNAdata")

# Background Read Data ---------------------
Spleen_Pop = read.csv("R_Output/POP_DE_Corrected.csv")
Spleen_Inj = read.csv("R_Output/INJ_DE_Corrected.csv")
Spleen_Pop <- Spleen_Pop %>%
  rename(ID = X)
Spleen_Inj <- Spleen_Inj %>%
  rename(ID = X)


# MUNGE ---------------------
# DE Data
Spleen_Pop = Spleen_Pop[,c("ID","logFC","adj.P.Val")]
Spleen_Inj = Spleen_Inj[,c("ID","logFC","adj.P.Val")]


# GO ENRICHMENT ---------------------

# Background Read Data ---------------------

background = Spleen_Pop$ID

# SPLEEN POP ---------------

Spleen_Pop <- Spleen_Pop %>%
  filter(adj.P.Val < 0.05)

Spleen_Pop$diffexpressed <- ifelse(Spleen_Pop$logFC > 0.5 & Spleen_Pop$adj.P.Val < 0.05, "UP",
                                   ifelse(Spleen_Pop$logFC < -0.5 & Spleen_Pop$adj.P.Val < 0.05, "DOWN", "NA"))

count(Spleen_Pop %>% filter(diffexpressed == "UP"))
count(Spleen_Pop %>% filter(diffexpressed == "DOWN"))


# GO  ------------------------
UP = Spleen_Pop %>% filter(diffexpressed == "UP")
UP = as.vector(UP$ID)

Down = Spleen_Pop %>% filter(diffexpressed == "DOWN")
Down = as.vector(Down$ID)


#GO Enrichment Analysis
#Up ------------------------
Up_GO = gost(UP, 
             organism = "pmbairdii",
             custom_bg = background,
             ordered_query = F,
             exclude_iea = F,
             correction_method = "fdr",
             evcodes = T,
             significant=F)

head(Up_GO$result)
gostplot(Up_GO, interactive = TRUE, capped = FALSE)

UP_Results = data.frame(Cluster = Up_GO$result$query,
                        Term.ID = Up_GO$result$term_id,
                        Term.Name = Up_GO$result$term_name,
                        geneid = Up_GO$result$intersection,
                        P.value = Up_GO$result$p_value,
                        Source = Up_GO$result$source,
                        Term.Size = Up_GO$result$term_size,
                        Precision = Up_GO$result$precision,
                        intersection_size = Up_GO$result$intersection_size,
                        query_size = Up_GO$result$query_size,
                        Gene_ratio = as.numeric((Up_GO$result$intersection_size/
                                                   Up_GO$result$query_size)))





# SPLEEN INJ ---------------

Spleen_Inj <- Spleen_Inj %>%
  filter(adj.P.Val < 0.05)

Spleen_Inj$diffexpressed <- ifelse(Spleen_Inj$logFC > 0.5 & Spleen_Inj$adj.P.Val < 0.05, "Up",
                                   ifelse(Spleen_Inj$logFC < -0.5 & Spleen_Inj$adj.P.Val < 0.05, "Down", "NA"))

count(Spleen_Inj %>% filter(diffexpressed == "Up"))
count(Spleen_Inj %>% filter(diffexpressed == "Down"))



# GO  ------------------------
Up_INJ = Spleen_Inj %>% filter(diffexpressed == "Up")
Up_INJ = as.vector(Up_INJ$ID)


Down_INJ = Spleen_Inj %>% filter(diffexpressed == "Down")
Down_INJ = as.vector(Down_INJ$ID)



#GO Enrichment Analysis
#Up ------------------------
Up_GO = gost(Up_INJ, 
             organism = "pmbairdii",
             custom_bg = background,
             ordered_query = F,
             significant = TRUE,
             correction_method = "fdr",
             evcodes = T)


head(Up_GO$result)
gostplot(Up_GO, interactive = TRUE, capped = FALSE)

Up_ResultsInj = data.frame(Cluster = Up_GO$result$query,
                           Term.ID = Up_GO$result$term_id,
                           Term.Name = Up_GO$result$term_name,
                           geneid = Up_GO$result$intersection,
                           P.value = Up_GO$result$p_value,
                           Source = Up_GO$result$source,
                           Term.Size = Up_GO$result$term_size,
                           Precision = Up_GO$result$precision,
                           intersection_size = Up_GO$result$intersection_size,
                           query_size = Up_GO$result$query_size,
                           Gene_ratio = as.numeric((Up_GO$result$intersection_size/
                                                      Up_GO$result$query_size)))

