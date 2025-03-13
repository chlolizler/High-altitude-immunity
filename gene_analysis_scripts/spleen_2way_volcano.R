library(ggplot2)
library(readxl)
library(dplyr) 
library(edgeR)
library(ggrepel)
library(gprofiler2)
library(readr)

logFC_cutoff <- 0
pval_cutoff <- 0.05
log_pval_cutoff <- 1.3  # For volcano plot (-log10(0.05))

# Load and clean data
Spleen_Summary <- read.csv("/Users/chloebutler/Desktop/spleen_RNAdata/clean_large_supplemental_tables_FRACMM/S3_master_gene_list.csv")

Spleen_Summary_POP = Spleen_Summary %>%
  filter(Population_SIG == "SIG")

# Consolidate diffexpressed_EP calculation
Spleen_Summary_POP <- Spleen_Summary_POP %>%
  mutate(diffexpressed_ST = case_when(
    Population_logFC > logFC_cutoff & Population_adj_P.Val < pval_cutoff ~ "UP",
    Population_logFC < -logFC_cutoff & Population_adj_P.Val < pval_cutoff ~ "DOWN",
    TRUE ~ "NA"
  ))

Spleen_Summary_POP <- Spleen_Summary_POP %>%
  select(Gene_ID, Population_logFC, diffexpressed_ST)


Spleen_Summary_INJ = Spleen_Summary %>%
  filter(Inj_SIG == "SIG")

Spleen_Summary_INJ <- Spleen_Summary_INJ %>%
  mutate(diffexpressed_IN = case_when(
    Inj_logFC > logFC_cutoff & Inj_adj_P.Val < pval_cutoff ~ "UP",
    Inj_logFC < -logFC_cutoff & Inj_adj_P.Val < pval_cutoff ~ "DOWN",
    TRUE ~ "NA"
  ))

Spleen_Summary_INJ <- Spleen_Summary_INJ %>%
  select(Gene_ID, Inj_logFC, diffexpressed_IN)


Spleen_summary_pop_inj = full_join(Spleen_Summary_POP, Spleen_Summary_INJ, by = c("Gene_ID"))
Spleen_summary_pop_inj = na.omit(Spleen_summary_pop_inj)

sum = Spleen_summary_pop_inj %>%
  group_by(diffexpressed_ST, diffexpressed_IN) %>%
  summarise(Freq = n()) %>%
  filter(!diffexpressed_IN == "NA")


HI_LPS = Spleen_summary_pop_inj %>% filter(diffexpressed_ST == "UP", diffexpressed_IN == "UP")

HI_SAL = Spleen_summary_pop_inj %>% filter(diffexpressed_ST == "UP", diffexpressed_IN == "DOWN")

LO_SAL = Spleen_summary_pop_inj %>% filter(diffexpressed_ST == "DOWN", diffexpressed_IN == "DOWN")

LO_lps = Spleen_summary_pop_inj %>% filter(diffexpressed_ST == "DOWN", diffexpressed_IN == "UP")


lower_cutoff <- -0.5
upper_cutoff <- 0.5

# Define color mapping for the quadrants, including gray for the cutoff range
quadrant_colors <- c("Up-Up" = "#FFA500",
                     "Up-Down" = "#FF1493",
                     "Down-Down" = "cornflowerblue",
                     "Down-Up" = "#9966CC",
                     "InCutoff" = "gray")  # Added gray for points in cutoff range

# Add a 'Quadrant' column based on the combination of statuses
Spleen_summary_pop_inj <- Spleen_summary_pop_inj %>%
  mutate(Quadrant = case_when(
    Population_logFC < lower_cutoff & Inj_logFC < lower_cutoff ~ "Down-Down",
    Population_logFC < lower_cutoff & Inj_logFC > upper_cutoff ~ "Down-Up",
    Population_logFC > upper_cutoff & Inj_logFC < lower_cutoff ~ "Up-Down",
    Population_logFC > upper_cutoff & Inj_logFC > upper_cutoff ~ "Up-Up",
    Population_logFC >= lower_cutoff & Inj_logFC <= upper_cutoff &
      Population_logFC >= lower_cutoff & Inj_logFC <= upper_cutoff ~ "InCutoff",  # Gray for points in cutoff range
    TRUE ~ "NA"
  ))



# Create a 4-quadrant scatter plot
ggplot(Spleen_summary_pop_inj, aes(x = Inj_logFC, y = Population_logFC, color = Quadrant)) +
  geom_point(alpha = 0.6, size = 3) +
  scale_color_manual(values = quadrant_colors) +
  geom_hline(yintercept = upper_cutoff, linetype = "dashed") +
  geom_hline(yintercept = lower_cutoff, linetype = "dashed") +
  geom_vline(xintercept = upper_cutoff, linetype = "dashed") +
  geom_vline(xintercept = lower_cutoff, linetype = "dashed") +
  labs(
    x = "Injection Log10 Fold Change",
    y = "Population Log10 Fold Change",
    color = "Expression Status"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")




VOLCANO <- ggplot(Spleen_summary_pop_inj, aes(x = Inj_logFC, y = Population_logFC, color = Quadrant)) +
  geom_point(alpha = 0.6, size = 3) +
  scale_color_manual(values = quadrant_colors) +
  geom_hline(yintercept = upper_cutoff, linetype = "dashed") +
  geom_hline(yintercept = lower_cutoff, linetype = "dashed") +
  geom_vline(xintercept = -upper_cutoff, linetype = "dashed") +  # Negate the upper cutoff
  geom_vline(xintercept = -lower_cutoff, linetype = "dashed") +  # Negate the lower cutoff
  geom_text(aes(label = Gene_ID), vjust = 1, hjust = 1, size = 3, check_overlap = TRUE) +  # Add labels
  labs(
    x = "Injection Log10 Fold Change",
    y = "Population Log10 Fold Change",
    color = "Expression Status"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top") + scale_x_continuous(limits= c(-3.7,3.7))  + scale_y_continuous(limits= c(-3.7,3.7))


VOLCANO
ggsave("2WAY_volcano_NEW.pdf", plot = VOLCANO, device = "pdf")


library(ggplot2)
library(dplyr)
library(ggrepel)
library(gridExtra)  


# Define the top genes for each quadrant
top_gene_HI_LPS <- Spleen_summary_pop_inj %>%
  filter(Quadrant == "Up-Up") %>%
  arrange(desc(Population_logFC), desc(Inj_logFC)) %>%
  head(1)

top_gene_HI_SAL <- Spleen_summary_pop_inj %>%
  filter(Quadrant == "Up-Down") %>%
  arrange(desc(Population_logFC)) %>%
  head(1)


top_gene_LO_SAL <- Spleen_summary_pop_inj %>%
  filter(Quadrant == "Down-Down") %>%
  arrange(Population_logFC, Inj_logFC) %>%
  head(1)

top_gene_LO_LPS <- Spleen_summary_pop_inj %>%
  filter(Quadrant == "Down-Up") %>%
  arrange(desc(Inj_logFC)) %>%
  head(1)

top_gene_LO_SAL <- Spleen_summary_strain_inj %>%
  filter(Quadrant == "Down-Up") %>%
  arrange(strain_logFC, desc(Inj_logFC)) %>%  # Sort strain_logFC ascending and Inj_logFC descending
  head(1)

