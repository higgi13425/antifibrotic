library(tidyverse)
library(ggridges)
library(ggsignif)
library(readxl)
library(here)

tidygenes <- read_excel(here::here("MST-41tidy.xlsx"),
                        sheet = "mst41_geneexp_tidy")

tidyweights <- read_excel(here::here("MST-41tidy.xlsx"),
                          sheet = "colonweight_tidy")

# analyze colon weights
ggplot(tidyweights, aes(x= as.factor(category), y = cec_col_wt) ) +
  geom_boxplot() +
  geom_signif(y_position = 0.68,comparisons = list(c("1", "3")), test="t.test") +
  geom_signif(y_position = 0.70,comparisons = list(c("3", "4")), test="t.test") +
  geom_signif(y_position = 0.72,comparisons = list(c("3", "5")), test="t.test") +
  scale_x_discrete(labels = c("1" = "Untreated", 
                              "2" = "AF Medication control",
                              "3" = "Induced Fibrosis",
                              "4" = "Fibrosis + Low Dose AF",
                              "5" = "Fibrosis + High Dose AF")) +
  labs(y = "Colon Weight (grams)", x = "Treatment Group") +
  theme(legend.position = "none", plot.title = element_text(face="bold")) +
  ggtitle("MST-41 Weight Results")

# analyze CTGF expression
tidygenes %>% 
  filter(gene == "ctgf") %>% 
  ggplot(.,aes(x= as.factor(category), y = fold) ) +
  geom_boxplot() +
  geom_signif(y_position = 6.4,comparisons = list(c("1", "3")), test="t.test") +
  geom_signif(y_position = 7.6,comparisons = list(c("3", "4")), test="t.test") +
  geom_signif(y_position = 8.5,comparisons = list(c("3", "5")), test="t.test") +
  scale_x_discrete(labels = c("1" = "Untreated", 
                              "2" = "AF Medication control",
                              "3" = "Induced Fibrosis",
                              "4" = "Fibrosis + Low Dose AF",
                              "5" = "Fibrosis + High Dose AF")) +
  labs(y = "Colon Weight (grams)", x = "Treatment Group") +
  theme(legend.position = "none", plot.title = element_text(face="bold")) +
  ggtitle("MST-41 CTGF Expression") ->
ctgf

# analyze TGFb expression
tidygenes %>% 
  filter(gene == "tgfb") %>% 
  ggplot(.,aes(x= as.factor(category), y = fold) ) +
  geom_boxplot() +
  geom_signif(y_position = 3.0,comparisons = list(c("1", "3")), test="t.test") +
  geom_signif(y_position = 3.2,comparisons = list(c("3", "4")), test="t.test") +
  geom_signif(y_position = 3.5,comparisons = list(c("3", "5")), test="t.test") +
  scale_x_discrete(labels = c("1" = "Untreated", 
                              "2" = "AF Medication control",
                              "3" = "Induced Fibrosis",
                              "4" = "Fibrosis + Low Dose AF",
                              "5" = "Fibrosis + High Dose AF")) +
  labs(y = "Colon Weight (grams)", x = "Treatment Group") +
  theme(legend.position = "none", plot.title = element_text(face="bold")) +
  ggtitle("MST-41 TGFb Expression") ->
tgfb

# analyze Collagen 1 expression
tidygenes %>% 
  filter(gene == "col1a1") %>% 
  ggplot(.,aes(x= as.factor(category), y = fold) ) +
  geom_boxplot() +
  geom_signif(y_position = 3.0,comparisons = list(c("1", "3")), test="t.test") +
  geom_signif(y_position = 3.2,comparisons = list(c("3", "4")), test="t.test") +
  geom_signif(y_position = 3.5,comparisons = list(c("3", "5")), test="t.test") +
  scale_x_discrete(labels = c("1" = "Untreated", 
                              "2" = "AF Medication control",
                              "3" = "Induced Fibrosis",
                              "4" = "Fibrosis + Low Dose AF",
                              "5" = "Fibrosis + High Dose AF")) +
  labs(y = "Colon Weight (grams)", x = "Treatment Group") +
  theme(legend.position = "none", plot.title = element_text(face="bold")) +
  ggtitle("MST-41 Collagen 1 Expression") ->
col1a1

# analyze IGF-1 expression
tidygenes %>% 
  filter(gene == "igf1") %>% 
  ggplot(.,aes(x= as.factor(category), y = fold) ) +
  geom_boxplot() +
  geom_signif(y_position = 4.9,comparisons = list(c("1", "3")), test="t.test") +
  geom_signif(y_position = 5.2,comparisons = list(c("3", "4")), test="t.test") +
  geom_signif(y_position = 5.5,comparisons = list(c("3", "5")), test="t.test") +
  scale_x_discrete(labels = c("1" = "Untreated", 
                              "2" = "AF Medication control",
                              "3" = "Induced Fibrosis",
                              "4" = "Fibrosis + Low Dose AF",
                              "5" = "Fibrosis + High Dose AF")) +
  labs(y = "Colon Weight (grams)", x = "Treatment Group") +
  theme(legend.position = "none", plot.title = element_text(face="bold")) +
  ggtitle("MST-41 IGF-1 Expression") ->
igf1


# analyze Collagen 3 expression
tidygenes %>% 
  filter(gene == "col3a1") %>% 
  ggplot(.,aes(x= as.factor(category), y = fold) ) +
  geom_boxplot() +
  geom_signif(y_position = 2.5,comparisons = list(c("1", "3")), test="t.test") +
  geom_signif(y_position = 2.8,comparisons = list(c("3", "4")), test="t.test") +
  geom_signif(y_position = 3.1,comparisons = list(c("3", "5")), test="t.test") +
  scale_x_discrete(labels = c("1" = "Untreated", 
                              "2" = "AF Medication control",
                              "3" = "Induced Fibrosis",
                              "4" = "Fibrosis + Low Dose AF",
                              "5" = "Fibrosis + High Dose AF")) +
  labs(y = "Colon Weight (grams)", x = "Treatment Group") +
  theme(legend.position = "none", plot.title = element_text(face="bold")) +
  ggtitle("MST-41 Collagen 3 Expression") ->
col3a1
