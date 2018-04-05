library(tidyverse)
library(ggridges)
library(ggsignif)
library(readxl)
library(here)

#read data with readr package
df <- read_excel(here("MST-41.xlsx"), sheet = "gross", skip =4)

#clean up names
names(df) <- c('rx', 'mouse_num', 'group', 'length','junk', 'gross_wt',
               'tare', 'colon_wt', 'junk1', 'junk2', 'junk3', 'junk4',
               'junk5', 'junk6', 'junk7', 'body_wt', 'junk8','junk9',
               'junk10','junk11','junk12')

#select, filter, mutate
df <- df %>% select(-starts_with('junk'), -gross_wt, -tare) %>% 
   filter(!is.na(length)) %>% 
   mutate(colon_density = colon_wt/length, 
          colon_fraction = colon_wt/body_wt)

# t tests
# un vs drug control
with(df, t.test(colon_wt[group==1], colon_wt[group==2]))
onetwo <- with(df, t.test(colon_wt[group==1], colon_wt[group==2]))$p.value

# un vs S. typh
with(df, t.test(colon_wt[group==1], colon_wt[group==3]))
onethree <- with(df, t.test(colon_wt[group==1], colon_wt[group==3]))$p.value

#  S. typh vs. low dose ABT263
with(df, t.test(colon_wt[group==3], colon_wt[group==4]))
threefour <- with(df, t.test(colon_wt[group==3], colon_wt[group==4]))$p.value

# S. typh vs. high dose ABT263
with(df, t.test(colon_wt[group==3], colon_wt[group==5]))
threefive <- with(df, t.test(colon_wt[group==3], colon_wt[group==5]))$p.value

#plotting
#boxplot
ggplot(df, aes(x= as.factor(group), y = colon_wt)) +
  geom_boxplot()

#boxplot
ggplot(df, aes(x= as.factor(group), y = colon_wt, color=group)) +
  geom_jitter(width = 0.2)

#violin plot
ggplot(df, aes(x= as.factor(group), y = colon_wt)) +
  geom_violin(aes(fill=group)) + 
  geom_jitter(width = 0.1)

#ridge plot
gridge <- ggplot(df, aes(x = colon_wt, y= as.factor(group), fill = group)) +
  geom_density_ridges(aes(point_color=group, point_fill=group,
                          point_shape = group),
            rel_min_height =0.01, scale=0.7,
            alpha = 0.2, jittered_points=TRUE) +
  scale_discrete_manual(aesthetics = "point shape", values = c(21,22,23)) +
  labs(x = "Colon Weight (grams)", y = "Treatment Group") +
  scale_y_discrete(labels = c("1" = "Untreated", 
                              "2" = "AF Medication control",
                              "3" = "Induced Fibrosis",
                              "4" = "Fibrosis + Low Dose AF",
                              "5" = "Fibrosis + High Dose AF")) +
  theme(legend.position = "none", plot.title = element_text(face="bold")) +
  ggtitle("MST-41 Results")

gridge

# add ggsignif to boxplot
ggplot(df, aes(x= as.factor(group), y = colon_wt) ) +
  geom_boxplot() +
  geom_signif(y_position = 0.67,comparisons = list(c("1", "3"))) +
  geom_signif(y_position = 0.69,comparisons = list(c("3", "4"))) +
  geom_signif(y_position = 0.71,comparisons = list(c("3", "5"))) +
  scale_x_discrete(labels = c("1" = "Untreated", 
                              "2" = "AF Medication control",
                              "3" = "Induced Fibrosis",
                              "4" = "Fibrosis + Low Dose AF",
                              "5" = "Fibrosis + High Dose AF")) +
  labs(y = "Colon Weight (grams)", x = "Treatment Group") +
  theme(legend.position = "none", plot.title = element_text(face="bold")) +
  ggtitle("MST-41 Results")

# add ggsignif to violinplot
ggplot(df, aes(x= as.factor(group), y = colon_wt) ) +
  geom_violin() +
  geom_jitter(width = 0.1) +
  geom_signif(y_position = 0.69,comparisons = list(c("1", "3"))) +
  geom_signif(y_position = 0.71,comparisons = list(c("3", "4"))) +
  geom_signif(y_position = 0.73,comparisons = list(c("3", "5"))) +
  scale_x_discrete(labels = c("1" = "Untreated", 
                              "2" = "AF Medication control",
                              "3" = "Induced Fibrosis",
                              "4" = "Fibrosis + Low Dose AF",
                              "5" = "Fibrosis + High Dose AF")) +
  labs(y = "Colon Weight (grams)", x = "Treatment Group") +
  theme(legend.position = "none", plot.title = element_text(face="bold")) +
  ggtitle("MST-41 Results") +
  coord_flip()
