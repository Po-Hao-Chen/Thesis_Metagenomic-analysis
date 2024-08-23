#test

library(ggplot2)
library(readr)
library(gridExtra)
library(patchwork)
library(grid)
library(cowplot)
library(stringr)
library(forcats)
library(dplyr)

# convert the table to a data frame
HMM_Final <- read.csv("B50_HMMFinal_loose_ForBarChart.csv", header=TRUE, sep = ',')

# Replace all "Engineered" values in column "ecosystem" with "Managed"
HMM_Final$ecosystem[HMM_Final$ecosystem == "Engineered"] <- "Managed"

# Replace all "Non-marine Saline and Alkaline" values in column "ecosystem_category" with "Soda lake"
HMM_Final$ecosystem_category[HMM_Final$ecosystem_category == "Non-marine Saline and Alkaline"] <- "Soda lake"

# Add new column with percentage values
HMM_Final <- HMM_Final %>%
  group_by(ecosystem_category) %>%
  mutate(percent = NumberOfMAGs / sum(NumberOfMAGs) * 100)
HMM_Final


# Check unique value of dataframe
unique(HMM_Final$ecosystem)
unique(HMM_Final$ecosystem_category)
unique(HMM_Final$taxonomy)

# Change taxonomy order
taxonomy_order <- c( "Unclassified Bacteria", 'Other Bacterial Phylum','Myxococcia', 'Thermodesulfobacterota',
                     "Chloroflexi", "Alphaproteobacteria", "Gammaproteobacteria", "Actinobacteria")

HMM_Final$taxonomy <- fct_relevel(HMM_Final$taxonomy, taxonomy_order)
HMM_Final

# change ecosystem order
ecosystem_order <- c("Aquatic", "Terrestrial", "Host-associated", "Managed")
HMM_Final$ecosystem <- fct_relevel(HMM_Final$ecosystem, ecosystem_order)

# create the color vector
class_colors <- c("Alphaproteobacteria" = "#2e75b6",
                  "Gammaproteobacteria" = "#9dc3e6", 
                  "Actinobacteria" = "#c00000",
                  "Chloroflexi" = "#a9d18e",
                  'Thermodesulfobacterota' = '#ffd966',
                  "Myxococcia" = "#7e33b8",
                  "Other Bacterial Phylum" = "#404040",
                  "Unclassified Bacteria" = "#cbcbcb"
)

# ggplot No legend
p <- ggplot(HMM_Final, aes(x = str_wrap(ecosystem_category, width = 30), y = percent, fill = taxonomy)) +
  geom_bar(stat = "identity",
           width = ifelse(HMM_Final$ecosystem == "Terrestrial", 0.4, 0.8),
           position = "stack") +
  labs(title = "",
       x ="",
       y = "")+
  scale_fill_manual(values = class_colors,
                    limits = c("Actinobacteria", "Chloroflexi", "Alphaproteobacteria", "Gammaproteobacteria",
                               'Myxococcia', 'Thermodesulfobacterota', 'Other Bacterial Phylum', "Unclassified Bacteria"))+
  theme_bw()+
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 11, margin = margin(r = 10), face = "bold"),
        strip.text = element_text(size = 11, face = "bold"),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1, color ="black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.margin = unit(c(-0.5, 0.1, -0.5, 0.8), "cm"),
        text = element_text(family = "Arial"))+
  scale_y_continuous(expand = c(0.01, 0)) +
  facet_wrap(~ecosystem, scales = "free_x", nrow = 1)+
  guides(fill = guide_legend(ncol = 4))
p

# save plot with ggsave
# unit = 'cm': 高圖參數: (width = 18.18, height = 26.67, dpi = 300) / 矮圖參數: (width = 18.18, height = 7, dpi = 300)
ggsave("MAGsHits_PercentageBarChart_loose1.tiff", p, width = 18.18, height = 7, dpi = 300, units = "cm")

