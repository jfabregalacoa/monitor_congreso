################################################################################
# This script is provided for replication of 
# Fábrega, Jorge, 2025, "Ideological Estimates of the Chilean Chamber of Deputies, 2002–2026", 
# https://doi.org/10.7910/DVN/FOXOIT, Harvard Dataverse, 
# DRAFT VERSION, UNF:6:0R5mlR/SgWltI4kra2OeVg== [fileUNF] 
#
#
# If you have any question or comment please reach me at:
# Contact email: jfabrega@udd.cl
################################################################################

rm(list=ls())

library(here)
library(GGally)

aqui <- here()

base_actualizada <- read.csv(paste0(aqui,"/results/ideologia_congreso_chile_2002_2026_long_format.csv"))

cor_data <- base_actualizada %>%
  filter(!is.na(dim_ideology), !is.na(bay_ideology), !is.na(wnom_ideology)) %>%
  select(dim_ideology, bay_ideology, wnom_ideology)

ggpairs(cor_data,
        lower = list(continuous = wrap("smooth", alpha = 0.3)),
        diag = list(continuous = "densityDiag"),
        upper = list(continuous = wrap("cor", size = 4))) +
  theme_minimal()

ggsave(paste0(aqui,"/results/figure1_correlation_matrix.png"), width = 8, height = 8, dpi = 300)

library(ggplot2)
library(ggridges)

base_actualizada %>%
  filter(!is.na(dim_ideology)) %>%
  mutate(Period = factor(Period, levels = paste0("Periodo_", 1:6))) %>%
  ggplot(aes(x = dim_ideology, y = Period, fill = Period)) +
  geom_density_ridges(scale = 1.1, alpha = 0.8, color = "white") +
  labs(
    title = "Distribution of ideological estimates by legislative period",
    x = "Dynamic ideological estimate",
    y = "Legislative period"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(paste0(aqui,"/results/figure2_ideology_distribution.png"), width = 10, height = 6, dpi = 300)
