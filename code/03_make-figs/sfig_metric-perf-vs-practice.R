#--make a bump chart of rankings
rm(list = ls())

library(tidyverse)
library(CENTSdata)
library(ggh4x)
library(patchwork)
library(scales)

source("code/00_color-palettes.R")

# 1. data -----------------------------------------------------------------

draw <- read_csv("data/tidy_multicriteria-values.csv") 


d <- 
  draw |> 
  mutate(
    cctrt_id = factor(cctrt_id, levels = c("nocc", "mix_E", "mix_M", "rad_L", "rad_M"),
                      labels = c("NoCC", "MixE", "MixM", "RadL", "RadM")),
    till_id  = factor(till_id, levels = c("inversion", "surface", "notill"),
                      labels = c("Inversion", "Surface", "No-till")),
    straw_id = factor(straw_id, levels = c("removed", "retained"),
                      labels = c("Removed", "Retained")),
    name = factor(name,
                  levels = c("yield", "pctcov", "fallbio", "fallvegvalue", "perennweeds", "minitox"),
                  labels = c("Yield", "Percent cover", "Fall biomass",
                             "Fall veg. value", "Perennial weeds", "Pest. toxicity")))

# NOTE: the label text above is my guess at what each code means; edit to match yours.

# Long format: one row per factor level, so the three factors can share one plot.
# The design is fully crossed (5 x 3 x 2), so averaging over the other two
# factors gives a fair marginal mean for each level.

long <- d %>%
  pivot_longer(c(cctrt_id, till_id, straw_id),
               names_to = "factor", values_to = "level") %>%
  mutate(factor = factor(factor,
                         levels = c("cctrt_id", "till_id", "straw_id"),
                         labels = c("Cover crop", "Tillage", "Straw")))

marg <- long %>%
  group_by(name, factor, level) %>%
  summarise(mean = mean(value), lo = min(value), hi = max(value), .groups = "drop")

# ---------------------------------------------------------------
# Figure 1: heatmap of marginal means (the overview)
# ---------------------------------------------------------------
p1 <- ggplot(marg, aes(level, fct_rev(name), fill = mean)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = number(mean, accuracy = 0.01),
                color = mean > 0.6), size = 3.2, show.legend = FALSE) +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "black")) +
  scale_fill_viridis_c(option = "viridis", limits = c(0, 1),
                       name = "Mean value\n(0–1)") +
  facet_grid(. ~ factor, scales = "free_x", space = "free_x") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.spacing = unit(0.8, "lines"))

p1

ggsave("figs/sfig_heatmap.png", width = 8, height = 3.8, dpi = 300)


