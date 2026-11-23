#--make a bump chart of rankings
rm(list = ls())

library(tidyverse)
library(CENTSdata)
library(ggh4x)
library(patchwork)

source("code/00_color-palettes.R")



# 1. which systems win ----------------------------------------------------

d1 <- read_csv("data/tidy_weights-space-exploration.csv")

straw_cols <- c(removed = "#C8553D", retained = "#2A6F97")

# Panel A: how robust is each system's rank to the choice of weights?
#   dot  = median rank across all weightings
#   bar  = 5th-95th percentile of rank
#   text = % of weightings under which the system ranks #1 overall
pA <- 
  ggplot(d1, aes(y = till_id, colour = straw_id)) +
  geom_vline(xintercept = c(1, 10, 20, 30), colour = "grey90") +
  geom_linerange(aes(xmin = rank_lo, xmax = rank_hi),
                 linewidth = 3, alpha = 0.35,
                 position = position_dodge(width = 0.6)) +
  geom_point(aes(x = rank_med, size = win_share),
             position = position_dodge(width = 0.6)) +
  geom_text(aes(x = rank_hi,
                label = ifelse(win_share > 0, percent(win_share, 1), "")),
            hjust = -0.3, size = 3, show.legend = FALSE,
            position = position_dodge(width = 0.6)) +
  facet_wrap(~ cctrt_id, ncol = 1, strip.position = "left") +
  #facet_grid(straw_id~ cctrt_id) +
  scale_x_continuous(breaks = c(1, 10, 20, 30),
                     expand = expansion(mult = c(0.02, 0.12))) +
  scale_colour_manual(values = straw_cols, name = "Straw") +
  scale_size_area(max_size = 6, labels = percent, name = "Share of weightings\nwhere system ranks #1") +
  labs(title = "How robust is each system to the choice of weights?",
       subtitle = "Dot = median rank; bar = 5th\u201395th percentile of rank across all weight combinations",
       x = "Rank among the 30 systems", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 90, face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title.position = "plot")

pA

ggsave("figs/sfig_weighting-space.png", width = 6.51, height = 5.02)


# 2. not as clear -----------------------------------------------------------------

metric_cols <- c("#8DA0CB", "#66C2A5", "#E5B93C", "#D95F5F", "#9B7EBD", "#7FB069")

d2 <- 
  MakeNiceLabels(read_csv("data/tidy_system-winning-weight-profile.csv")) |>
  mutate(straw_nice = ifelse(straw_id == "retained", "(+)Straw", "(-)Straw"),
         till_nice = case_when(
           till_nice == "Inv"~"Inversion",
           till_nice == "Surf"~"Surface",
           TRUE ~ "No-till"
         ),
         till_nice = factor(till_nice, 
                            levels = c("Inversion", "Surface", "No-till")))


# Order cropping-system rows by overall median rank
cc_order <- 
  d2 |>
  group_by(cctrt_nice) |>
  summarise(m = median(rank_med)) |>
  arrange(desc(m)) |>
  pull(cctrt_nice)

win_profile <- 
  d2 |> 
  mutate(cctrt_nice = factor(cctrt_nice, levels = cc_order))

metrics <- d2 |> pull(metric) |> unique()

pB <- 
  ggplot(win_profile,
             aes(x = mean_weight, y = interaction(cctrt_nice, straw_nice, sep = " "), fill = metric)) +
  geom_col(width = 0.75, colour = "white", linewidth = 0.3) +
  facet_wrap(~ till_nice, ncol = 1, scales = "free_y") +
  scale_x_continuous(labels = percent, expand = c(0, 0)) +
  scale_fill_manual(values = setNames(metric_cols, metrics), name = "Metric") +
  labs(title = "Weight profile under which each winning system ranks #1",
       subtitle = "Only systems that win under at least one weighting are shown",
       x = "Average weight on each metric", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        plot.title.position = "plot",
        strip.text = element_text(face = "bold"))

pB
