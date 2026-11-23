#--use emmeans estimates for yield figure
#--9 april updated to include yields separated by tillage effect (which was sig)
#--21 oct 2026 eliminate dk average lines, they are distracting

library(CENTSdata)
library(patchwork)
library(tidyverse)
library(ggbeeswarm)

rm(list = ls())


source("code/00_color-palettes.R")

w <- cents_wea
y <- read_csv("data/stats/figs_emmeans/emmeans-fig1-yields.csv")
yraw <- cents_cropyields
#cents_cropyields

dk <- 
  read_csv("data/tidy_dk-avg-yields.csv") %>% 
  mutate(crop = c("2018 (Spring barley)", "2019 (Spring oat)", "2020 (Faba bean)"))

# plot1 precip ------------------------------------------------------------------

w2 <- 
  w %>% 
  group_by(year) %>% 
  mutate(cprec_mm = cumsum(prec_mm),
         dprec_mm = cprec_mm - LTcprec_mm) %>% 
  ungroup() %>% 
  mutate(yearH = case_when(
    year == 2018 ~ "2018 (Spring barley)", 
    year == 2019 ~ "2019 (Spring oat)",
    year == 2020 ~ "2020 (Faba bean)", 
    TRUE ~ "1990-2020"
  )) %>% 
  mutate(cprecip = round(max(LTcprec_mm))) 

p1 <-
  w2 %>% 
  mutate(facet = "Cumulative Precipitation") %>% 
  filter(doy < 364) %>% 
  ggplot() +
  geom_line(aes(doy, dprec_mm, group = year, color = yearH)) +
  geom_hline(yintercept = 0) +
  geom_line(data = . %>% filter(year %in% c(2018, 2019, 2020)), 
            aes(doy, dprec_mm, group = year, color = yearH), linewidth = 1.2) +
  scale_color_manual(values = c("gray80", ylw1, dkbl1, c2)) +
  theme_bw() + 
 th1 +
  labs(x = "Day of year",
       y = "Deviation from\nLT mean (mm)", 
       color = NULL) +
  facet_grid(.~facet) +
  theme(legend.direction = "horizontal")

p1

# plot 2 (temperature) ------------------------------------------------------------------

t3 <- 
  w %>% 
  group_by(year) %>% 
  mutate(dt_c = avgte - LTavgte,
         cdt_c = cumsum(dt_c)) %>% 
  ungroup() %>% 
  mutate(yearH = case_when(
    year == 2018 ~ "2018 (Spring barley)", 
    year == 2019 ~ "2019 (Spring oat)",
    year == 2020 ~ "2020 (Faba bean)", 
    TRUE ~ "1990-2020"
  )) %>% 
  mutate(meanT = round(mean(LTavgte))) 



p2 <- 
  t3 %>% 
  mutate(facet = "Air Temperature") %>% 
  filter(doy < 364) %>% 
  ggplot() +
  geom_line(aes(doy, cdt_c, group = year, color = yearH)) +
  geom_hline(yintercept = 0) +
  geom_line(data = . %>% filter(year %in% c(2018, 2019, 2020)), 
            aes(doy, cdt_c, group = year, color = yearH), linewidth = 1.2) +
  scale_color_manual(values = c("gray80", ylw1, dkbl1, c2)) +
  theme_bw() + 
  th1 +
  labs(x = "Day of year",
       y = "Deviation from\nLT mean (deg C - days)", 
       color = NULL) +
  facet_grid(.~facet)+
  theme(legend.direction = "horizontal")


p2

# plot 3 square plot -------------------------------------------------------------


LTprec <-
  w2 %>% 
  dplyr::select(cprecip) %>% 
  distinct() %>% 
  pull()

LTtemp <-
  t3 %>% 
  dplyr::select(meanT) %>% 
  distinct() %>% 
  pull()

d3 <- 
  w2 %>% 
  dplyr::select(doy, year, cprecip, cprec_mm) %>% 
  group_by(year) %>% 
  filter(cprec_mm == max(cprec_mm)) %>% 
  left_join(
    t3 %>% 
      dplyr::select(doy, year, meanT, avgte) %>% 
      group_by(year, meanT) %>% 
      summarise(avgT = mean(avgte))
  ) %>% 
  mutate(yearH = case_when(
    year == 2018 ~ "2018 (Spring barley)", 
    year == 2019 ~ "2019 (Spring oat)",
    year == 2020 ~ "2020 (Faba bean)", 
    TRUE ~ "1990-2020"
  )) %>% 
  mutate(fakefacet = "Weather Summary")

p3 <- 
  ggplot() + 
  geom_point(data = d3 %>% filter(yearH == "1990-2020"),
             aes(x = cprec_mm, y = avgT), 
             size = 4, color = "gray80") +
  geom_point(data = d3 %>% filter(yearH == "2018 (Spring barley)"),
             aes(x = cprec_mm, y = avgT), 
             size = 4, color = ylw1, shape = 15) +
  geom_point(data = d3 %>% filter(yearH == "2019 (Spring oat)"),
             aes(x = cprec_mm, y = avgT), 
             size = 4, color = dkbl1, shape = 17) +
  geom_point(data = d3 %>% filter(yearH == "2020 (Faba bean)"),
             aes(x = cprec_mm, y = avgT), 
             size = 4, color = c2, , shape = 19) +
  geom_vline(xintercept = LTprec) +
  geom_hline(yintercept = LTtemp) +
  geom_text(aes(x = 700, y = 7, label = "Wet and cool"), 
            check_overlap = T, hjust = 0.5, 
            fontface = "italic",  color = "gray35") +
  geom_text(aes(x = 700, y = 11, label = "Wet and hot"), 
            check_overlap = T, hjust = 0.5, 
            fontface = "italic", color = "gray35") +
  geom_text(aes(x = 475, y = 7, label = "Dry and cool"), 
            check_overlap = T, hjust = 0.5, 
            fontface = "italic", color = "gray35") +
  geom_text(aes(x = 475, y = 11, label = "Dry and hot"), 
            check_overlap = T, hjust = 0.5, 
            fontface = "italic", color = "gray35") +
  scale_color_manual(values = c("gray80", ylw1, dkbl1)) +
  scale_x_continuous(limits = c(400, 800)) +
  scale_y_continuous(limits = c(7, 11)) +
  theme_bw() +
  th1 +
  labs(x = "Mean cumulative precipitation\n(mm)",
       y = "Mean air temperatue\n(deg C)",
       color = NULL)+
  facet_grid(.~fakefacet)

p3

# plot 4 yields ------------------------------------------------------------------


d <- 
  y |> 
  mutate(estimate = emmean) %>% 
  left_join(
    yraw |> 
      left_join(cents_eukey) |> 
      dplyr::select(-date2)
  ) |> 
  distinct() |> 
  mutate(
    cctrt_nice = case_when(
      cctrt_id == "nocc" ~ "NoCC",
      cctrt_id == "mix_E" ~ "MixE",
      cctrt_id == "mix_M" ~ "MixM",
      cctrt_id == "rad_M" ~ "RadM",
      cctrt_id == "rad_L" ~ "RadL",
      TRUE~"XXX"
    ),
    cctrt_id = factor(cctrt_id, levels = ord.cctrt_id),
    cctrt_nice = factor(cctrt_nice, levels = ord.cctrt_niceS),
    crop = case_when(
      crop == "faba bean" ~ "2020 (Faba bean)",
      crop == "oat" ~ "2019 (Spring oat)",
      crop == "spring barley" ~ "2018 (Spring barley)"
    ),
    crop = factor(crop, levels = ord.crop)) 

# ai help ----------------------------------------------------------------------

# Order treatments with the control first; shorter labels
trt_levels <- c("NoCC", "MixE", "MixM", "RadM", "RadL")
d <- d %>% mutate(cctrt_nice = factor(cctrt_nice, levels = trt_levels))

# One row per treatment x crop for the estimates
est <- d %>%
  distinct(crop, cctrt_nice, cctrt_id, emmean, lower.CL, upper.CL)

# Control mean per crop, as a reference line
ctrl <- est %>% filter(cctrt_nice == "NoCC")

# Significance marker placed just above the top of the CI
star <- est %>% filter(cctrt_id == "rad_M")
# 
# crop_cols <- c("2018 (Spring barley)" = "#E69F00",
#                "2019 (Spring oat)"    = "#0072B2",
#                "2020 (Faba bean)"     = "#009E73")

crop_cols <- c("2018 (Spring barley)" = ylw1,
               "2019 (Spring oat)"    = dkbl1,
               "2020 (Faba bean)"     = c2)

p4 <- 
  ggplot(est, aes(cctrt_nice, emmean, color = crop)) +
  geom_hline(data = ctrl, aes(yintercept = emmean),
             linetype = "dashed", color = "grey60", linewidth = 0.4) +
  # raw plot-level observations
  geom_jitter(data = d, aes(y = yield_dry_Mgha),
              width = 0.12, height = 0, color = "grey80",
              size = 0.9, alpha = 0.7) +
  # 95% CI
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.18, linewidth = 0.7) +
  # estimated marginal means
  geom_point(aes(fill = crop, shape = crop), color = "white",
             size = 3.4, stroke = 0.8, show.legend = F) +
  geom_text(data = star, aes(y = upper.CL + 0.15, label = "*"),
            size = 7, color = "black", vjust = 0) +
  guides(shape = "none",
         fill = "none",
         color = "none") +
  facet_grid(. ~ crop) +
  scale_color_manual(values = crop_cols) +
  scale_fill_manual(values = crop_cols) +
  scale_shape_manual(values = c(22, 24, 21)) +
  scale_y_continuous(limits = c(0, 6), breaks = 0:6,
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL,
       y = "Dry grain yield (Mg ha<sup>-1</sup>)",
       caption = "Coloured points: estimated marginal means with 95% CI. Grey points: plot-level yields. Dashed line: NoCC mean. * = significant difference from NoCC.") +
  theme_classic(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.y = element_markdown(margin = margin(r = 8)),
        axis.line = element_line(linewidth = 0.4),
        axis.ticks = element_line(linewidth = 0.4),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
        panel.spacing = unit(1.2, "lines"),
        plot.caption = element_text(size = 8, color = "grey35", hjust = 0))

#ggsave("yield_figure.png", width = 7.5, height = 4, dpi = 300)
p4

# put together ------------------------------------------------------------


(p3 + p2 + p1) / p4 + plot_layout(guides = 'collect',
                                  heights = c(1, 1.5)) & theme(legend.position = 'top')


ggsave("figs/fig1_wea-and-yields.png", width = 9, height = 7)

