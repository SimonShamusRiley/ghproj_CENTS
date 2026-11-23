# ------------------------------------------------------------------
# Multi-criteria assessment of 30 cropping systems across ALL weight
# combinations of six metrics.
#
# Idea: sweep every weight vector w (w_i >= 0, sum(w) = 1, step = 1/n),
# compute each system's weighted-sum score, rank the systems under each
# weighting, then ask: how robust is each system's performance to the
# choice of weights?
# ------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(scales)

# ---- 1. Settings --------------------------------------------------
infile <- read_csv("data/tidy_multicriteria-values.csv")

# Weight resolution: weights move in steps of 1/n_steps.
#   n_steps = 10 ->   3,003 weight combinations
#   n_steps = 20 ->  53,130 weight combinations (still fast)
n_steps <- 20

# Values look pre-scaled to 0-1. Set FALSE for any metric where a LOWER
# value is better (it will be flipped to 1 - value). ASSUMPTION: all TRUE.
higher_is_better <- c(
  fallvegvalue = TRUE, fallbio = TRUE, yield = TRUE,
  minitox = TRUE, perennweeds = TRUE, pctcov = TRUE
)

# ---- 2. Data -> matrix (30 systems x 6 metrics) ------------------
dat <- infile

wide <- dat |>
  pivot_wider(names_from = name, values_from = value) |>
  arrange(till_id, cctrt_id, straw_id)

metrics <- names(higher_is_better)
V <- as.matrix(wide[, metrics])
for (m in metrics) if (!higher_is_better[[m]]) V[, m] <- 1 - V[, m]

systems <- wide |> select(cctrt_id, till_id, straw_id)

# ---- 3. All weight combinations (simplex grid) -------------------
# Stars-and-bars: every way to split n_steps units across k metrics
make_weights <- function(k, n) {
  bars <- utils::combn(n + k - 1, k - 1)
  parts <- apply(bars, 2, function(b) diff(c(0, b, n + k)) - 1)
  t(parts) / n                       # rows = weight vectors
}
W <- make_weights(length(metrics), n_steps)
colnames(W) <- metrics
stopifnot(all(abs(rowSums(W) - 1) < 1e-9))
message(nrow(W), " weight combinations")

# ---- 4. Score and rank every system under every weighting --------
S <- V %*% t(W)                                        # 30 x n_weights
R <- apply(-S, 2, rank, ties.method = "min")           # 1 = best

# ---- 5. Summaries per system -------------------------------------

#--I actually want the huge 
s.df <- 
  S |> 
  as.data.frame()  
  
res <- cbind(systems, s.df)

res2 <- 
  res |> 
  pivot_longer(V1:V3003) |> 
  mutate(cctrt_straw = paste(cctrt_id, straw_id, sep = "_")) 

res2 |> 
  ggplot(aes(cctrt_straw, value)) +
  geom_violin() +
  facet_grid(. ~ till_id) +
  coord_flip()

res2 |> 
  ggplot(aes(cctrt_straw, value)) +
  geom_jitter() +
  facet_grid(. ~ till_id) +
  coord_flip()

#--want to try ggridges

systems2 <- 
  systems |>
  mutate(
    score_mean = rowMeans(S),
    score_lo = apply(S, 1, quantile, probs = 0.05),
    score_hi = apply(S, 1, quantile, probs = 0.95),
    win_share = rowMeans(R == 1),                       # % of weightings it ranks #1
    top5_share = rowMeans(R <= 5),
    rank_med  = apply(R, 1, median),
    rank_lo   = apply(R, 1, quantile, probs = 0.05),
    rank_hi   = apply(R, 1, quantile, probs = 0.95),
    label     = paste(cctrt_id, till_id, straw_id, sep = " | ")
  )

systems2 |> 
  write_csv("data/tidy_weights-space-exploration.csv")

# results -----------------------------------------------------------------

# results <- systems |>
#   select(cctrt_id, till_id, straw_id, win_share, top5_share, rank_med, rank_lo, rank_hi) |>
#   arrange(desc(win_share), rank_med)
# print(results, n = 30)
# write_csv(results, "multicriteria_weight_sweep_results.csv")

