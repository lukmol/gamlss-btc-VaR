# Distribution Choice in Bitcoin Tail-Risk Forecasting
# RBFin — March 2026
# 10_dm_test.R — Diebold-Mariano tests (FZ0 loss, Newey-West HAC + HLN correction)

library(data.table)
library(sandwich)
library(lmtest)

PATH_OUTPUT <- "./outputs"
ALPHA <- 0.01

# FZ0 loss per observation (same linear form as 09_backtest.R)
fz0_loss_obs <- function(y, v, e, alpha) {
  ind <- as.numeric(y <= v)
  (v - y) * (ind - alpha) / alpha - e * ind + y * ind
}

# Manual DM test with Newey-West HAC + Harvey-Leybourne-Newbold correction
dm_test_manual <- function(loss1, loss2, h = 1) {
  d <- loss1 - loss2; n <- length(d); d_bar <- mean(d)
  reg <- lm(d ~ 1)
  nw <- NeweyWest(reg, lag = h - 1, prewhite = FALSE)
  se <- sqrt(nw[1, 1])
  hln <- sqrt((n + 1 - 2*h + h*(h-1)/n) / n)
  dm_stat <- (d_bar / se) * hln
  list(d_bar = d_bar, se = se, dm_stat = dm_stat,
       p_less = pt(dm_stat, df = n-1),
       p_two = 2 * pt(-abs(dm_stat), df = n-1), n = n)
}

run_dm_pairwise <- function(fc, gamlss_var, gamlss_es, gamlss_label, bench_specs, alpha = 0.01) {
  loss_gamlss <- fz0_loss_obs(fc$ret, fc[[gamlss_var]], fc[[gamlss_es]], alpha)
  results <- list()
  for (b in bench_specs) {
    loss_bench <- fz0_loss_obs(fc$ret, fc[[b$var_col]], fc[[b$es_col]], alpha)
    valid <- !is.na(loss_gamlss) & !is.na(loss_bench)
    dm <- dm_test_manual(loss_gamlss[valid], loss_bench[valid], h = 1)
    results[[b$name]] <- data.table(
      comparison = paste0(gamlss_label, " vs ", b$name),
      mean_loss_gamlss = mean(loss_gamlss[valid]),
      mean_loss_bench = mean(loss_bench[valid]),
      loss_diff = dm$d_bar, dm_statistic = dm$dm_stat,
      dm_pvalue_less = dm$p_less, dm_pvalue_two = dm$p_two, n_obs = dm$n)
  }
  rbindlist(results)
}

benchmarks <- list(
  list(name = "GARCH(1,1)-sstd", var_col = "VaR_garch", es_col = "ES_garch"),
  list(name = "Hist. Simulation", var_col = "VaR_hs", es_col = "ES_hs"),
  list(name = "iGARCH(1,1)-N", var_col = "VaR_igarch", es_col = "ES_igarch"))

# Panel A: Main (PE)
fc_main <- fread(file.path(PATH_OUTPUT, "rolling_forecasts_main.csv"))
fc_main[, date := as.Date(date)]
dm_main <- run_dm_pairwise(fc_main, "VaR_gamlss", "ES_gamlss", "GAMLSS-PE", benchmarks, ALPHA)
print(dm_main)

# Panel B: Robust (GT)
fc_rob <- fread(file.path(PATH_OUTPUT, "rolling_forecasts_robust.csv"))
fc_rob[, date := as.Date(date)]
dm_rob <- run_dm_pairwise(fc_rob, "VaR_gamlss", "ES_gamlss", "GAMLSS-GT", benchmarks, ALPHA)
print(dm_rob)

# Save
dm_main[, panel := "Main (PE)"]
dm_rob[, panel := "Robust (GT)"]
dm_all <- rbind(dm_main, dm_rob)

dir.create(file.path(PATH_OUTPUT, "tables"), showWarnings = FALSE, recursive = TRUE)
fwrite(dm_all, file.path(PATH_OUTPUT, "tables", "dm_test_results.csv"))

# LaTeX table
format_row <- function(r, gamlss_label) {
  bench_name <- sub(paste0(gamlss_label, " vs "), "", r$comparison)
  stars <- ifelse(r$dm_pvalue_two < 0.01, "***",
           ifelse(r$dm_pvalue_two < 0.05, "**",
           ifelse(r$dm_pvalue_two < 0.10, "*", "")))
  sprintf("%s vs %s & %.4f & %.4f & %.4f & %.3f & %.3f%s \\\\",
          gamlss_label, bench_name,
          r$mean_loss_gamlss, r$mean_loss_bench, r$loss_diff,
          r$dm_statistic, r$dm_pvalue_two, stars)
}

latex_lines <- c(
  "\\begin{table}[htbp]", "\\centering",
  "\\caption{Diebold--Mariano Tests for Equal Predictive Accuracy (FZ$_0$ Loss)}",
  "\\label{tab:dm_test}", "\\small",
  "\\begin{tabular}{l c c c c c}", "\\toprule",
  "\\textbf{Comparison} & $\\overline{L}_{\\text{GAMLSS}}$ & $\\overline{L}_{\\text{Bench}}$ & $\\overline{d}$ & \\textbf{DM} & $p$\\textbf{-value} \\\\",
  "\\midrule",
  "\\multicolumn{6}{l}{\\textit{Panel A: Main Analysis (2015+, PE distribution)}} \\\\",
  "\\midrule")

for (i in seq_len(nrow(dm_main))) latex_lines <- c(latex_lines, format_row(dm_main[i], "GAMLSS-PE"))

latex_lines <- c(latex_lines, "\\midrule",
  "\\multicolumn{6}{l}{\\textit{Panel B: Robustness Analysis (2019+, GT distribution)}} \\\\",
  "\\midrule")

for (i in seq_len(nrow(dm_rob))) latex_lines <- c(latex_lines, format_row(dm_rob[i], "GAMLSS-GT"))

latex_lines <- c(latex_lines, "\\bottomrule", "\\end{tabular}",
  "\\begin{tablenotes}", "\\small",
  "\\item Notes: $H_0$: equal predictive accuracy (two-sided). $\\overline{d} = \\overline{L}_{\\text{GAMLSS}} - \\overline{L}_{\\text{Bench}}$; negative values indicate GAMLSS superiority. Newey--West HAC + HLN correction. */**/*** = 10\\%/5\\%/1\\%.",
  "\\end{tablenotes}", "\\end{table}")

writeLines(latex_lines, file.path(PATH_OUTPUT, "tables", "dm_test.tex"))
