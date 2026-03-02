# Distribution Choice in Bitcoin Tail-Risk Forecasting
# RBFin — March 2026
# 09_backtest.R — UC, CC, FZ0 backtesting (main PE + robust GT)

library(data.table)
library(xtable)

ALPHA       <- 0.01
PATH_OUTPUT <- "./outputs"
PATH_TABLES <- "./outputs/tables"

kupiec_uc <- function(ret, VaR, alpha) {
  n <- length(ret); x <- sum(ret < VaR, na.rm = TRUE); p <- x / n
  if (p == 0 || p == 1) return(list(stat = NA, pval = NA))
  LR <- -2 * (x*log(alpha) + (n-x)*log(1-alpha) - x*log(p) - (n-x)*log(1-p))
  list(stat = LR, pval = 1 - pchisq(LR, df = 1))
}

christoffersen_cc <- function(ret, VaR, alpha) {
  viol <- as.integer(ret < VaR); n <- length(viol); x <- sum(viol); p <- x / n
  if (p == 0 || p == 1) return(list(stat = NA, pval = NA))
  LR_uc <- -2 * (x*log(alpha) + (n-x)*log(1-alpha) - x*log(p) - (n-x)*log(1-p))
  n00 <- sum(viol[-n]==0 & viol[-1]==0); n01 <- sum(viol[-n]==0 & viol[-1]==1)
  n10 <- sum(viol[-n]==1 & viol[-1]==0); n11 <- sum(viol[-n]==1 & viol[-1]==1)
  p01 <- max(min(n01/(n00+n01), 1-1e-10), 1e-10)
  p11 <- max(min(n11/(n10+n11), 1-1e-10), 1e-10)
  p_m <- max(min((n01+n11)/(n00+n01+n10+n11), 1-1e-10), 1e-10)
  LR_ind <- -2*(n00*log(1-p_m)+n01*log(p_m)+n10*log(1-p_m)+n11*log(p_m)-
                n00*log(1-p01)-n01*log(p01)-n10*log(1-p11)-n11*log(p11))
  list(stat = LR_uc + LR_ind, pval = 1 - pchisq(LR_uc + LR_ind, df = 2))
}

fz0_loss <- function(ret, VaR, ES, alpha) {
  ind <- as.numeric(ret < VaR)
  mean((VaR - ret) * (ind - alpha) / alpha - ES * ind + ret * ind, na.rm = TRUE)
}

run_backtest <- function(name, ret, VaR, ES, alpha = ALPHA) {
  valid <- !is.na(VaR) & !is.na(ES)
  r <- ret[valid]; v <- VaR[valid]; e <- ES[valid]
  n <- length(r); x <- sum(r < v)
  uc <- kupiec_uc(r, v, alpha); cc <- christoffersen_cc(r, v, alpha); fz <- fz0_loss(r, v, e, alpha)
  list(model = name, n = n, viol = x, rate = round(100*x/n, 3),
       exp_rate = round(100*alpha, 1), uc_pval = round(uc$pval, 4),
       cc_pval = round(cc$pval, 4), fz0 = round(fz, 6))
}

# Panel A: Main (PE, 2015+)
fc <- fread(file.path(PATH_OUTPUT, "rolling_forecasts_main.csv"))
fc[, date := as.Date(date)]

results <- rbindlist(list(
  run_backtest("GAMLSS-PE (Strategy A)", fc$ret, fc$VaR_gamlss, fc$ES_gamlss),
  run_backtest("Hist. Simulation",       fc$ret, fc$VaR_hs,     fc$ES_hs),
  run_backtest("iGARCH(1,1)-N",         fc$ret, fc$VaR_igarch,  fc$ES_igarch),
  run_backtest("GARCH(1,1)-sstd",       fc$ret, fc$VaR_garch,   fc$ES_garch)))
setorder(results, fz0); results[, rank := .I]

print(results[, .(rank, model, n, viol, rate, exp_rate, uc_pval, cc_pval, fz0)])

fwrite(results, file.path(PATH_TABLES, "backtest_main.csv"))

xtab <- xtable(results[, .(Rank=rank, Model=model, N=n, Viol.=viol,
                            `Rate(%)`=rate, `Exp.(%)`=exp_rate,
                            `UC p`=uc_pval, `CC p`=cc_pval, FZ0=fz0)],
               caption = "Backtesting Results — Main Analysis (2015+), PE Distribution",
               label = "tab:backtest_main", digits = c(0,0,0,0,0,3,1,4,4,6))
print(xtab, file = file.path(PATH_TABLES, "table_backtest_main.tex"),
      include.rownames = FALSE, booktabs = TRUE)

# Panel B: Robustness (GT, 2019+)
fc_rob <- fread(file.path(PATH_OUTPUT, "rolling_forecasts_robust.csv"))
fc_rob[, date := as.Date(date)]

results_rob <- rbindlist(list(
  run_backtest("GAMLSS-GT (Robustez)", fc_rob$ret, fc_rob$VaR_gamlss, fc_rob$ES_gamlss),
  run_backtest("Hist. Simulation",      fc_rob$ret, fc_rob$VaR_hs,     fc_rob$ES_hs),
  run_backtest("iGARCH(1,1)-N",        fc_rob$ret, fc_rob$VaR_igarch,  fc_rob$ES_igarch),
  run_backtest("GARCH(1,1)-sstd",      fc_rob$ret, fc_rob$VaR_garch,   fc_rob$ES_garch)))
setorder(results_rob, fz0); results_rob[, rank := .I]

print(results_rob[, .(rank, model, n, viol, rate, exp_rate, uc_pval, cc_pval, fz0)])

fwrite(results_rob, file.path(PATH_TABLES, "backtest_robust.csv"))

xtab_rob <- xtable(results_rob[, .(Rank=rank, Model=model, N=n, Viol.=viol,
                                    `Rate(%)`=rate, `Exp.(%)`=exp_rate,
                                    `UC p`=uc_pval, `CC p`=cc_pval, FZ0=fz0)],
                    caption = "Backtesting Results — Robustness (2019+), GT Distribution",
                    label = "tab:backtest_robust", digits = c(0,0,0,0,0,3,1,4,4,6))
print(xtab_rob, file = file.path(PATH_TABLES, "table_backtest_robust.tex"),
      include.rownames = FALSE, booktabs = TRUE)
