# Distribution Choice in Bitcoin Tail-Risk Forecasting
# RBFin — March 2026
# 11_generate_figures.R — Backtest panels, parameter evolution, VaR comparison

library(data.table)
library(gamlss)

PATH_OUTPUT  <- "./outputs"
PATH_FIGURES <- "./outputs/figures"
dir.create(PATH_FIGURES, showWarnings = FALSE, recursive = TRUE)
ALPHA <- 0.01

fc_main <- fread(file.path(PATH_OUTPUT, "rolling_forecasts_main.csv"))
fc_main[, date := as.Date(date)]
fc_rob <- fread(file.path(PATH_OUTPUT, "rolling_forecasts_robust.csv"))
fc_rob[, date := as.Date(date)]

# Backtest panel — Main (PE)
pdf(file.path(PATH_FIGURES, "backtest_panel_main.pdf"), width = 10, height = 12)
par(mfrow = c(4, 1), mar = c(3, 4, 2.5, 1), oma = c(1, 0, 2, 0))
models_main <- list(
  list(name = "GAMLSS-PE", var_col = "VaR_gamlss", es_col = "ES_gamlss"),
  list(name = "GARCH(1,1)-sstd", var_col = "VaR_garch", es_col = "ES_garch"),
  list(name = "Historical Simulation", var_col = "VaR_hs", es_col = "ES_hs"),
  list(name = "iGARCH(1,1)-N", var_col = "VaR_igarch", es_col = "ES_igarch"))
for (m in models_main) {
  VaR_vals <- fc_main[[m$var_col]]; ES_vals <- fc_main[[m$es_col]]
  viol <- fc_main$ret < VaR_vals & !is.na(VaR_vals)
  ylim <- range(c(fc_main$ret, VaR_vals, ES_vals), na.rm = TRUE)
  plot(fc_main$date, fc_main$ret, type = "h", col = "grey70", ylim = ylim,
       xlab = "", ylab = "Return", main = sprintf("%s (violations: %d)", m$name, sum(viol, na.rm=T)))
  lines(fc_main$date, VaR_vals, col = "red", lwd = 1.5)
  lines(fc_main$date, ES_vals, col = "blue", lwd = 1, lty = 2)
  points(fc_main$date[viol], fc_main$ret[viol], pch = 16, col = "red", cex = 1.2)
  legend("bottomleft", c("Return", "1% VaR", "1% ES", "Violation"),
         col = c("grey70", "red", "blue", "red"), lty = c(1,1,2,NA), pch = c(NA,NA,NA,16), cex = 0.7, bg = "white")
}
mtext("Backtesting — Main Analysis (2015+, PE)", outer = TRUE, cex = 1.1, font = 2)
dev.off()

# Backtest panel — Robust (GT)
pdf(file.path(PATH_FIGURES, "backtest_panel_robust.pdf"), width = 10, height = 12)
par(mfrow = c(4, 1), mar = c(3, 4, 2.5, 1), oma = c(1, 0, 2, 0))
models_rob <- list(
  list(name = "GAMLSS-GT", var_col = "VaR_gamlss", es_col = "ES_gamlss"),
  list(name = "GARCH(1,1)-sstd", var_col = "VaR_garch", es_col = "ES_garch"),
  list(name = "Historical Simulation", var_col = "VaR_hs", es_col = "ES_hs"),
  list(name = "iGARCH(1,1)-N", var_col = "VaR_igarch", es_col = "ES_igarch"))
for (m in models_rob) {
  VaR_vals <- fc_rob[[m$var_col]]; ES_vals <- fc_rob[[m$es_col]]
  viol <- fc_rob$ret < VaR_vals & !is.na(VaR_vals)
  ylim <- range(c(fc_rob$ret, VaR_vals, ES_vals), na.rm = TRUE)
  plot(fc_rob$date, fc_rob$ret, type = "h", col = "grey70", ylim = ylim,
       xlab = "", ylab = "Return", main = sprintf("%s (violations: %d)", m$name, sum(viol, na.rm=T)))
  lines(fc_rob$date, VaR_vals, col = "red", lwd = 1.5)
  lines(fc_rob$date, ES_vals, col = "blue", lwd = 1, lty = 2)
  points(fc_rob$date[viol], fc_rob$ret[viol], pch = 16, col = "red", cex = 1.2)
  legend("bottomleft", c("Return", "1% VaR", "1% ES", "Violation"),
         col = c("grey70", "red", "blue", "red"), lty = c(1,1,2,NA), pch = c(NA,NA,NA,16), cex = 0.7, bg = "white")
}
mtext("Backtesting — Robustness (2019+, GT)", outer = TRUE, cex = 1.1, font = 2)
dev.off()

# Parameter evolution — Main (PE)
params_main <- fread(file.path(PATH_OUTPUT, "rolling_params_main.csv"))
params_main[, date := as.Date(date)]

pdf(file.path(PATH_FIGURES, "params_evolution_main.pdf"), width = 10, height = 8)
par(mfrow = c(3, 1), mar = c(3, 4, 2.5, 1), oma = c(1, 0, 2, 0))
plot(params_main$date, params_main$mu_pred, type = "l", col = "darkblue", lwd = 1.5,
     xlab = "", ylab = expression(hat(mu)[t]), main = expression("Location " * hat(mu)[t]))
abline(h = 0, lty = 3, col = "grey50")
plot(params_main$date, params_main$sigma_pred, type = "l", col = "darkred", lwd = 1.5,
     xlab = "", ylab = expression(hat(sigma)[t]), main = expression("Scale " * hat(sigma)[t]))
plot(params_main$date, params_main$nu_pred, type = "l", col = "darkgreen", lwd = 1.5,
     xlab = "", ylab = expression(hat(nu)[t]), main = expression("Shape " * hat(nu)[t]))
abline(h = 2, lty = 3, col = "grey50")
mtext("GAMLSS-PE Parameters — Test Period", outer = TRUE, cex = 1.1, font = 2)
dev.off()

# Parameter evolution — Robust (GT)
params_rob <- fread(file.path(PATH_OUTPUT, "rolling_params_robust.csv"))
params_rob[, date := as.Date(date)]

pdf(file.path(PATH_FIGURES, "params_evolution_robust.pdf"), width = 10, height = 10)
par(mfrow = c(4, 1), mar = c(3, 4, 2.5, 1), oma = c(1, 0, 2, 0))
plot(params_rob$date, params_rob$mu_pred, type = "l", col = "darkblue", lwd = 1.5,
     xlab = "", ylab = expression(hat(mu)[t]), main = expression("Location " * hat(mu)[t]))
abline(h = 0, lty = 3, col = "grey50")
plot(params_rob$date, params_rob$sigma_pred, type = "l", col = "darkred", lwd = 1.5,
     xlab = "", ylab = expression(hat(sigma)[t]), main = expression("Scale " * hat(sigma)[t]))
plot(params_rob$date, params_rob$nu_pred, type = "l", col = "darkgreen", lwd = 1.5,
     xlab = "", ylab = expression(hat(nu)[t]), main = expression("Predicted " * hat(nu)[t]))
plot(params_rob$date, params_rob$tau_pred, type = "l", col = "purple", lwd = 1.5,
     xlab = "", ylab = expression(hat(tau)[t]), main = expression("Predicted " * hat(tau)[t]))
abline(h = 1, lty = 3, col = "grey50")
mtext("GAMLSS-GT Parameters — Test Period", outer = TRUE, cex = 1.1, font = 2)
dev.off()

# VaR comparison overlay
pdf(file.path(PATH_FIGURES, "var_comparison_overlay.pdf"), width = 10, height = 5)
par(mar = c(4, 4, 2, 1))
ylim <- range(c(fc_main$VaR_gamlss, fc_main$VaR_garch, fc_main$VaR_hs, fc_main$VaR_igarch), na.rm = TRUE)
plot(fc_main$date, fc_main$VaR_gamlss, type = "l", col = "red", lwd = 1.8,
     ylim = ylim, xlab = "", ylab = "1% VaR", main = "VaR Comparison — All Models")
lines(fc_main$date, fc_main$VaR_garch, col = "blue", lwd = 1.2, lty = 2)
lines(fc_main$date, fc_main$VaR_hs, col = "green4", lwd = 1, lty = 3)
lines(fc_main$date, fc_main$VaR_igarch, col = "orange", lwd = 1, lty = 4)
legend("bottomleft", c("GAMLSS-PE", "GARCH-sstd", "Hist. Sim.", "iGARCH-N"),
       col = c("red", "blue", "green4", "orange"), lty = 1:4, cex = 0.8, bg = "white")
dev.off()

# P-spline smooth effect
fit_pe <- readRDS(file.path(PATH_OUTPUT, "gamlss_main_strategyA_PE.rds"))
pdf(file.path(PATH_FIGURES, "pb_smooth_effect_sigma.pdf"), width = 8, height = 5)
par(mar = c(4, 4, 2, 1))
term.plot(fit_pe, what = "sigma", pages = 1, ask = FALSE)
dev.off()
