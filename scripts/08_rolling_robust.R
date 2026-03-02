# Distribution Choice in Bitcoin Tail-Risk Forecasting
# RBFin — March 2026
# 08_rolling_robust.R — Rolling-window forecasts (GT + benchmarks, 2019+)

library(data.table)
library(gamlss)
library(rugarch)

ALPHA       <- 0.01
WINDOW_SIZE <- 1000
REFIT_EVERY <- 25
PATH_OUTPUT <- "./outputs"

# VaR/ES for GT distribution
VaR_GT <- function(alpha, mu, sigma, nu, tau) qGT(alpha, mu = mu, sigma = sigma, nu = nu, tau = tau)
ES_GT  <- function(alpha, mu, sigma, nu, tau, n_grid = 2000) {
  mean(qGT(seq(1e-8, alpha, length.out = n_grid), mu = mu, sigma = sigma, nu = nu, tau = tau))
}

q_sstd   <- function(p, skew, shape) qdist("sstd", p, mu = 0, sigma = 1, skew = skew, shape = shape)
VaR_sstd <- function(sigma, skew, shape, alpha) sigma * q_sstd(alpha, skew, shape)
ES_sstd  <- function(sigma, skew, shape, alpha, n_grid = 2000) {
  sigma * mean(sapply(seq(1e-8, alpha, length.out = n_grid), function(p) q_sstd(p, skew, shape)))
}

dt_full       <- fread(file.path(PATH_OUTPUT, "data_full.csv"))
dt_full[, date := as.Date(date)]
dt_trainvalid <- fread(file.path(PATH_OUTPUT, "data_trainvalid.csv"))
dt_test       <- fread(file.path(PATH_OUTPUT, "data_test.csv"))
dt_test[, date := as.Date(date)]

n_tv   <- nrow(dt_trainvalid)
n_test <- nrow(dt_test)

spec_garch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "sstd")
spec_igarch <- ugarchspec(
  variance.model = list(model = "iGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "norm")

forecasts <- data.table(
  date = dt_test$date, ret = dt_test$ret,
  VaR_gamlss = NA_real_, ES_gamlss = NA_real_,
  VaR_hs = NA_real_, ES_hs = NA_real_,
  VaR_igarch = NA_real_, ES_igarch = NA_real_,
  VaR_garch = NA_real_, ES_garch = NA_real_)

params_gamlss <- data.table(
  date = dt_test$date,
  mu_pred = NA_real_, mu_intercept = NA_real_,
  sigma_pred = NA_real_, sigma_intercept = NA_real_,
  sigma_coef_ret_lin = NA_real_, sigma_fng_lin = NA_real_,
  nu_pred = NA_real_, nu_intercept = NA_real_,
  tau_pred = NA_real_, tau_intercept = NA_real_,
  window_n = NA_integer_)

# Formulas from Strategy A (robust)
formula_mu    <- ret ~ 1
formula_sigma <- ~ pb(fng_scaled_lag1_z) + ret_lag1_z
formula_nu    <- ~ 1
formula_tau   <- ~ 1

pb_bar <- txtProgressBar(min = 0, max = n_test, style = 3)
last_fit_gamlss <- NULL; last_fit_garch <- NULL; last_fit_igarch <- NULL
first_idx <- n_tv + 1

for (t in seq_len(n_test)) {
  current_idx  <- first_idx + t - 1
  window_start <- current_idx - WINDOW_SIZE
  window_end   <- current_idx - 1
  dt_window    <- dt_full[window_start:window_end]
  dt_point     <- dt_full[current_idx]
  should_refit <- (t == 1) || ((t - 1) %% REFIT_EVERY == 0)

  # GAMLSS-GT
  vars_g <- c("ret", "ret_lag1", "fng_scaled_lag1")
  dt_w_g <- na.omit(dt_window[, ..vars_g])

  if (nrow(dt_w_g) >= 50) {
    m_rl <- mean(dt_w_g$ret_lag1, na.rm = TRUE)
    sd_rl <- sd(dt_w_g$ret_lag1, na.rm = TRUE)
    m_fg <- mean(dt_w_g$fng_scaled_lag1, na.rm = TRUE)
    sd_fg <- sd(dt_w_g$fng_scaled_lag1, na.rm = TRUE)
    dt_w_g[, ret_lag1_z := (ret_lag1 - m_rl) / sd_rl]
    dt_w_g[, fng_scaled_lag1_z := (fng_scaled_lag1 - m_fg) / sd_fg]

    if (should_refit) {
      tryCatch({
        last_fit_gamlss <- gamlss(
          formula = formula_mu, sigma.formula = formula_sigma,
          nu.formula = formula_nu, tau.formula = formula_tau,
          family = GT, data = dt_w_g, trace = FALSE,
          control = gamlss.control(n.cyc = 500))
      }, error = function(e) {})
    }

    if (!is.null(last_fit_gamlss)) {
      tryCatch({
        rl_pt <- dt_point$ret_lag1
        fg_pt <- dt_point$fng_scaled_lag1
        if (!is.na(rl_pt) && !is.na(fg_pt) && sd_rl > 0 && sd_fg > 0) {
          newdata_df <- data.frame(ret = 0, ret_lag1_z = (rl_pt - m_rl) / sd_rl,
                                   fng_scaled_lag1_z = (fg_pt - m_fg) / sd_fg)
          pred <- predictAll(last_fit_gamlss, newdata = newdata_df, type = "response")
          forecasts[t, VaR_gamlss := VaR_GT(ALPHA, pred$mu, pred$sigma, pred$nu, pred$tau)]
          forecasts[t, ES_gamlss  := ES_GT(ALPHA, pred$mu, pred$sigma, pred$nu, pred$tau)]

          coef_mu <- coef(last_fit_gamlss, what = "mu")
          coef_sigma <- coef(last_fit_gamlss, what = "sigma")
          coef_nu <- coef(last_fit_gamlss, what = "nu")
          coef_tau <- coef(last_fit_gamlss, what = "tau")
          params_gamlss[t, `:=`(
            mu_pred = pred$mu, mu_intercept = coef_mu["(Intercept)"],
            sigma_pred = pred$sigma, sigma_intercept = coef_sigma["(Intercept)"],
            sigma_coef_ret_lin = {pb <- coef_sigma[grep("ret_lag1_z", names(coef_sigma))]; ifelse(length(pb)>0, pb[1], NA)},
            sigma_fng_lin = {pb <- coef_sigma[grep("fng_scaled_lag1_z", names(coef_sigma))]; ifelse(length(pb)>0, pb[1], NA)},
            nu_pred = pred$nu, nu_intercept = coef_nu["(Intercept)"],
            tau_pred = pred$tau, tau_intercept = coef_tau["(Intercept)"],
            window_n = nrow(dt_w_g))]
        }
      }, error = function(e) {})
    }
  }

  # Historical Simulation
  recent <- tail(dt_window$ret[!is.na(dt_window$ret)], 250)
  if (length(recent) >= 10) {
    VaR_h <- quantile(recent, probs = ALPHA)
    forecasts[t, VaR_hs := VaR_h]
    forecasts[t, ES_hs  := mean(recent[recent <= VaR_h])]
  }

  # iGARCH
  if (should_refit)
    tryCatch({ last_fit_igarch <- ugarchfit(spec_igarch, data = dt_window$ret, solver = "hybrid") }, error = function(e) {})
  if (!is.null(last_fit_igarch)) {
    tryCatch({
      fc <- ugarchforecast(last_fit_igarch, n.ahead = 1)
      s <- as.numeric(sigma(fc)); z <- qnorm(ALPHA)
      forecasts[t, VaR_igarch := s * z]
      forecasts[t, ES_igarch  := -s * dnorm(z) / ALPHA]
    }, error = function(e) {})
  }

  # GARCH-sstd
  if (should_refit)
    tryCatch({ last_fit_garch <- ugarchfit(spec_garch, data = dt_window$ret, solver = "hybrid") }, error = function(e) {})
  if (!is.null(last_fit_garch)) {
    tryCatch({
      fc <- ugarchforecast(last_fit_garch, n.ahead = 1)
      s <- as.numeric(sigma(fc))
      skew <- as.numeric(coef(last_fit_garch)["skew"])
      shape <- as.numeric(coef(last_fit_garch)["shape"])
      forecasts[t, VaR_garch := VaR_sstd(s, skew, shape, ALPHA)]
      forecasts[t, ES_garch  := ES_sstd(s, skew, shape, ALPHA)]
    }, error = function(e) {})
  }

  setTxtProgressBar(pb_bar, t)
}
close(pb_bar)

fwrite(forecasts, file.path(PATH_OUTPUT, "rolling_forecasts_robust.csv"))
fwrite(params_gamlss, file.path(PATH_OUTPUT, "rolling_params_robust.csv"))
cat("\nForecasts saved:", file.path(PATH_OUTPUT, "rolling_forecasts_robust.csv"), "\n")
