# Distribution Choice in Bitcoin Tail-Risk Forecasting
# RBFin — March 2026
# 07_rolling_main.R — Rolling-window forecasts (PE + benchmarks, 2015+)

library(data.table)
library(gamlss)
library(rugarch)

ALPHA       <- 0.01
WINDOW_SIZE <- 1000
REFIT_EVERY <- 25
PATH_OUTPUT <- "./outputs"

# VaR/ES for PE distribution
VaR_PE <- function(alpha, mu, sigma, nu) qPE(alpha, mu = mu, sigma = sigma, nu = nu)
ES_PE  <- function(alpha, mu, sigma, nu, n_grid = 2000) {
  mean(qPE(seq(1e-8, alpha, length.out = n_grid), mu = mu, sigma = sigma, nu = nu))
}

# VaR/ES for skewed Student-t (GARCH)
q_sstd   <- function(p, skew, shape) qdist("sstd", p, mu = 0, sigma = 1, skew = skew, shape = shape)
VaR_sstd <- function(sigma, skew, shape, alpha) sigma * q_sstd(alpha, skew, shape)
ES_sstd  <- function(sigma, skew, shape, alpha, n_grid = 2000) {
  sigma * mean(sapply(seq(1e-8, alpha, length.out = n_grid), function(p) q_sstd(p, skew, shape)))
}

# Load data
dt_full       <- fread(file.path(PATH_OUTPUT, "data_full.csv"))
dt_full[, date := as.Date(date)]
dt_trainvalid <- fread(file.path(PATH_OUTPUT, "data_trainvalid.csv"))
dt_test       <- fread(file.path(PATH_OUTPUT, "data_test.csv"))
dt_test[, date := as.Date(date)]

n_tv   <- nrow(dt_trainvalid)
n_test <- nrow(dt_test)

# GARCH specs
spec_garch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "sstd")
spec_igarch <- ugarchspec(
  variance.model = list(model = "iGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "norm")

# Output tables
forecasts <- data.table(
  date = dt_test$date, ret = dt_test$ret,
  VaR_gamlss = NA_real_, ES_gamlss = NA_real_,
  VaR_hs = NA_real_, ES_hs = NA_real_,
  VaR_igarch = NA_real_, ES_igarch = NA_real_,
  VaR_garch = NA_real_, ES_garch = NA_real_)

params_gamlss <- data.table(
  date = dt_test$date,
  mu_pred = NA_real_, mu_intercept = NA_real_, mu_coef_ret = NA_real_,
  sigma_pred = NA_real_, sigma_intercept = NA_real_, sigma_coef_ret_lin = NA_real_,
  nu_pred = NA_real_, nu_intercept = NA_real_, window_n = NA_integer_)

# Formulas (from Strategy A)
formula_mu    <- ret ~ ret_lag1_z
formula_sigma <- ~ pb(ret_lag1_z)
formula_nu    <- ~ 1

# Rolling loop
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

  # GAMLSS-PE
  dt_w_g <- na.omit(dt_window[, .(ret, ret_lag1)])
  if (nrow(dt_w_g) >= 50) {
    m_rl <- mean(dt_w_g$ret_lag1, na.rm = TRUE)
    sd_rl <- sd(dt_w_g$ret_lag1, na.rm = TRUE)
    dt_w_g[, ret_lag1_z := (ret_lag1 - m_rl) / sd_rl]

    if (should_refit) {
      tryCatch({
        last_fit_gamlss <- gamlss(
          formula = formula_mu, sigma.formula = formula_sigma, nu.formula = formula_nu,
          family = PE, data = dt_w_g, trace = FALSE,
          control = gamlss.control(n.cyc = 500))
      }, error = function(e) {})
    }

    if (!is.null(last_fit_gamlss)) {
      tryCatch({
        rl_pt <- dt_point$ret_lag1
        if (!is.na(rl_pt) && sd_rl > 0) {
          newdata_df <- data.frame(ret = 0, ret_lag1_z = (rl_pt - m_rl) / sd_rl)
          pred <- predictAll(last_fit_gamlss, newdata = newdata_df, type = "response")
          forecasts[t, VaR_gamlss := VaR_PE(ALPHA, pred$mu, pred$sigma, pred$nu)]
          forecasts[t, ES_gamlss  := ES_PE(ALPHA, pred$mu, pred$sigma, pred$nu)]

          coef_mu <- coef(last_fit_gamlss, what = "mu")
          coef_sigma <- coef(last_fit_gamlss, what = "sigma")
          coef_nu <- coef(last_fit_gamlss, what = "nu")
          params_gamlss[t, `:=`(
            mu_pred = pred$mu, mu_intercept = coef_mu["(Intercept)"],
            mu_coef_ret = coef_mu["ret_lag1_z"],
            sigma_pred = pred$sigma, sigma_intercept = coef_sigma["(Intercept)"],
            sigma_coef_ret_lin = {pb <- coef_sigma[grep("ret_lag1_z", names(coef_sigma))]; ifelse(length(pb)>0, pb[1], NA)},
            nu_pred = pred$nu, nu_intercept = coef_nu["(Intercept)"],
            window_n = nrow(dt_w_g))]
        }
      }, error = function(e) {})
    }
  }

  # Historical Simulation (250-day)
  recent <- tail(dt_window$ret[!is.na(dt_window$ret)], 250)
  if (length(recent) >= 10) {
    VaR_h <- quantile(recent, probs = ALPHA)
    forecasts[t, VaR_hs := VaR_h]
    forecasts[t, ES_hs  := mean(recent[recent <= VaR_h])]
  }

  # iGARCH(1,1)-N
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

  # GARCH(1,1)-sstd
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

fwrite(forecasts, file.path(PATH_OUTPUT, "rolling_forecasts_main.csv"))
fwrite(params_gamlss, file.path(PATH_OUTPUT, "rolling_params_main.csv"))
cat("\nForecasts saved:", file.path(PATH_OUTPUT, "rolling_forecasts_main.csv"), "\n")
