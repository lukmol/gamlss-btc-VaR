# Distribution Choice in Bitcoin Tail-Risk Forecasting
# RBFin — March 2026
# 00_setup.R — Packages, constants, helper functions

library(data.table)
library(gamlss)
library(gamlss.dist)
library(rugarch)
library(forecast)
library(sandwich)
library(lmtest)
library(ggplot2)
library(ggtext)
library(patchwork)
library(scales)
library(xtable)

options(scipen = 999)
set.seed(42)

# Sample periods
TRAIN_START  <- as.Date("2015-01-01")
TRAIN_END    <- as.Date("2022-12-31")
VALID_END    <- as.Date("2023-12-31")
TEST_END     <- as.Date("2025-12-29")
ROBUST_START <- as.Date("2019-09-11")

# Rolling window
WINDOW_SIZE <- 1000
REFIT_EVERY <- 25
ALPHA       <- 0.01

# Paths
PATH_DATA    <- "./data/model_data_full.csv"
PATH_OUTPUT  <- "./outputs"
PATH_TABLES  <- file.path(PATH_OUTPUT, "tables")
PATH_FIGURES <- file.path(PATH_OUTPUT, "figures")

dir.create(PATH_OUTPUT,  showWarnings = FALSE, recursive = TRUE)
dir.create(PATH_TABLES,  showWarnings = FALSE, recursive = TRUE)
dir.create(PATH_FIGURES, showWarnings = FALSE, recursive = TRUE)

# FZ0 loss (Fissler-Ziegel, linear form)
fz0_loss_obs <- function(y, v, e, alpha) {
  ind <- as.numeric(y <= v)
  (v - y) * (ind - alpha) / alpha - e * ind + y * ind
}

# Kupiec UC test
kupiec_uc <- function(ret, VaR, alpha) {
  n <- length(ret)
  x <- sum(ret < VaR, na.rm = TRUE)
  p <- x / n
  if (p == 0 || p == 1) return(list(stat = NA, pval = NA))
  LR <- -2 * (x*log(alpha) + (n-x)*log(1-alpha) -
              x*log(p)     - (n-x)*log(1-p))
  list(stat = LR, pval = 1 - pchisq(LR, df = 1))
}

# Christoffersen CC test
christoffersen_cc <- function(ret, VaR, alpha) {
  viol <- as.integer(ret < VaR)
  n <- length(viol); x <- sum(viol)
  p <- x / n
  if (p == 0 || p == 1) return(list(stat = NA, pval = NA))
  LR_uc <- -2 * (x*log(alpha) + (n-x)*log(1-alpha) -
                 x*log(p)     - (n-x)*log(1-p))
  n00 <- sum(viol[-n]==0 & viol[-1]==0)
  n01 <- sum(viol[-n]==0 & viol[-1]==1)
  n10 <- sum(viol[-n]==1 & viol[-1]==0)
  n11 <- sum(viol[-n]==1 & viol[-1]==1)
  p01 <- max(min(n01/(n00+n01), 1-1e-10), 1e-10)
  p11 <- max(min(n11/(n10+n11), 1-1e-10), 1e-10)
  p_m <- max(min((n01+n11)/(n00+n01+n10+n11), 1-1e-10), 1e-10)
  LR_ind <- -2*(n00*log(1-p_m)+n01*log(p_m)+n10*log(1-p_m)+n11*log(p_m)-
                n00*log(1-p01)-n01*log(p01)-n10*log(1-p11)-n11*log(p11))
  list(stat = LR_uc + LR_ind, pval = 1 - pchisq(LR_uc + LR_ind, df = 2))
}

# Normal ES
compute_ES_normal <- function(sigma, alpha = 0.01) {
  -sigma * dnorm(qnorm(alpha)) / alpha
}
