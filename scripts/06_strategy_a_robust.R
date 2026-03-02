# Distribution Choice in Bitcoin Tail-Risk Forecasting
# RBFin — March 2026
# 06_strategy_a_robust.R — Covariate selection via stepGAICAll.A (GT, 2019+)

library(data.table)
library(gamlss)
library(parallel)

PATH_OUTPUT  <- "./outputs"
PATH_FIGURES <- "./outputs/figures"

dt_train <- fread(file.path(PATH_OUTPUT, "data_train.csv"))
dt_train[, date := as.Date(date)]

vars_needed <- c("date", "ret", "ret_lag1", "vix_chg_lag1", "dxy_chg_lag1",
                 "vol_chg_lag1", "fng_scaled_lag1", "funding_mean_lag1",
                 "hash_growth_lag1", "hash_ribbon_lag1")

dt_model <- na.omit(dt_train[, ..vars_needed])

predictors <- c("ret_lag1", "vix_chg_lag1", "dxy_chg_lag1", "vol_chg_lag1",
                "fng_scaled_lag1", "funding_mean_lag1", "hash_growth_lag1")

scaling_params <- list()
for (var in predictors) {
  mu_v <- mean(dt_model[[var]], na.rm = TRUE)
  sd_v <- sd(dt_model[[var]], na.rm = TRUE)
  dt_model[, (paste0(var, "_z")) := (get(var) - mu_v) / sd_v]
  scaling_params[[var]] <- list(mean = mu_v, sd = sd_v)
}

saveRDS(scaling_params, file.path(PATH_OUTPUT, "scaling_params_robust.rds"))

# Null model (GT has 4 parameters)
m0 <- gamlss(ret ~ 1, sigma.formula = ~ 1, nu.formula = ~ 1, tau.formula = ~ 1,
             family = GT, data = dt_model, trace = FALSE,
             control = gamlss.control(n.cyc = 100))

scope_formula <- as.formula(paste0(
  "~ ret_lag1_z + pb(ret_lag1_z) + ",
  "  vix_chg_lag1_z + pb(vix_chg_lag1_z) + ",
  "  dxy_chg_lag1_z + pb(dxy_chg_lag1_z) + ",
  "  vol_chg_lag1_z + pb(vol_chg_lag1_z) + ",
  "  fng_scaled_lag1_z + pb(fng_scaled_lag1_z) + ",
  "  funding_mean_lag1_z + pb(funding_mean_lag1_z) + ",
  "  hash_growth_lag1_z + pb(hash_growth_lag1_z) + ",
  "  hash_ribbon_lag1"))

scope_list <- list(lower = ~ 1, upper = scope_formula)

m_robust <- stepGAICAll.A(
  object = m0, scope = scope_list,
  k = log(nrow(dt_model)), parallel = "snow", ncpus = 4, trace = TRUE)

print(summary(m_robust))
cat("mu: "); print(formula(m_robust, what = "mu"))
cat("sigma: "); print(formula(m_robust, what = "sigma"))
cat("nu: "); print(formula(m_robust, what = "nu"))
cat("tau: "); print(formula(m_robust, what = "tau"))

saveRDS(m_robust, file.path(PATH_OUTPUT, "gamlss_robust_strategyA_GT.rds"))
saveRDS(list(mu = formula(m_robust, what = "mu"),
             sigma = formula(m_robust, what = "sigma"),
             nu = formula(m_robust, what = "nu"),
             tau = formula(m_robust, what = "tau"),
             dist = "GT", sample = "robust_2019"),
        file.path(PATH_OUTPUT, "gamlss_formulas_robust.rds"))

pdf(file.path(PATH_FIGURES, "gamlss_robust_effects.pdf"), width = 12, height = 8)
tryCatch(term.plot(m_robust, pages = 1, ask = FALSE), error = function(e) NULL)
dev.off()

pdf(file.path(PATH_FIGURES, "gamlss_robust_residuals.pdf"), width = 12, height = 8)
tryCatch({ par(mfrow=c(2,2)); plot(m_robust) }, error = function(e) NULL)
dev.off()
