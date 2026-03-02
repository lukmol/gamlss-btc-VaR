# Distribution Choice in Bitcoin Tail-Risk Forecasting
# RBFin — March 2026
# 04_dist_selection_robust.R — Distribution selection, robustness sample (2019+)

library(gamlss)
library(gamlss.dist)
library(data.table)
library(xtable)

PATH_OUTPUT <- "./outputs"
PATH_TABLES <- "./outputs/tables"

dt_train <- fread(file.path(PATH_OUTPUT, "data_train.csv"))
dt_train[, date := as.Date(date)]

vars_all <- c("ret", "ret_lag1", "vix_chg_lag1", "dxy_chg_lag1",
              "vol_chg_lag1", "fng_scaled_lag1", "funding_mean_lag1",
              "hash_growth_lag1", "hash_ribbon_lag1")

dt_complete <- na.omit(dt_train[, ..vars_all])
y <- dt_complete$ret
n <- length(y)

all_dists <- c("NO", "NO2",
               "TF", "PE", "PE2", "SN1", "SN2",
               "JSU", "JSUo", "SHASH", "SHASHo", "SHASHo2",
               "SEP1", "SEP2", "SEP3", "SEP4",
               "SST", "ST1", "ST2", "ST3", "ST4", "ST5",
               "GT", "NIG", "EGB2", "NET")
n_params <- c(rep(2,2), rep(3,5), rep(4,19))

results <- list()

for (i in seq_along(all_dists)) {
  d <- all_dists[i]
  res <- tryCatch({
    m <- gamlss(y ~ 1, family = d, data = data.frame(y = y),
                control = gamlss.control(n.cyc = 100, trace = FALSE))
    list(dist = d, n_param = n_params[i],
         gdev = m$G.deviance, df = m$df.fit,
         aic = GAIC(m, k = 2), bic = GAIC(m, k = log(n)),
         conv = m$converged, err = NA)
  }, warning = function(w) {
    list(dist=d, n_param=n_params[i], gdev=NA, df=NA, aic=NA, bic=NA, conv=FALSE, err=conditionMessage(w))
  }, error = function(e) {
    list(dist=d, n_param=n_params[i], gdev=NA, df=NA, aic=NA, bic=NA, conv=FALSE, err=conditionMessage(e))
  })
  results[[i]] <- res
}

dt_res <- rbindlist(lapply(results, function(x)
  data.table(Dist=x$dist, Params=x$n_param, GDev=x$gdev,
             AIC=x$aic, BIC=x$bic, Conv=x$conv)))

dt_ok <- dt_res[!is.na(BIC) & Conv == TRUE]
setorder(dt_ok, BIC)
dt_ok[, Rank := .I]
dt_ok[, delta_BIC := BIC - min(BIC)]

print(dt_ok[, .(Rank, Dist, Params, GDev = round(GDev, 2),
                AIC = round(AIC, 2), BIC = round(BIC, 2),
                dBIC = round(delta_BIC, 2))])

best <- dt_ok$Dist[1]
cat(sprintf("\nBest distribution (robust): %s (BIC = %.2f)\n", best, dt_ok$BIC[1]))

# Residuals — top 3
pdf(file.path(PATH_OUTPUT, "dist_selection_robust_residuals.pdf"), width = 14, height = 10)
for (d in dt_ok$Dist[1:min(3, nrow(dt_ok))]) {
  tryCatch({
    m <- gamlss(y ~ 1, family = d, data = data.frame(y = y),
                control = gamlss.control(n.cyc = 100, trace = FALSE))
    par(mfrow = c(2,2)); plot(m, main = paste("Residuals:", d))
  }, error = function(e) NULL)
}
dev.off()

fwrite(dt_ok, file.path(PATH_TABLES, "dist_selection_robust.csv"))

xtab <- xtable(dt_ok[, .(Rank, Distribution=Dist, Params,
                          `Glob.Dev.`=round(GDev,2), AIC=round(AIC,2),
                          BIC=round(BIC,2), `$\\Delta$BIC`=round(delta_BIC,2))],
               caption = "Distribution Selection — Robustness Sample (2019+)",
               label = "tab:dist_robust")
print(xtab, file = file.path(PATH_TABLES, "table_dist_selection_robust.tex"),
      include.rownames = FALSE, booktabs = TRUE, sanitize.text.function = identity)

saveRDS(list(best_dist = best, results = dt_ok),
        file.path(PATH_OUTPUT, "dist_selection_robust.rds"))
