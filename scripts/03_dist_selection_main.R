# Distribution Choice in Bitcoin Tail-Risk Forecasting
# RBFin — March 2026
# 03_dist_selection_main.R — Distribution selection (26 families, BIC ranking)

library(gamlss)
library(gamlss.dist)
library(data.table)
library(xtable)

PATH_OUTPUT <- "./outputs"
PATH_TABLES <- "./outputs/tables"

dt_tv <- fread(file.path(PATH_OUTPUT, "data_train.csv"))
dt_tv[, date := as.Date(date)]

y <- dt_tv$ret
n <- length(y)

# 2-param
dist_2param <- c("NO", "NO2")
# 3-param
dist_3param <- c("TF", "PE", "PE2", "SN1", "SN2")
# 4-param
dist_4param <- c("JSU", "JSUo", "SHASH", "SHASHo", "SHASHo2",
                 "SEP1", "SEP2", "SEP3", "SEP4",
                 "SST", "ST1", "ST2", "ST3", "ST4", "ST5",
                 "GT", "NIG", "EGB2", "NET")

all_dists <- c(dist_2param, dist_3param, dist_4param)
n_params  <- c(rep(2, length(dist_2param)),
               rep(3, length(dist_3param)),
               rep(4, length(dist_4param)))

results <- list()

for (i in seq_along(all_dists)) {
  dist_name <- all_dists[i]

  result_i <- tryCatch({
    m <- gamlss(y ~ 1, family = dist_name, data = data.frame(y = y),
                control = gamlss.control(n.cyc = 100, trace = FALSE))
    list(dist = dist_name, n_param = n_params[i],
         gdev = m$G.deviance, df = m$df.fit,
         aic = GAIC(m, k = 2), bic = GAIC(m, k = log(n)),
         converged = m$converged, error = NA)
  }, warning = function(w) {
    list(dist = dist_name, n_param = n_params[i],
         gdev = NA, df = NA, aic = NA, bic = NA,
         converged = FALSE, error = conditionMessage(w))
  }, error = function(e) {
    list(dist = dist_name, n_param = n_params[i],
         gdev = NA, df = NA, aic = NA, bic = NA,
         converged = FALSE, error = conditionMessage(e))
  })

  results[[i]] <- result_i
}

results_dt <- rbindlist(lapply(results, function(x) {
  data.table(Distribuicao = x$dist, N_params = x$n_param,
             GDev = x$gdev, df = x$df,
             AIC = x$aic, BIC = x$bic, Convergiu = x$converged)
}))

results_valid <- results_dt[!is.na(BIC) & Convergiu == TRUE]
setorder(results_valid, BIC)
results_valid[, Rank := .I]
results_valid[, delta_BIC := BIC - min(BIC)]

print(results_valid[, .(Rank, Distribuicao, N_params,
                        GDev = round(GDev, 2), AIC = round(AIC, 2),
                        BIC = round(BIC, 2), delta_BIC = round(delta_BIC, 2))])

best_dist <- results_valid$Distribuicao[1]
cat(sprintf("\nBest distribution: %s (BIC = %.2f)\n", best_dist, results_valid$BIC[1]))

# Residual plots — top 3
pdf(file.path(PATH_OUTPUT, "distribution_selection_residuals.pdf"), width = 14, height = 10)
for (d in results_valid$Distribuicao[1:min(3, nrow(results_valid))]) {
  tryCatch({
    m <- gamlss(y ~ 1, family = d, data = data.frame(y = y),
                control = gamlss.control(n.cyc = 100, trace = FALSE))
    par(mfrow = c(2, 2)); plot(m, main = paste("Residuals:", d))
  }, error = function(e) NULL)
}
dev.off()

# LaTeX table
table_tex <- results_valid[, .(Rank, Distribution = Distribuicao, Params = N_params,
                                `Glob. Dev.` = round(GDev, 2), AIC = round(AIC, 2),
                                BIC = round(BIC, 2), `$\\Delta$BIC` = round(delta_BIC, 2))]

xtab <- xtable(table_tex,
               caption = "Distribution Selection for Bitcoin Returns (null model, intercepts only)",
               label = "tab:dist_selection", digits = c(0, 0, 0, 0, 2, 2, 2, 2))
print(xtab, file = file.path(PATH_TABLES, "table_distribution_selection.tex"),
      include.rownames = FALSE, booktabs = TRUE, sanitize.text.function = identity)

fwrite(results_valid, file.path(PATH_TABLES, "distribution_selection_results.csv"))
