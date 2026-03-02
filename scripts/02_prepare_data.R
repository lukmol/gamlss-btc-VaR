# Distribution Choice in Bitcoin Tail-Risk Forecasting
# RBFin — March 2026
# 02_prepare_data.R — Train/test split, standardization

library(data.table)
library(moments)

source("scripts/00_setup.R")

dt <- fread(PATH_DATA)
dt[, date := as.Date(date)]
setorder(dt, date)

stopifnot(all(diff(as.numeric(dt$date)) >= 0))

# Split
dt[, period := fcase(
  date <= TRAIN_END, "train",
  date <= VALID_END, "valid",
  date <= TEST_END,  "test"
)]

dt_train      <- dt[period == "train"]
dt_valid      <- dt[period == "valid"]
dt_test       <- dt[period == "test"]
dt_trainvalid <- dt[period %in% c("train", "valid")]

# Standardize predictors (training-set statistics)
predictors <- c("ret_lag1", "vix_chg_lag1", "dxy_chg_lag1", "vol_chg_lag1",
                "fng_scaled_lag1", "funding_mean_lag1", "hash_growth_lag1",
                "addr_growth_lag1", "fees_growth_lag1")

stats <- dt_train[, lapply(.SD, function(x) list(mean = mean(x, na.rm = TRUE),
                                                  sd = sd(x, na.rm = TRUE))),
                  .SDcols = predictors]

for (var in predictors) {
  var_z    <- paste0(var, "_z")
  mean_val <- stats[[var]]$mean
  sd_val   <- stats[[var]]$sd
  dt[, (var_z) := (get(var) - mean_val) / sd_val]
}

cat("Train:", nrow(dt_train), "| Valid:", nrow(dt_valid),
    "| Test:", nrow(dt_test), "\n")

fwrite(dt_train, file.path(PATH_OUTPUT, "data_train.csv"))
fwrite(dt_valid, file.path(PATH_OUTPUT, "data_valid.csv"))
fwrite(dt_test,  file.path(PATH_OUTPUT, "data_test.csv"))
fwrite(dt_trainvalid, file.path(PATH_OUTPUT, "data_trainvalid.csv"))
fwrite(dt, file.path(PATH_OUTPUT, "data_full.csv"))
