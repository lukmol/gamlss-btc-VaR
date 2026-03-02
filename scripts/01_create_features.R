# Distribution Choice in Bitcoin Tail-Risk Forecasting
# RBFin — March 2026
# 01_create_features.R — Log-returns, growth rates, lags

library(data.table)

data_clean <- fread("./data/data_merged_clean.csv")
data_clean[, date := as.Date(date)]
setorder(data_clean, date)

# Returns and changes
data_clean[, `:=`(
  ret     = c(NA, diff(log(price))),
  vix_chg = c(NA, diff(log(vix))),
  dxy_chg = c(NA, diff(log(dxy))),
  vol_chg = c(NA, diff(log(volume)))
)]

# On-chain growth rates
if ("hashrate" %in% names(data_clean))
  data_clean[, hash_growth := c(NA, diff(log(hashrate + 1)))]
if ("active_addresses" %in% names(data_clean))
  data_clean[, addr_growth := c(NA, diff(log(active_addresses + 1)))]
if ("tx_count" %in% names(data_clean))
  data_clean[, tx_growth := c(NA, diff(log(tx_count + 1)))]
if ("fees_btc" %in% names(data_clean))
  data_clean[, fees_growth := c(NA, diff(log(fees_btc + 1)))]

# Derived features
if ("fng" %in% names(data_clean))
  data_clean[, fng_scaled := (fng - 50) / 50]

if ("hashrate" %in% names(data_clean)) {
  data_clean[, `:=`(
    hash_ma30 = frollmean(hashrate, 30, align = "right"),
    hash_ma60 = frollmean(hashrate, 60, align = "right")
  )]
  data_clean[, hash_ribbon := as.numeric(hash_ma30 < hash_ma60)]
}

# Lags (t-1)
vars_to_lag <- c("ret", "vix_chg", "dxy_chg", "vol_chg",
                 "hash_growth", "addr_growth", "tx_growth", "fees_growth",
                 "fng_scaled", "funding_mean", "funding_abs", "hash_ribbon")

for (var in vars_to_lag) {
  if (var %in% names(data_clean))
    data_clean[, (paste0(var, "_lag1")) := shift(get(var), 1)]
}

# Remove rows without price or return
data_final <- data_clean[!is.na(price) & !is.na(ret)]

fwrite(data_final, "./data/model_data_full.csv")
cat("Features created:", nrow(data_final), "rows |",
    as.character(min(data_final$date)), "to", as.character(max(data_final$date)), "\n")
