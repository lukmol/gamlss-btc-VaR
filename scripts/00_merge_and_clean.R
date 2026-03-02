# Distribution Choice in Bitcoin Tail-Risk Forecasting
# RBFin — March 2026
# 00_merge_and_clean.R — Merge all data sources, LOCF imputation

library(data.table)
library(zoo)

btc_data     <- fread("./data/btc_price_volume.csv")
vix_data     <- fread("./data/vix.csv")
dxy_data     <- fread("./data/dxy.csv")
fng_data     <- fread("./data/fear_greed.csv")
hash_data    <- fread("./data/hashrate.csv")
diff_data    <- fread("./data/difficulty.csv")
cm_data      <- fread("./data/coinmetrics.csv")
funding_data <- fread("./data/funding_rates.csv")

for (dt in list(btc_data, vix_data, dxy_data, fng_data, hash_data,
                diff_data, cm_data, funding_data)) {
  if (nrow(dt) > 0) dt[, date := as.Date(date)]
}

# Merge all sources onto Bitcoin base
data_merged <- copy(btc_data)
setorder(data_merged, date)

for (src in list(vix_data, dxy_data, fng_data, hash_data, diff_data, cm_data, funding_data)) {
  if (nrow(src) > 0) {
    data_merged <- merge(data_merged, src, by = "date", all.x = TRUE)
    setorder(data_merged, date)
  }
}

# Forward-fill (LOCF) — no look-ahead bias
market_vars    <- c("vix", "dxy")
onchain_vars   <- c("hashrate", "difficulty", "active_addresses", "tx_count", "fees_btc")
sentiment_vars <- c("fng", "funding_mean", "funding_abs")

for (var in c(market_vars, onchain_vars, sentiment_vars)) {
  if (var %in% names(data_merged))
    data_merged[, (var) := nafill(get(var), type = "locf")]
}

if (sum(is.na(data_merged$price)) > 0)
  data_merged[, price := nafill(price, type = "locf")]
if (sum(is.na(data_merged$volume)) > 0)
  data_merged[, volume := nafill(volume, type = "locf")]

# Verify chronological order
setorder(data_merged, date)
stopifnot(all(diff(as.numeric(data_merged$date)) >= 0))

fwrite(data_merged, "./data/data_merged_clean.csv")
cat("Merged data:", nrow(data_merged), "rows |",
    as.character(min(data_merged$date)), "to", as.character(max(data_merged$date)), "\n")
