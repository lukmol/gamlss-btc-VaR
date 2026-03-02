# Replication: Distribution Choice in Bitcoin Tail-Risk Forecasting

## Requirements

R 4.3+ with packages: `data.table`, `gamlss`, `gamlss.dist`, `rugarch`, `forecast`, `sandwich`, `lmtest`, `ggplot2`, `ggtext`, `patchwork`, `scales`, `xtable`.

## Setup

Set your working directory to this repository root before running any script:

```r
setwd("/path/to/this/folder")
```

## Data

Raw CSVs in `data/` from CoinGecko, FRED, Alternative.me, Binance, and Coin Metrics (2015-2025).

## Pipeline

Run scripts in `scripts/` in numerical order (00-13).

| # | Script | Description |
|---|---|---|
| 00 | `00_setup.R` | Packages, constants, helper functions (sourced by other scripts) |
| 00 | `00_merge_and_clean.R` | Merge sources, LOCF imputation |
| 01 | `01_create_features.R` | Log-returns, growth rates, lags |
| 02 | `02_prepare_data.R` | Train/test split |
| 03-04 | `03_dist_selection_main.R`, `04_...robust.R` | Distribution selection (26 families) |
| 05-06 | `05_strategy_a_main.R`, `06_...robust.R` | Covariate/functional-form selection (Strategy A) |
| 07-08 | `07_rolling_main.R`, `08_...robust.R` | Rolling-window forecasts (PE + GT + benchmarks) |
| 09 | `09_backtest.R` | UC, CC, FZ0 backtesting (main + robust) |
| 10 | `10_dm_test.R` | Diebold-Mariano tests |
| 11-13 | `11-13_*.R` | Figures |

## Output

Results are saved to `outputs/` (tables and figures).
