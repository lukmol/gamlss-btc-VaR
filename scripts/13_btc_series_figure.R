# Distribution Choice in Bitcoin Tail-Risk Forecasting
# RBFin — March 2026
# 13_btc_series_figure.R — Bitcoin price and returns (ggplot2 + patchwork)

library(data.table)
library(ggplot2)
library(ggtext)
library(scales)
library(patchwork)

PATH_OUTPUT  <- "./outputs"
PATH_FIGURES <- "./outputs/figures"

dt <- fread(file.path(PATH_OUTPUT, "data_full.csv"))
dt[, date := as.Date(date)]
setorder(dt, date)

train_end     <- as.Date("2022-12-31")
rolling_start <- as.Date("2024-01-01")
test_end      <- as.Date("2025-12-29")
first_window_start <- dt$date[which(dt$date >= rolling_start)[1] - 1000]

theme_paper <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_markdown(size = 13, face = "bold", margin = margin(b = 4)),
    plot.subtitle = element_markdown(size = 9, color = "grey40", margin = margin(b = 8)),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 10),
    axis.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(8, 12, 4, 8),
    legend.position = "none")

col_train <- "#2166AC"; col_test <- "#B2182B"

# Panel A: Price
p1 <- ggplot(dt, aes(x = date, y = price)) +
  annotate("rect", xmin = min(dt$date), xmax = train_end, ymin = -Inf, ymax = Inf, fill = col_train, alpha = 0.07) +
  annotate("rect", xmin = rolling_start, xmax = test_end, ymin = -Inf, ymax = Inf, fill = col_test, alpha = 0.07) +
  annotate("segment", x = first_window_start, xend = rolling_start, y = 95000, yend = 95000,
           color = "grey40", arrow = arrow(ends = "both", length = unit(0.08, "inches")), linewidth = 0.4) +
  annotate("text", x = first_window_start + (rolling_start - first_window_start)/2, y = 100000,
           label = "1st rolling window (1,000 days)", size = 2.8, color = "grey40") +
  geom_line(linewidth = 0.4, color = "grey20") +
  geom_vline(xintercept = train_end, linetype = "dashed", color = col_train, linewidth = 0.5) +
  geom_vline(xintercept = rolling_start, linetype = "dashed", color = col_test, linewidth = 0.5) +
  annotate("label", x = as.Date("2018-06-01"), y = 90000, label = "Training\n(selection)",
           size = 3, color = col_train, fill = "white", label.size = 0, fontface = "bold") +
  annotate("label", x = as.Date("2023-06-15"), y = 90000, label = "2023\n(in rolling\nwindow)",
           size = 2.5, color = "grey50", fill = "white", label.size = 0) +
  annotate("label", x = as.Date("2025-01-01"), y = 90000, label = "Test\n(out-of-sample)",
           size = 3, color = col_test, fill = "white", label.size = 0, fontface = "bold") +
  scale_y_continuous(labels = label_dollar(prefix = "$", big.mark = ","), expand = expansion(mult = c(0.02, 0.08))) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.01))) +
  labs(title = "**A.** Bitcoin Daily Closing Price (USD)", subtitle = "January 2015 -- December 2025", y = "Price (USD)") +
  theme_paper

# Panel B: Returns
crashes <- data.table(
  date = as.Date(c("2020-03-12", "2021-05-19", "2022-06-13")),
  label = c("COVID-19\ncrash", "China\nban", "LUNA/\nTerra"),
  y_off = c(-0.02, -0.02, -0.02))

p2 <- ggplot(dt[!is.na(ret)], aes(x = date, y = ret)) +
  annotate("rect", xmin = min(dt$date), xmax = train_end, ymin = -Inf, ymax = Inf, fill = col_train, alpha = 0.07) +
  annotate("rect", xmin = rolling_start, xmax = test_end, ymin = -Inf, ymax = Inf, fill = col_test, alpha = 0.07) +
  geom_segment(aes(xend = date, yend = 0), linewidth = 0.15, color = "grey50") +
  geom_hline(yintercept = quantile(dt$ret, 0.01, na.rm = TRUE), linetype = "dotted", color = col_test, linewidth = 0.4) +
  annotate("text", x = as.Date("2015-06-01"), y = quantile(dt$ret, 0.01, na.rm = TRUE) - 0.015,
           label = sprintf("Empirical 1%% quantile = %.1f%%", 100*quantile(dt$ret, 0.01, na.rm = TRUE)),
           size = 2.8, color = col_test, hjust = 0) +
  geom_point(data = crashes, aes(x = date, y = dt[match(crashes$date, dt$date), ret]),
             color = col_test, size = 1.5, shape = 16) +
  geom_text(data = crashes,
            aes(x = date, y = dt[match(crashes$date, dt$date), ret] + y_off, label = label),
            size = 2.3, color = "grey30", lineheight = 0.85) +
  geom_vline(xintercept = train_end, linetype = "dashed", color = col_train, linewidth = 0.5) +
  geom_vline(xintercept = rolling_start, linetype = "dashed", color = col_test, linewidth = 0.5) +
  scale_y_continuous(labels = label_percent(), expand = expansion(mult = c(0.05, 0.05))) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.01))) +
  labs(title = "**B.** Daily Log-Returns",
       subtitle = paste0("N = ", format(sum(!is.na(dt$ret)), big.mark = ","), " observations"),
       y = "Log-return") +
  theme_paper

combined <- p1 / p2 + plot_layout(heights = c(1, 1))
ggsave(file.path(PATH_FIGURES, "btc_series_periods.pdf"), combined, width = 10, height = 7.5, device = cairo_pdf)
