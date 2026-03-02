# Distribution Choice in Bitcoin Tail-Risk Forecasting
# RBFin — March 2026
# 12_histogram.R — Return histogram with 6 fitted distribution families

library(data.table)
library(gamlss)
library(gamlss.dist)

PATH_OUTPUT  <- "./outputs"
PATH_FIGURES <- "./outputs/figures"

dt_train <- fread(file.path(PATH_OUTPUT, "data_train.csv"))
ret <- dt_train$ret[!is.na(dt_train$ret)]

# Fit distributions (intercept-only)
fits <- list()
for (fam in c("PE", "SHASH", "JSU", "GT", "TF", "NO")) {
  tryCatch({ fits[[fam]] <- gamlssML(ret, family = fam) }, error = function(e) NULL)
}

# Compute densities
x_seq <- seq(-0.25, 0.22, length.out = 2000)

get_density <- function(fit, family_name, x) {
  dfun <- match.fun(paste0("d", family_name))
  mu <- fitted(fit, what = "mu")[1]
  sigma <- fitted(fit, what = "sigma")[1]
  if (family_name %in% c("SHASH", "JSU", "GT"))
    dfun(x, mu = mu, sigma = sigma, nu = fitted(fit, what = "nu")[1], tau = fitted(fit, what = "tau")[1])
  else if (family_name %in% c("PE", "TF"))
    dfun(x, mu = mu, sigma = sigma, nu = fitted(fit, what = "nu")[1])
  else
    dfun(x, mu = mu, sigma = sigma)
}

densities <- list()
for (nm in names(fits))
  tryCatch({ densities[[nm]] <- get_density(fits[[nm]], nm, x_seq) }, error = function(e) NULL)

# Style
display <- list(
  PE    = list(label = "Power Exponential (PE)", col = "orangered",   lwd = 3.0, lty = 1),
  SHASH = list(label = "Sinh-Arcsinh (SHASH)",  col = "darkorange",  lwd = 2.0, lty = 1),
  GT    = list(label = "Generalized t (GT)",     col = "forestgreen", lwd = 2.0, lty = 4),
  JSU   = list(label = "Johnson SU",             col = "purple",      lwd = 2.0, lty = 5),
  TF    = list(label = "Student-t",              col = "dodgerblue",  lwd = 2.0, lty = 2),
  NO    = list(label = "Normal",                 col = "red",         lwd = 2.0, lty = 3))

pdf(file.path(PATH_FIGURES, "histogram_distributions.pdf"), width = 10, height = 6)
par(mar = c(4.5, 4.5, 3, 1))

hist(ret, breaks = 120, freq = FALSE,
     col = adjustcolor("grey80", 0.5), border = "grey90",
     xlim = c(-0.25, 0.20),
     ylim = c(0, max(sapply(densities, max, na.rm = TRUE)) * 1.05),
     xlab = "Daily Log-Returns", ylab = "Density", main = "",
     cex.lab = 1.2, cex.axis = 1.0)
title(main = "Bitcoin Returns: Distribution Comparison", cex.main = 1.4, line = 2)
mtext(sprintf("Training Set (2015-01-02 to 2022-12-31, N = %d)", length(ret)),
      side = 3, line = 0.5, cex = 1.0, col = "grey40")

for (nm in names(densities)) {
  s <- display[[nm]]
  lines(x_seq, densities[[nm]], col = s$col, lwd = s$lwd, lty = s$lty)
}

legend("topright",
       legend = sapply(names(densities), function(nm) display[[nm]]$label),
       col = sapply(names(densities), function(nm) display[[nm]]$col),
       lwd = sapply(names(densities), function(nm) display[[nm]]$lwd),
       lty = sapply(names(densities), function(nm) display[[nm]]$lty),
       cex = 0.85, bg = "white", title = "Distribution", title.col = "grey30")
dev.off()
