## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(RMediation)

## ----method-comparison--------------------------------------------------------
# Same example parameters
mu.x <- 0.5  # Effect of X on M
mu.y <- 0.6  # Effect of M on Y (controlling for X)
se.x <- 0.08 # Standard error of a path
se.y <- 0.04 # Standard error of b path
rho <- 0     # Correlation between a and b

# Compare all methods
results <- medci(
  mu.x = mu.x, mu.y = mu.y,
  se.x = se.x, se.y = se.y,
  rho = rho,
  type = "all",  # This gives results for all methods
  plot = FALSE
)

# Print results
for(method_name in names(results)) {
  cat("\n", method_name, ":\n")
  # Display the structure of each result
  str(results[[method_name]])
}

## ----performance-comparison---------------------------------------------------
# Example of comparing results across different parameter values

# Define parameter values to test
param_sets <- list(
  list(mu.x = 0.5, mu.y = 0.6, se.x = 0.08, se.y = 0.04),
  list(mu.x = 0.3, mu.y = 0.4, se.x = 0.10, se.y = 0.05),
  list(mu.x = 0.1, mu.y = 0.2, se.x = 0.05, se.y = 0.03)
)

for(i in seq_along(param_sets)) {
  params <- param_sets[[i]]
  cat("\nParameter set", i, ":\n")
  cat("Effect sizes: a =", params$mu.x, ", b =", params$mu.y, "\n")

  results_subset <- medci(
    mu.x = params$mu.x, mu.y = params$mu.y,
    se.x = params$se.x, se.y = params$se.y,
    rho = 0,
    type = "all",
    plot = FALSE
  )

  # Print confidence intervals from each method
  for(method_name in names(results_subset)) {
    ci_values <- results_subset[[method_name]][[1]]  # Extract CI values
    cat(sprintf("%-25s: [%.3f, %.3f]\n", method_name, ci_values[1], ci_values[2]))
  }
}

## ----dop-example--------------------------------------------------------------
# Example using DOP method
result_dop <- medci(
  mu.x = 0.5, mu.y = 0.6,
  se.x = 0.08, se.y = 0.04,
  rho = 0,
  type = "dop"
)

cat("DOP Method Results:\n")
str(result_dop)

## ----mc-example---------------------------------------------------------------
# Example using MC method
result_mc <- medci(
  mu.x = 0.5, mu.y = 0.6,
  se.x = 0.08, se.y = 0.04,
  rho = 0,
  type = "mc",
  n.mc = 1e5  # Specify Monte Carlo sample size
)

cat("Monte Carlo Method Results:\n")
str(result_mc)

## ----asymptotic-example-------------------------------------------------------
# Example using asymptotic method
result_asymp <- medci(
  mu.x = 0.5, mu.y = 0.6,
  se.x = 0.08, se.y = 0.04,
  rho = 0,
  type = "asymp"
)

cat("Asymptotic Method Results:\n")
str(result_asymp)

## ----s7-example, eval = FALSE-------------------------------------------------
# # Create a ProductNormal object (represents distribution of product)
# pn <- ProductNormal(
#   mu = c(0.5, 0.6),  # Means of the two variables
#   Sigma = matrix(c(0.0064, 0, 0, 0.0016), 2, 2)  # Covariance matrix (se.x^2, 0, 0, se.y^2)
# )
# 
# # Compute cumulative distribution function
# cat("CDF at 0.1:", cdf(pn, q = 0.1), "\n")
# cat("CDF at 0.3:", cdf(pn, q = 0.3), "\n")
# 
# # Compute quantiles
# cat("2.5% quantile:", quantile(pn, p = 0.025), "\n")
# cat("97.5% quantile:", quantile(pn, p = 0.975), "\n")
# 
# # Compute confidence interval
# ci_result <- ci(pn, level = 0.95)
# cat("95% Confidence Interval:", ci_result, "\n")
# 
# # Print and summary methods
# show(pn)  # Use show instead of print
# summary(pn)  # This should work as it's a different S7 method

