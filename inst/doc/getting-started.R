## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)
library(RMediation)

## ----example1-----------------------------------------------------------------
# Example: Single mediator with known coefficients and standard errors
result <- medci(
    mu.x = 0.5, # Effect of X on M
    mu.y = 0.6, # Effect of M on Y (controlling for X)
    se.x = 0.08, # Standard error of a path
    se.y = 0.04, # Standard error of b path
    rho = 0, # Correlation between a and b (often 0 for experimental X)
    type = "dop" # Distribution of the product method
)

# Display the structure of the result
str(result)

# Or access specific components
cat("Confidence Interval:", result$`95% CI`, "\n")
cat("Estimate:", result$Estimate, "\n")
cat("Standard Error:", result$SE, "\n")

## ----example2-----------------------------------------------------------------
# Example: Two sequential mediators
result2 <- ci(
    mu = c(b1 = 1, b2 = .7, b3 = .6, b4 = .45),
    Sigma = c(.05, 0, 0, 0, .05, 0, 0, .03, 0, .03),
    quant = ~ b1 * b2 * b3 * b4,
    type = "MC",
    plot = FALSE # Set to TRUE to see visualization
)

str(result2)

## ----example3, eval = FALSE---------------------------------------------------
# # Create a ProductNormal distribution object
# pn <- ProductNormal(
#     mu = c(0.5, 0.3), # Means of the two normal variables
#     Sigma = matrix(c(0.01, 0.002, 0.002, 0.01), 2, 2) # Covariance matrix
# )
# 
# # Compute confidence interval
# ci_result <- ci(pn, level = 0.95)
# 
# show(pn) # Use show instead of print
# str(ci_result) # Use str to show structure

## ----methods------------------------------------------------------------------
# Compare different methods
comparison <- medci(
    mu.x = 0.5,
    mu.y = 0.6,
    se.x = 0.08,
    se.y = 0.04,
    rho = 0,
    type = "all",
    plot = FALSE
)

# Show comparison
str(comparison)

## ----lavaan-integration, eval = FALSE-----------------------------------------
# library(lavaan)
# 
# # Define a simple mediation model
# model <- '
#   # Direct effect
#   Y ~ c*X
# 
#   # Mediator
#   M ~ a*X
#   Y ~ b*M
# 
#   # Indirect effect
#   ab := a*b
#   # Total effect
#   total := c + (a*b)
# '
# # Simulate data
# set.seed(123)
# n <- 1000
# X <- rnorm(n)
# M <- 0.5 * X + rnorm(n)
# Y <- 0.3 * M + 0.2 * X + rnorm(n)
# df <- data.frame(X, M, Y)
# 
# # This would be fitted with your data
# fit <- sem(model, data = df)
# 
# # Automatically compute CI for defined parameters
# ci(fit) # This would auto-detect 'ab' parameter

## ----validation, eval = FALSE-------------------------------------------------
# # These would throw helpful error messages:
# # medci(mu.x = 0.5, mu.y = 0.6, se.x = -0.1, se.y = 0.04)  # Invalid negative SE
# # ci(mu = c(0.5), Sigma = matrix(1), quant = ~ b1)  # Dimension mismatch

