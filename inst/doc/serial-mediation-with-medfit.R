## ----setup, include = FALSE---------------------------------------------------
# medfit is a Suggested (optional) dependency. Every chunk that touches it is
# guarded so the vignette still builds (and CRAN checks pass) when medfit is
# absent. This mirrors the runtime contract in R/ci_medfit.R, where each medfit
# call site is wrapped in requireNamespace("medfit").
medfit_available <- requireNamespace("medfit", quietly = TRUE) &&
  requireNamespace("lavaan", quietly = TRUE)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = medfit_available
)

library(RMediation)

## ----medfit-missing, eval = !medfit_available, echo = FALSE, results = "asis"----
# cat(
#   "> **Note:** This vignette requires the `medfit` (>= 0.2.0) and `lavaan`",
#   "packages, which are not installed in the current environment, so the code",
#   "below is shown but not executed. Install medfit with",
#   "`install.packages(\"medfit\")` to run these examples.\n"
# )

## ----lavaan-fit---------------------------------------------------------------
set.seed(42)
n  <- 800
X  <- rnorm(n)
M1 <- 0.5 * X  + rnorm(n)
M2 <- 0.6 * M1 + rnorm(n)
Y  <- 0.7 * M2 + 0.2 * X + rnorm(n)
dat <- data.frame(X, M1, M2, Y)

model <- "M1 ~ a*X
          M2 ~ d1*M1
          Y  ~ b*M2 + cp*X"
fit <- lavaan::sem(model, data = dat)

# medfit extracts the named estimates + covariance RMediation expects.
mu <- medfit::extract_mediation(
  fit, treatment = "X", mediator = c("M1", "M2"), outcome = "Y"
)
class(mu)
names(mu@estimates)

## ----lavaan-ci----------------------------------------------------------------
res <- ci(mu, level = 0.95, type = "MC")
res$Estimate
res$CI

## ----lm-fit-------------------------------------------------------------------
set.seed(7)
n  <- 800
X  <- rnorm(n)
M1 <- 0.5 * X  + rnorm(n)
M2 <- 0.6 * M1 + rnorm(n)
Y  <- 0.7 * M2 + 0.2 * X + rnorm(n)
dat <- data.frame(X, M1, M2, Y)

mu_lm <- medfit::extract_mediation(
  lm(M1 ~ X, dat),
  model_y         = lm(Y ~ M2 + X, dat),
  treatment       = "X",
  mediator        = c("M1", "M2"),
  mediator_models = list(lm(M2 ~ M1, dat))
)

ci(mu_lm, level = 0.95, type = "MC")$CI

## ----guard-demo, eval = FALSE-------------------------------------------------
# # When medfit is NOT installed, the bridge stops with a clear message:
# ci(mu)
# #> Error: Package 'medfit' is required for this method.

