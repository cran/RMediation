#' Integration test: end-to-end serial mediation via the REAL medfit extractor
#'
#' Unlike test-ci-medfit-covariance.R (which builds SerialMediationData by hand),
#' this fits an actual lavaan model, runs medfit::extract_mediation() to produce
#' a SerialMediationData, and feeds it to ci(). This exercises the full wiring:
#' fit -> medfit serial extractor (@vcov named a, d1.., b, c_prime) -> rmediation
#' .serial_d_labels resolver -> ci_serial_mediation_data().
#'
#' Requires medfit >= 0.2.0 — the first release shipping the serial extractor.

skip_if_not_installed("medfit")
skip_if_not_installed("lavaan")
skip_if(
  utils::packageVersion("medfit") < "0.2.0",
  "serial mediation extractor requires medfit >= 0.2.0"
)

test_that("lavaan 2-mediator chain: extract_mediation() -> ci() recovers the indirect effect", {
  set.seed(42)
  n <- 800
  X  <- rnorm(n)
  M1 <- 0.5 * X  + rnorm(n)
  M2 <- 0.6 * M1 + rnorm(n)
  Y  <- 0.7 * M2 + 0.2 * X + rnorm(n)
  dat <- data.frame(X, M1, M2, Y)

  model <- "M1 ~ a*X
            M2 ~ d1*M1
            Y  ~ b*M2 + cp*X"
  fit <- lavaan::sem(model, data = dat)

  mu <- medfit::extract_mediation(
    fit, treatment = "X", mediator = c("M1", "M2"), outcome = "Y"
  )

  # medfit emits the name contract rmediation's resolver depends on
  expect_true(all(c("a", "d1", "b", "c_prime") %in% names(mu@estimates)))
  expect_true(all(c("a", "d1", "b") %in% rownames(mu@vcov)))
  expect_length(mu@d_path, 1L) # k - 1 for 2 mediators

  res <- ci(mu, level = 0.95, type = "MC")

  # Return shape
  expect_type(res, "list")
  expect_true(all(c("CI", "Estimate", "SE", "type", "k") %in% names(res)))
  expect_identical(res$k, 1L)

  # Point estimate is a sensible serial indirect effect (true a*d1*b = 0.21).
  # Range check, not expect_equal (testthat tolerance is RELATIVE) — the wiring
  # check that matters is the CI bracketing the truth below.
  expect_gt(unname(res$Estimate), 0.12)
  expect_lt(unname(res$Estimate), 0.30)

  # The 95% CI brackets the truth and is a proper interval
  expect_lt(res$CI[["lower"]], 0.21)
  expect_gt(res$CI[["upper"]], 0.21)
  expect_lt(res$CI[["lower"]], res$CI[["upper"]])
})

test_that("lm/glm 2-mediator chain: extract_mediation(mediator_models=) -> ci() works", {
  # Exercises the lm/sequential-regression serial path (medfit's `mediator_models`
  # argument) — the half of decision #2 that was the original lm gap. lm chains are
  # block-structured (separate equations), distinct from the lavaan single-equation case.
  set.seed(7)
  n <- 800
  X  <- rnorm(n)
  M1 <- 0.5 * X  + rnorm(n)
  M2 <- 0.6 * M1 + rnorm(n)
  Y  <- 0.7 * M2 + 0.2 * X + rnorm(n)
  dat <- data.frame(X, M1, M2, Y)

  fit_m1 <- lm(M1 ~ X, data = dat)      # first mediator model
  fit_m2 <- lm(M2 ~ M1, data = dat)     # mediators 2..k
  fit_y  <- lm(Y ~ M2 + X, data = dat)  # outcome model

  mu <- medfit::extract_mediation(
    fit_m1, model_y = fit_y, treatment = "X",
    mediator = c("M1", "M2"), mediator_models = list(fit_m2)
  )

  expect_true(all(c("a", "d1", "b", "c_prime") %in% names(mu@estimates)))
  expect_true(all(c("a", "d1", "b") %in% rownames(mu@vcov)))
  expect_length(mu@d_path, 1L)

  res <- ci(mu, level = 0.95, type = "MC")

  expect_type(res, "list")
  expect_true(all(c("CI", "Estimate", "SE") %in% names(res)))
  # Sensible serial indirect effect (true a*d1*b = 0.21); generous band, lm point
  # estimates vary more than the single-equation SEM fit.
  expect_gt(unname(res$Estimate), 0.12)
  expect_lt(unname(res$Estimate), 0.33)
  expect_lt(res$CI[["lower"]], 0.21)
  expect_gt(res$CI[["upper"]], 0.21)
  expect_lt(res$CI[["lower"]], res$CI[["upper"]])
})
