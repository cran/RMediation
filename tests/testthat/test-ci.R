# Test file for ci() function
# Note: ci() returns list with first element (unnamed) containing CI bounds,
# then named elements: Estimate, SE, MCError (for MC type), p

test_that("ci works with basic parameters", {
  mu <- c(b1 = 0.3, b2 = 0.4)
  Sigma <- diag(0.01, 2)
  result <- ci(mu, Sigma, quant = ~b1*b2, type = "asymp")

  expect_type(result, "list")
  expect_true("Estimate" %in% names(result))
  expect_true("SE" %in% names(result))
  # CI is the first element (unnamed)
  expect_length(result[[1]], 2)
  expect_true(result[[1]][1] < result[[1]][2])
})

test_that("ci handles formula interface correctly", {
  mu <- c(b1 = 0.3, b2 = 0.4, b3 = 0.5)
  Sigma <- diag(0.01, 3)

  # Simple product
  result1 <- ci(mu, Sigma, quant = ~b1*b2, type = "asymp")
  expect_equal(result1$Estimate, 0.3 * 0.4)

  # Three-way product
  result2 <- ci(mu, Sigma, quant = ~b1*b2*b3, type = "asymp")
  expect_equal(result2$Estimate, 0.3 * 0.4 * 0.5)

  # Sum of products
  result3 <- ci(mu, Sigma, quant = ~b1*b2 + b2*b3, type = "asymp")
  expect_equal(result3$Estimate, (0.3 * 0.4) + (0.4 * 0.5))
})

test_that("ci works with vector Sigma (stacked lower triangle)", {
  mu <- c(b1 = 0.3, b2 = 0.4)

  # 2x2 covariance matrix has 3 unique elements in lower triangle
  # Sigma = | 0.01  0   |
  #         | 0     0.01|
  # Stacked: c(0.01, 0, 0.01)
  Sigma_vec <- c(0.01, 0, 0.01)

  result <- ci(mu, Sigma_vec, quant = ~b1*b2, type = "asymp")

  expect_type(result, "list")
  expect_true(is.numeric(result$Estimate))
})

test_that("ci validates quant parameter", {
  mu <- c(b1 = 0.3, b2 = 0.4)
  Sigma <- diag(0.01, 2)

  # Wrong parameter names
  expect_error(
    ci(mu, Sigma, quant = ~b1*b3),
    "names"
  )
})

test_that("ci handles type parameter", {
  mu <- c(b1 = 0.3, b2 = 0.4)
  Sigma <- diag(0.01, 2)

  result_mc <- ci(mu, Sigma, quant = ~b1*b2, type = "MC", n.mc = 1e4)
  result_asymp <- ci(mu, Sigma, quant = ~b1*b2, type = "asymp")
  result_all <- ci(mu, Sigma, quant = ~b1*b2, type = "all", n.mc = 1e4)

  expect_type(result_mc, "list")
  expect_type(result_asymp, "list")
  expect_type(result_all, "list")

  # type="all" should return nested list
  expect_length(result_all, 2)
  expect_named(result_all, c("MC", "Asymptotic"))
})

test_that("ci MC method returns MCError", {
  mu <- c(b1 = 0.3, b2 = 0.4)
  Sigma <- diag(0.01, 2)

  result <- ci(mu, Sigma, quant = ~b1*b2, type = "MC", n.mc = 1e4)

  expect_true("MCError" %in% names(result))
  expect_true(is.numeric(result$MCError))
  expect_true(result$MCError > 0)
})

test_that("ci respects alpha parameter", {
  mu <- c(b1 = 0.3, b2 = 0.4)
  Sigma <- diag(0.01, 2)

  result_95 <- ci(mu, Sigma, quant = ~b1*b2, alpha = 0.05, type = "asymp")
  result_99 <- ci(mu, Sigma, quant = ~b1*b2, alpha = 0.01, type = "asymp")

  # Both should have Estimate and SE
  expect_true("Estimate" %in% names(result_95))
  expect_true("Estimate" %in% names(result_99))

  # 99% CI should be wider than 95% CI
  width_95 <- diff(result_95[[1]])
  width_99 <- diff(result_99[[1]])
  expect_true(width_99 > width_95)
})

test_that("ci handles automatic parameter naming", {
  # Without names, parameters should be b1, b2, ...
  mu <- c(0.3, 0.4, 0.5)
  Sigma <- diag(0.01, 3)

  result <- ci(mu, Sigma, quant = ~b1*b2*b3, type = "asymp")

  expect_equal(result$Estimate, 0.3 * 0.4 * 0.5)
})

test_that("ci handles complex functions", {
  mu <- c(a1 = 0.3, b1 = 0.4, a2 = 0.2, b2 = 0.5)
  Sigma <- diag(0.01, 4)

  # Total indirect effect
  result <- ci(mu, Sigma, quant = ~(a1*b1) + (a2*b2), type = "asymp")

  expected <- (0.3 * 0.4) + (0.2 * 0.5)
  expect_equal(result$Estimate, expected)
})

test_that("ci handles covariance between parameters", {
  mu <- c(b1 = 0.3, b2 = 0.4)

  # Covariance matrix with correlation
  Sigma_cov <- matrix(c(0.01, 0.005, 0.005, 0.01), 2, 2)

  result <- ci(mu, Sigma_cov, quant = ~b1*b2, type = "asymp")

  expect_type(result, "list")
  expect_true(is.numeric(result$SE))
})

test_that("ci handles n.mc parameter", {
  mu <- c(b1 = 0.3, b2 = 0.4)
  Sigma <- diag(0.01, 2)

  result_small <- ci(mu, Sigma, quant = ~b1*b2, type = "MC", n.mc = 1e3)
  result_large <- ci(mu, Sigma, quant = ~b1*b2, type = "MC", n.mc = 1e4)

  expect_type(result_small, "list")
  expect_type(result_large, "list")

  # Larger n.mc should have smaller MCError
  expect_true(result_large$MCError < result_small$MCError)
})

test_that("ci handles H0 parameter", {
  mu <- c(b1 = 0.3, b2 = 0.4, b3 = 0.3)
  Sigma <- diag(0.01, 3)

  # Test with H0 = TRUE and mu0
  result <- ci(mu, Sigma, quant = ~b1*b2*b3,
               type = "MC", n.mc = 1e4,
               H0 = TRUE, mu0 = c(b1 = 0.3, b2 = 0.4, b3 = 0))

  expect_type(result, "list")
})

test_that("ci handles edge case: zero effect", {
  mu <- c(b1 = 0, b2 = 0.4)
  Sigma <- diag(0.01, 2)

  result <- ci(mu, Sigma, quant = ~b1*b2, type = "asymp")

  expect_equal(result$Estimate, 0)
})

test_that("ci handles negative coefficients", {
  mu <- c(b1 = -0.3, b2 = 0.4)
  Sigma <- diag(0.01, 2)

  result <- ci(mu, Sigma, quant = ~b1*b2, type = "asymp")

  expect_true(result$Estimate < 0)
})

test_that("ci MC and asymptotic methods produce consistent estimates", {
  set.seed(123)

  mu <- c(b1 = 0.3, b2 = 0.4)
  Sigma <- diag(0.01, 2)

  result_mc <- ci(mu, Sigma, quant = ~b1*b2, type = "MC", n.mc = 1e5)
  result_asymp <- ci(mu, Sigma, quant = ~b1*b2, type = "asymp")

  # Point estimates should be very similar (MC has sampling variability)
  expect_equal(result_mc$Estimate, result_asymp$Estimate, tolerance = 0.01)

  # SEs should be similar
  expect_equal(as.numeric(result_mc$SE), as.numeric(result_asymp$SE), tolerance = 0.03)
})

test_that("ci handles four-path indirect effects", {
  mu <- c(b1 = 1, b2 = 0.7, b3 = 0.6, b4 = 0.45)
  Sigma_vec <- c(0.05, 0, 0, 0, 0.05, 0, 0, 0.03, 0, 0.03)

  result <- ci(mu, Sigma_vec, quant = ~b1*b2*b3*b4, type = "asymp")

  expected <- 1 * 0.7 * 0.6 * 0.45
  expect_equal(result$Estimate, expected)
})
