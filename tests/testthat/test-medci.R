# Test file for medci() function

test_that("medci works with basic parameters", {
  # Simple test with known parameters
  result <- medci(mu.x = 0.3, mu.y = 0.4, se.x = 0.1, se.y = 0.1, type = "asymp")

  expect_type(result, "list")
  expect_named(result, c("95% CI", "Estimate", "SE"))
  expect_length(result$`95% CI`, 2)
  expect_true(result$`95% CI`[1] < result$`95% CI`[2])
})

test_that("medci handles all type options", {
  mu.x <- 0.3
  mu.y <- 0.4
  se.x <- 0.1
  se.y <- 0.1

  # Test each type
  result_dop <- medci(mu.x, mu.y, se.x, se.y, type = "dop")
  result_mc <- medci(mu.x, mu.y, se.x, se.y, type = "mc", n.mc = 1e4)
  result_asymp <- medci(mu.x, mu.y, se.x, se.y, type = "asymp")
  result_all <- medci(mu.x, mu.y, se.x, se.y, type = "all", n.mc = 1e4)

  expect_type(result_dop, "list")
  expect_type(result_mc, "list")
  expect_type(result_asymp, "list")
  expect_type(result_all, "list")

  # When type="all", should return nested list
  expect_length(result_all, 3)
  expect_named(result_all, c("Distribution of Product", "Monte Carlo", "Asymptotic Normal"))
})

test_that("medci respects alpha parameter", {
  result_95 <- medci(0.3, 0.4, 0.1, 0.1, alpha = 0.05, type = "asymp")
  result_99 <- medci(0.3, 0.4, 0.1, 0.1, alpha = 0.01, type = "asymp")

  expect_named(result_95, c("95% CI", "Estimate", "SE"))
  expect_named(result_99, c("99% CI", "Estimate", "SE"))

  # 99% CI should be wider than 95% CI
  width_95 <- diff(result_95$`95% CI`)
  width_99 <- diff(result_99$`99% CI`)
  expect_true(width_99 > width_95)
})

test_that("medci handles rho parameter correctly", {
  # Test with rho = 0 (independence)
  result_rho0 <- medci(0.3, 0.4, 0.1, 0.1, rho = 0, type = "asymp")

  # Test with positive correlation
  result_rho_pos <- medci(0.3, 0.4, 0.1, 0.1, rho = 0.3, type = "asymp")

  # Test with negative correlation
  result_rho_neg <- medci(0.3, 0.4, 0.1, 0.1, rho = -0.3, type = "asymp")

  expect_type(result_rho0, "list")
  expect_type(result_rho_pos, "list")
  expect_type(result_rho_neg, "list")

  # SE should differ with different rho values
  expect_false(result_rho0$SE == result_rho_pos$SE)
})

test_that("medci validates parameter bounds", {
  # Test invalid rho values
  expect_error(
    medci(0.3, 0.4, 0.1, 0.1, rho = 1.5),
    "rho"
  )

  expect_error(
    medci(0.3, 0.4, 0.1, 0.1, rho = -1.5),
    "rho"
  )

  # Test invalid alpha values
  expect_error(
    medci(0.3, 0.4, 0.1, 0.1, alpha = 0),
    "alpha"
  )

  expect_error(
    medci(0.3, 0.4, 0.1, 0.1, alpha = 1),
    "alpha"
  )

  expect_error(
    medci(0.3, 0.4, 0.1, 0.1, alpha = 1.5),
    "alpha"
  )
})

test_that("medci validates type parameter", {
  expect_error(
    medci(0.3, 0.4, 0.1, 0.1, type = "invalid"),
    "should be one of"
  )
})

test_that("medci computes correct point estimate", {
  mu.x <- 0.3
  mu.y <- 0.4
  se.x <- 0.1
  se.y <- 0.1
  rho <- 0

  result <- medci(mu.x, mu.y, se.x, se.y, rho = rho, type = "asymp")

  # Point estimate should be mu.x * mu.y + rho * se.x * se.y
  expected_estimate <- mu.x * mu.y + rho * se.x * se.y
  expect_equal(result$Estimate, expected_estimate)
})

test_that("medci computes correct SE with rho=0", {
  mu.x <- 0.3
  mu.y <- 0.4
  se.x <- 0.1
  se.y <- 0.1
  rho <- 0

  result <- medci(mu.x, mu.y, se.x, se.y, rho = rho, type = "asymp")

  # Craig (1936) formula with rho=0
  expected_se <- sqrt(
    se.y^2 * mu.x^2 +
    se.x^2 * mu.y^2 +
    se.x^2 * se.y^2
  )

  expect_equal(result$SE, expected_se, tolerance = 1e-10)
})

test_that("medci MC method returns MC.Error", {
  result <- medci(0.3, 0.4, 0.1, 0.1, type = "mc", n.mc = 1e4)

  expect_true("MC.Error" %in% names(result))
  expect_true(is.numeric(result$MC.Error))
  expect_true(result$MC.Error > 0)
})

test_that("medci handles n.mc parameter", {
  # Small n.mc should run faster but be less precise
  result_small <- medci(0.3, 0.4, 0.1, 0.1, type = "mc", n.mc = 1e3)
  result_large <- medci(0.3, 0.4, 0.1, 0.1, type = "mc", n.mc = 1e4)

  expect_type(result_small, "list")
  expect_type(result_large, "list")

  # Larger n.mc should have smaller MC.Error
  expect_true(result_large$MC.Error < result_small$MC.Error)
})

test_that("medci plot parameters don't cause errors", {
  # plot=TRUE should not cause errors (but we won't actually display)
  expect_no_error(
    medci(0.3, 0.4, 0.1, 0.1, type = "asymp", plot = FALSE)
  )

  # Note: We don't test plot=TRUE in automated tests to avoid graphics device issues
})

test_that("medci handles edge case: zero indirect effect", {
  # When mu.x = 0, indirect effect should be 0 (with rho=0)
  result <- medci(0, 0.4, 0.1, 0.1, rho = 0, type = "asymp")

  expect_equal(result$Estimate, 0)
  expect_true(result$`95% CI`[1] < 0)
  expect_true(result$`95% CI`[2] > 0)
})

test_that("medci handles negative coefficients", {
  # Negative a path, positive b path
  result1 <- medci(-0.3, 0.4, 0.1, 0.1, type = "asymp")
  expect_true(result1$Estimate < 0)

  # Positive a path, negative b path
  result2 <- medci(0.3, -0.4, 0.1, 0.1, type = "asymp")
  expect_true(result2$Estimate < 0)

  # Both negative
  result3 <- medci(-0.3, -0.4, 0.1, 0.1, type = "asymp")
  expect_true(result3$Estimate > 0)
})

test_that("medci MC and DOP methods give similar results", {
  set.seed(123)

  result_mc <- medci(0.3, 0.4, 0.1, 0.1, type = "mc", n.mc = 1e5)
  result_dop <- medci(0.3, 0.4, 0.1, 0.1, type = "dop")

  # Estimates should be identical
  expect_equal(result_mc$Estimate, result_dop$Estimate, tolerance = 0.01)

  # CIs should be similar (within reason given MC error)
  expect_equal(result_mc$`95% CI`[1], result_dop$`95% CI`[1], tolerance = 0.03)
  expect_equal(result_mc$`95% CI`[2], result_dop$`95% CI`[2], tolerance = 0.03)
})
