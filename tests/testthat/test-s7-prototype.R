# Test S7 Prototype vs Legacy Implementation

test_that("S7 core .compute_ci_dop() matches legacy medciMeeker()", {
  # Test parameters
  mu.x <- 0.2
  mu.y <- 0.4
  se.x <- 1
  se.y <- 1
  rho <- 0
  alpha <- 0.05

  # Legacy result
  legacy <- medciMeeker(mu.x, mu.y, se.x, se.y, rho, alpha)

  # S7 core result
  Sigma <- matrix(c(se.x^2, rho * se.x * se.y,
                    rho * se.x * se.y, se.y^2), nrow = 2)
  pn <- ProductNormal(mu = c(mu.x, mu.y), Sigma = Sigma)
  s7 <- .compute_ci_dop(pn, alpha)

  # Compare CI
  expect_equal(s7$CI, legacy[[1]], tolerance = 1e-10)

  # Compare Estimate
  expect_equal(s7$Estimate, legacy$Estimate, tolerance = 1e-10)

  # Compare SE
  expect_equal(s7$SE, legacy$SE, tolerance = 1e-10)
})

test_that("S7 core .compute_ci_mc() matches legacy medciMC()", {
  # Test parameters
  mu.x <- 0.2
  mu.y <- 0.4
  se.x <- 1
  se.y <- 1
  rho <- 0
  alpha <- 0.05
  n.mc <- 1e5  # Larger for better convergence

  # Legacy result
  set.seed(12345)
  legacy <- medciMC(mu.x, mu.y, se.x, se.y, rho, alpha, n.mc)

  # S7 core result
  set.seed(12345)
  Sigma <- matrix(c(se.x^2, rho * se.x * se.y,
                    rho * se.x * se.y, se.y^2), nrow = 2)
  pn <- ProductNormal(mu = c(mu.x, mu.y), Sigma = Sigma)
  s7 <- .compute_ci_mc(pn, alpha, n.mc)

  # Compare CI (should be reasonably close)
  expect_equal(s7$CI[1], legacy[[1]][1], tolerance = 0.05)
  expect_equal(s7$CI[2], legacy[[1]][2], tolerance = 0.05)

  # Compare Estimate (MC, so more tolerance needed - within 10% of true value)
  expect_equal(s7$Estimate, mu.x * mu.y, tolerance = 0.05)
  expect_equal(legacy$Estimate, mu.x * mu.y, tolerance = 0.05)

  # Compare SE
  expect_equal(s7$SE, legacy$SE, tolerance = 0.05)
})

test_that("S7 core .compute_ci_asymp() matches legacy medciAsymp()", {
  # Test parameters
  mu.x <- 0.2
  mu.y <- 0.4
  se.x <- 1
  se.y <- 1
  rho <- 0
  alpha <- 0.05

  # Legacy result
  legacy <- medciAsymp(mu.x, mu.y, se.x, se.y, rho, alpha)

  # S7 core result
  Sigma <- matrix(c(se.x^2, rho * se.x * se.y,
                    rho * se.x * se.y, se.y^2), nrow = 2)
  pn <- ProductNormal(mu = c(mu.x, mu.y), Sigma = Sigma)
  s7 <- .compute_ci_asymp(pn, alpha)

  # Compare CI
  expect_equal(s7$CI, legacy[[1]], tolerance = 1e-10)

  # Compare Estimate
  expect_equal(s7$Estimate, legacy$Estimate, tolerance = 1e-10)

  # Compare SE
  expect_equal(s7$SE, legacy$SE, tolerance = 1e-10)
})

test_that("ProductNormal ci() S7 method returns correct structure", {
  Sigma <- matrix(c(1, 0, 0, 1), nrow = 2)
  pn <- ProductNormal(mu = c(0.2, 0.4), Sigma = Sigma)

  result <- ci(pn, level = 0.95, type = "dop")

  expect_type(result, "list")
  expect_named(result, c("CI", "Estimate", "SE"))
  expect_length(result$CI, 2)
  expect_true(result$CI[1] < result$CI[2])
})

test_that("ProductNormal ci() with type='all' returns all methods", {
  Sigma <- matrix(c(1, 0, 0, 1), nrow = 2)
  pn <- ProductNormal(mu = c(0.2, 0.4), Sigma = Sigma)

  result <- ci(pn, level = 0.95, type = "all", n.mc = 1e4)

  expect_type(result, "list")
  expect_named(result, c("dop", "mc", "asymp"))

  # Each method should return proper structure
  expect_named(result$dop, c("CI", "Estimate", "SE"))
  expect_named(result$mc, c("CI", "Estimate", "SE", "MC.Error"))
  expect_named(result$asymp, c("CI", "Estimate", "SE"))
})

test_that("medci_prototype() wrapper matches legacy medci() with type='dop'", {
  mu.x <- 0.2
  mu.y <- 0.4
  se.x <- 1
  se.y <- 1
  rho <- 0
  alpha <- 0.05

  # Legacy
  legacy <- medci(mu.x, mu.y, se.x, se.y, rho, alpha, type = "dop")

  # Prototype wrapper
  prototype <- medci_prototype(mu.x, mu.y, se.x, se.y, rho, alpha, type = "dop")

  # Compare structure
  expect_equal(names(prototype), names(legacy))

  # Compare CI
  expect_equal(prototype[[1]], legacy[[1]], tolerance = 1e-10)

  # Compare Estimate
  expect_equal(prototype$Estimate, legacy$Estimate, tolerance = 1e-10)

  # Compare SE
  expect_equal(prototype$SE, legacy$SE, tolerance = 1e-10)
})

test_that("medci_prototype() wrapper matches legacy medci() with type='mc'", {
  mu.x <- 0.2
  mu.y <- 0.4
  se.x <- 1
  se.y <- 1
  rho <- 0
  alpha <- 0.05
  n.mc <- 1e5  # Larger for better convergence

  # Legacy
  set.seed(123)
  legacy <- medci(mu.x, mu.y, se.x, se.y, rho, alpha, type = "mc", n.mc = n.mc)

  # Prototype wrapper
  set.seed(123)
  prototype <- medci_prototype(mu.x, mu.y, se.x, se.y, rho, alpha, type = "mc", n.mc = n.mc)

  # Compare structure
  expect_equal(names(prototype), names(legacy))

  # Compare CI (MC, so some tolerance)
  expect_equal(prototype[[1]], legacy[[1]], tolerance = 0.01)

  # Compare Estimate
  expect_equal(prototype$Estimate, legacy$Estimate, tolerance = 0.01)
})

test_that("medci_prototype() wrapper matches legacy medci() with type='asymp'", {
  mu.x <- 0.2
  mu.y <- 0.4
  se.x <- 1
  se.y <- 1
  rho <- 0
  alpha <- 0.05

  # Legacy
  legacy <- medci(mu.x, mu.y, se.x, se.y, rho, alpha, type = "asymp")

  # Prototype wrapper
  prototype <- medci_prototype(mu.x, mu.y, se.x, se.y, rho, alpha, type = "asymp")

  # Compare structure
  expect_equal(names(prototype), names(legacy))

  # Compare CI
  expect_equal(prototype[[1]], legacy[[1]], tolerance = 1e-10)

  # Compare Estimate
  expect_equal(prototype$Estimate, legacy$Estimate, tolerance = 1e-10)

  # Compare SE
  expect_equal(prototype$SE, legacy$SE, tolerance = 1e-10)
})

test_that("medci_prototype() with type='all' matches legacy structure", {
  mu.x <- 0.2
  mu.y <- 0.4
  se.x <- 1
  se.y <- 1
  rho <- 0.5
  alpha <- 0.05

  # Legacy
  legacy <- medci(mu.x, mu.y, se.x, se.y, rho, alpha, type = "all")

  # Prototype wrapper
  prototype <- medci_prototype(mu.x, mu.y, se.x, se.y, rho, alpha, type = "all", n.mc = 1e5)

  # Compare structure
  expect_equal(names(prototype), names(legacy))
  expect_length(prototype, 3)

  # Compare DOP results
  expect_equal(prototype$`Distribution of Product`[[1]],
               legacy$`Distribution of Product`[[1]],
               tolerance = 1e-10)
})

test_that("S7 ProductNormal supports N-variable products with MC", {
  # 3-variable product
  mu <- c(0.2, 0.3, 0.4)
  Sigma <- diag(3)
  pn <- ProductNormal(mu = mu, Sigma = Sigma)

  # Should work with MC
  result <- ci(pn, level = 0.95, type = "mc", n.mc = 1e4)

  expect_type(result, "list")
  expect_named(result, c("CI", "Estimate", "SE", "MC.Error"))
  expect_length(result$CI, 2)
})

test_that("S7 ProductNormal errors on N-variable with DOP method", {
  # 3-variable product
  mu <- c(0.2, 0.3, 0.4)
  Sigma <- diag(3)
  pn <- ProductNormal(mu = mu, Sigma = Sigma)

  # Should error with DOP
  expect_error(
    ci(pn, level = 0.95, type = "dop"),
    "currently only supports 2 variables"
  )
})

test_that("Edge case: high correlation rho=0.9", {
  mu.x <- 0.5
  mu.y <- 0.3
  se.x <- 1
  se.y <- 1
  rho <- 0.9
  alpha <- 0.05

  # Legacy
  legacy <- medci(mu.x, mu.y, se.x, se.y, rho, alpha, type = "dop")

  # Prototype
  prototype <- medci_prototype(mu.x, mu.y, se.x, se.y, rho, alpha, type = "dop")

  expect_equal(prototype[[1]], legacy[[1]], tolerance = 1e-10)
})

test_that("Edge case: negative correlation rho=-0.7", {
  mu.x <- 0.5
  mu.y <- 0.3
  se.x <- 1
  se.y <- 1
  rho <- -0.7
  alpha <- 0.05

  # Legacy
  legacy <- medci(mu.x, mu.y, se.x, se.y, rho, alpha, type = "asymp")

  # Prototype
  prototype <- medci_prototype(mu.x, mu.y, se.x, se.y, rho, alpha, type = "asymp")

  expect_equal(prototype[[1]], legacy[[1]], tolerance = 1e-10)
})
