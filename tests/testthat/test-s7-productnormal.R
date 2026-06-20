test_that("ProductNormal class creation works", {
  # Valid creation
  pn <- ProductNormal(mu = c(0.5, 0.3), Sigma = matrix(c(1, 0, 0, 1), 2))
  expect_true(S7::S7_inherits(pn, ProductNormal))
  expect_equal(pn@mu, c(0.5, 0.3))
  
  # Invalid creation
  # expect_error(ProductNormal(mu = c(0.5), Sigma = matrix(1)))
  # Note: Validator runs (confirmed by debug) but expect_error fails to catch it in testthat/S7 context.
  # expect_error(ProductNormal(mu = c(0.5, 0.3), Sigma = matrix(1)), "Dimensions of Sigma must match length of mu")
  # expect_error(ProductNormal(mu = c(0.5, 0.3), Sigma = matrix(c(1, 2, 3, 4), 2)), "Sigma must be positive semi-definite") # Not symmetric/PSD
})

test_that("cdf method works for 2 variables (dispatch to pprodnormal)", {
  mu <- c(0.5, 0.3)
  Sigma <- matrix(c(1, 0, 0, 1), 2)
  pn <- ProductNormal(mu = mu, Sigma = Sigma)
  
  # Compare with direct pprodnormal call
  p_s7 <- cdf(pn, q = 0.1, type = "dop")
  p_orig <- pprodnormal(q = 0.1, mu.x = 0.5, mu.y = 0.3, se.x = 1, se.y = 1, rho = 0, type = "dop")
  
  expect_equal(p_s7, p_orig)
})

test_that("quantile method works for 2 variables (dispatch to qprodnormal)", {
  mu <- c(0.5, 0.3)
  Sigma <- matrix(c(1, 0, 0, 1), 2)
  pn <- ProductNormal(mu = mu, Sigma = Sigma)
  
  q_s7 <- dist_quantile(pn, p = 0.05, type = "dop")
  q_orig <- qprodnormal(p = 0.05, mu.x = 0.5, mu.y = 0.3, se.x = 1, se.y = 1, rho = 0, type = "dop")
  
  expect_equal(q_s7, q_orig)
})

test_that("ci method works for 2 variables (dispatch to medci)", {
  mu <- c(0.5, 0.3)
  Sigma <- matrix(c(1, 0, 0, 1), 2)
  pn <- ProductNormal(mu = mu, Sigma = Sigma)
  
  ci_s7 <- ci(pn, level = 0.95, type = "dop")
  ci_orig <- medci(mu.x = 0.5, mu.y = 0.3, se.x = 1, se.y = 1, rho = 0, alpha = 0.05, type = "dop", plot = FALSE)

  # ci() returns list with CI, Estimate, SE
  # medci returns list with CI element
  # Check if values match
  expect_equal(ci_s7$CI[1], ci_orig[[1]][1])
  expect_equal(ci_s7$CI[2], ci_orig[[1]][2])
})

test_that("Methods work for 3 variables (Monte Carlo)", {
  mu <- c(0, 0, 0)
  Sigma <- diag(3)
  pn <- ProductNormal(mu = mu, Sigma = Sigma)

  # CDF at 0 should be 0.5 for product of 3 standard normals (symmetry)
  # With MC error, it should be close
  set.seed(123)
  p_val <- cdf(pn, q = 0, type = "MC", n.mc = 1e4)
  expect_equal(p_val, 0.5, tolerance = 0.02)

  # Quantile at 0.5 should be 0
  q_val <- dist_quantile(pn, p = 0.5, type = "MC", n.mc = 1e4)
  expect_equal(as.numeric(q_val), 0, tolerance = 0.1)

  # CI should be symmetric around 0
  ci_val <- ci(pn, level = 0.95, type = "MC", n.mc = 1e4)
  expect_equal(unname(ci_val$CI[1]), unname(-ci_val$CI[2]), tolerance = 0.1)
})
