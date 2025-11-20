# Test file for pprodnormal() and qprodnormal() functions

test_that("pprodnormal works with basic parameters", {
  result <- pprodnormal(0.1, mu.x = 0.3, mu.y = 0.4, se.x = 0.1, se.y = 0.1)

  expect_type(result, "double")
  expect_length(result, 1)
  expect_true(result >= 0 && result <= 1)
})

test_that("qprodnormal works with basic parameters", {
  result <- qprodnormal(0.5, mu.x = 0.3, mu.y = 0.4, se.x = 0.1, se.y = 0.1)

  expect_type(result, "double")
  expect_length(result, 1)
  expect_true(is.finite(result))
})

test_that("pprodnormal and qprodnormal are inverse functions", {
  mu.x <- 0.3
  mu.y <- 0.4
  se.x <- 0.1
  se.y <- 0.1

  # Test at different probability levels
  probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)

  for (p in probs) {
    q <- qprodnormal(p, mu.x, mu.y, se.x, se.y, type = "MC", n.mc = 1e4)
    p_recovered <- pprodnormal(q, mu.x, mu.y, se.x, se.y, type = "MC", n.mc = 1e4)

    expect_equal(p_recovered, p, tolerance = 0.05)
  }
})

test_that("pprodnormal handles type parameter", {
  q <- 0.1
  mu.x <- 0.3
  mu.y <- 0.4
  se.x <- 0.1
  se.y <- 0.1

  result_dop <- pprodnormal(q, mu.x, mu.y, se.x, se.y, type = "dop")
  result_mc <- pprodnormal(q, mu.x, mu.y, se.x, se.y, type = "MC", n.mc = 1e4)

  expect_type(result_dop, "double")
  expect_type(result_mc, "double")

  # Results should be similar
  expect_equal(result_dop, result_mc, tolerance = 0.05)
})

test_that("qprodnormal handles type parameter", {
  p <- 0.5
  mu.x <- 0.3
  mu.y <- 0.4
  se.x <- 0.1
  se.y <- 0.1

  result_dop <- qprodnormal(p, mu.x, mu.y, se.x, se.y, type = "dop")
  result_mc <- qprodnormal(p, mu.x, mu.y, se.x, se.y, type = "MC", n.mc = 1e4)

  expect_type(result_dop, "double")
  expect_type(result_mc, "double")

  # Results should be similar
  expect_equal(result_dop, result_mc, tolerance = 0.02)
})

test_that("pprodnormal is monotonically increasing", {
  mu.x <- 0.3
  mu.y <- 0.4
  se.x <- 0.1
  se.y <- 0.1

  q_values <- seq(0, 0.3, by = 0.05)
  p_values <- sapply(q_values, function(q) {
    pprodnormal(q, mu.x, mu.y, se.x, se.y, type = "MC", n.mc = 1e4)
  })

  # Check monotonicity
  expect_true(all(diff(p_values) >= 0))
})

test_that("qprodnormal is monotonically increasing", {
  mu.x <- 0.3
  mu.y <- 0.4
  se.x <- 0.1
  se.y <- 0.1

  p_values <- seq(0.1, 0.9, by = 0.1)
  q_values <- sapply(p_values, function(p) {
    qprodnormal(p, mu.x, mu.y, se.x, se.y, type = "MC", n.mc = 1e4)
  })

  # Check monotonicity
  expect_true(all(diff(q_values) >= 0))
})

test_that("pprodnormal handles rho parameter", {
  q <- 0.1
  mu.x <- 0.3
  mu.y <- 0.4
  se.x <- 0.1
  se.y <- 0.1

  result_rho0 <- pprodnormal(q, mu.x, mu.y, se.x, se.y, rho = 0, type = "MC", n.mc = 1e4)
  result_rho_pos <- pprodnormal(q, mu.x, mu.y, se.x, se.y, rho = 0.3, type = "MC", n.mc = 1e4)
  result_rho_neg <- pprodnormal(q, mu.x, mu.y, se.x, se.y, rho = -0.3, type = "MC", n.mc = 1e4)

  expect_type(result_rho0, "double")
  expect_type(result_rho_pos, "double")
  expect_type(result_rho_neg, "double")

  # Results should differ with different rho
  expect_false(result_rho0 == result_rho_pos)
})

test_that("qprodnormal handles rho parameter", {
  p <- 0.5
  mu.x <- 0.3
  mu.y <- 0.4
  se.x <- 0.1
  se.y <- 0.1

  result_rho0 <- qprodnormal(p, mu.x, mu.y, se.x, se.y, rho = 0, type = "dop")
  result_rho_pos <- qprodnormal(p, mu.x, mu.y, se.x, se.y, rho = 0.3, type = "dop")

  expect_type(result_rho0, "double")
  expect_type(result_rho_pos, "double")

  # Results should differ
  expect_false(abs(result_rho0 - result_rho_pos) < 0.001)
})

test_that("pprodnormal validates probability bounds", {
  # p should be between 0 and 1 for valid CDF values
  result_0 <- pprodnormal(-1e6, 0.3, 0.4, 0.1, 0.1, type = "MC", n.mc = 1e4)
  result_1 <- pprodnormal(1e6, 0.3, 0.4, 0.1, 0.1, type = "MC", n.mc = 1e4)

  expect_equal(result_0, 0, tolerance = 0.01)
  expect_equal(result_1, 1, tolerance = 0.01)
})

test_that("qprodnormal validates input probability", {
  expect_error(
    qprodnormal(1.5, 0.3, 0.4, 0.1, 0.1),
    "p"
  )

  expect_error(
    qprodnormal(-0.5, 0.3, 0.4, 0.1, 0.1),
    "p"
  )
})

test_that("pprodnormal handles extreme quantiles", {
  mu.x <- 0.3
  mu.y <- 0.4
  se.x <- 0.1
  se.y <- 0.1

  # Very small quantile should give p close to 0
  result_small <- pprodnormal(-1, mu.x, mu.y, se.x, se.y, type = "MC", n.mc = 1e4)
  expect_true(result_small < 0.1)

  # Very large quantile should give p close to 1
  result_large <- pprodnormal(1, mu.x, mu.y, se.x, se.y, type = "MC", n.mc = 1e4)
  expect_true(result_large > 0.9)
})

test_that("pprodnormal and qprodnormal handle n.mc parameter", {
  # Test that larger n.mc doesn't cause errors
  result_p <- pprodnormal(0.1, 0.3, 0.4, 0.1, 0.1, type = "MC", n.mc = 1e5)
  result_q <- qprodnormal(0.5, 0.3, 0.4, 0.1, 0.1, type = "MC", n.mc = 1e5)

  expect_type(result_p, "double")
  expect_type(result_q, "double")
})

test_that("pprodnormal lower.tail parameter works", {
  q <- 0.1
  mu.x <- 0.3
  mu.y <- 0.4
  se.x <- 0.1
  se.y <- 0.1

  result_lower <- pprodnormal(q, mu.x, mu.y, se.x, se.y,
                              lower.tail = TRUE, type = "MC", n.mc = 1e4)
  result_upper <- pprodnormal(q, mu.x, mu.y, se.x, se.y,
                              lower.tail = FALSE, type = "MC", n.mc = 1e4)

  # Should be complements
  expect_equal(result_lower + result_upper, 1, tolerance = 0.01)
})

test_that("DOP and MC methods converge with large n.mc", {
  set.seed(123)

  q <- 0.12
  mu.x <- 0.3
  mu.y <- 0.4
  se.x <- 0.1
  se.y <- 0.1

  result_dop <- pprodnormal(q, mu.x, mu.y, se.x, se.y, type = "dop")
  result_mc <- pprodnormal(q, mu.x, mu.y, se.x, se.y, type = "MC", n.mc = 1e5)

  # With large n.mc, MC should be close to DOP
  expect_equal(result_dop, result_mc, tolerance = 0.02)
})
