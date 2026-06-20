test_that("Legacy ci(mu, Sigma, ...) works with positional arguments", {
  mu <- c(b1 = 1, b2 = 0.7, b3 = 0.6, b4 = 0.45)
  Sigma <- diag(c(0.05, 0.05, 0.05, 0.05))
  names(Sigma) <- names(mu)
  rownames(Sigma) <- names(mu)
  
  # Legacy call with positional arguments
  # ci(mu, Sigma, quant, ...)
  # quant is ~ b1 * b2 * b3 * b4
  
  # We expect this to work and NOT throw an error about unused arguments or mismatch
  res <- ci(mu, Sigma, ~ b1 * b2 * b3 * b4, type = "asymp")
  
  # Legacy ci returns a matrix/list for type="asymp"
  expect_true(is.list(res) || is.matrix(res))
  # Check if it contains "97.5%" in names or colnames
  if (is.matrix(res)) {
    expect_true("97.5% CI" %in% colnames(res))
  } else {
    expect_true("97.5% CI" %in% names(res))
  }
})

test_that("Legacy ci works with named arguments", {
  mu <- c(b1 = 1, b2 = 0.7)
  Sigma <- diag(0.1, 2)
  names(mu) <- c("b1", "b2")
  colnames(Sigma) <- rownames(Sigma) <- names(mu)
  
  # This should now work because generic argument is 'mu'
  res <- ci(mu = mu, Sigma = Sigma, quant = ~b1*b2, type = "asymp")
  expect_true(is.list(res) || is.matrix(res))
})
