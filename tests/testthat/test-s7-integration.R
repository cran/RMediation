test_that("ci generic works with numeric input (legacy dispatch)", {
  # Legacy usage: ci(mu, Sigma, quant, ...)
  mu <- c(b1 = 1, b2 = .7, b3 = .6, b4 = .45)
  Sigma <- c(.05, 0, 0, 0, .05, 0, 0, .03, 0, .03)
  quant <- ~ b1 * b2 * b3 * b4
  
  # This should dispatch to .ci_core via S7 method for numeric
  res <- ci(mu = mu, Sigma = Sigma, quant = quant, type = "MC", plot = FALSE, plotCI = FALSE, n.mc = 1e4)
  
  expect_type(res, "list")
  expect_true("Estimate" %in% names(res))
  # CI is the first element (unnamed in list)
  expect_true(is.numeric(res[[1]]))
})

test_that("ci generic works with lavaan input (new auto-detection)", {
  skip_if_not_installed("lavaan")
  library(lavaan)
  
  # Create a simple data and model
  set.seed(1234)
  X <- rnorm(100)
  M <- 0.5*X + rnorm(100)
  Y <- 0.7*M + rnorm(100)
  Data <- data.frame(X = X, M = M, Y = Y)
  
  model <- '
    Y ~ b*M
    M ~ a*X
    ab := a*b
  '
  
  fit <- sem(model, data = Data)
  
  # Call ci(fit) - should auto-detect "ab"
  res <- ci(fit, level = 0.95, n.mc = 1e4)
  
  expect_type(res, "list")
  expect_true("ab" %in% names(res))
  # "ab" result is a list with CI, Estimate, SE from ProductNormal method
  expect_type(res$ab, "list")
  expect_true("CI" %in% names(res$ab))
  expect_length(res$ab$CI, 2)
})

test_that("ci generic works with lavaan input (legacy quant behavior)", {
  skip_if_not_installed("lavaan")
  library(lavaan)
  
  set.seed(1234)
  X <- rnorm(100)
  M <- 0.5*X + rnorm(100)
  Y <- 0.7*M + rnorm(100)
  Data <- data.frame(X = X, M = M, Y = Y)
  
  model <- '
    Y ~ b*M
    M ~ a*X
  '
  
  fit <- sem(model, data = Data)
  
  # Call ci(fit, quant = ...) - should use legacy logic
  res <- ci(fit, quant = ~ a*b, type = "MC", n.mc = 1e4, plot = FALSE)
  
  expect_type(res, "list")
  expect_true("Estimate" %in% names(res))
})
