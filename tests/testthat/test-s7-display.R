test_that("ProductNormal display methods work", {
  mu <- c(0.5, 0.3)
  Sigma <- matrix(c(0.01, 0.002, 0.002, 0.01), 2, 2)
  pn <- ProductNormal(mu = mu, Sigma = Sigma)
  
  # Test print
  expect_output(print(pn), "ProductNormal Distribution")
  expect_output(print(pn), "Number of variables: 2")
  expect_output(print(pn), "Means:")
  expect_output(print(pn), "Covariance matrix:")
  
  # Test summary
  expect_output(summary(pn), "ProductNormal Distribution Summary")
  expect_output(summary(pn), "Standard Deviations:")
  expect_output(summary(pn), "Correlation Matrix:")
  
  # Test show
  expect_output(show(pn), "ProductNormal Distribution")
})

test_that("MBCOResult display methods work", {
  res <- MBCOResult(statistic = 10.5, df = 2, p_value = 0.005, type = "asymp")
  
  # Test print
  expect_output(print(res), "MBCO Test Result")
  expect_output(print(res), "Test type: asymp")
  expect_output(print(res), "Chi-squared statistic:")
  expect_output(print(res), "P-value:")
  expect_output(print(res), "Significance:")
  
  # Test summary
  expect_output(summary(res), "MBCO Test Summary")
  expect_output(summary(res), "Model-Based Constrained Optimization")
  expect_output(summary(res), "Interpretation:")
  
  # Test show
  expect_output(show(res), "MBCO Test Result")
  
  # Test significance levels
  res_ns <- MBCOResult(statistic = 1.5, df = 2, p_value = 0.47, type = "asymp")
  expect_output(print(res_ns), "ns \\(not significant\\)")
  
  res_sig <- MBCOResult(statistic = 15.5, df = 2, p_value = 0.0004, type = "asymp")
  expect_output(print(res_sig), "\\*\\*\\* \\(p < 0.001\\)")
})
