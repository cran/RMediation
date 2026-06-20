test_that("mbco supports legacy list-style access", {
  skip_if_not_installed("OpenMx")
  library(OpenMx)
  
  # Use real MxModel objects to trigger dispatch
  h0 <- mxModel("NullModel")
  h1 <- mxModel("AltModel")
  
  # We know the internal logic fails with empty models, but we want to test the RETURN object.
  # Since we can't easily get a return object from the actual call without it erroring on logic,
  # we will manually create an MBCOResult object and test its compatibility.
  # This assumes the mbco method returns exactly this type of object.
  
  res <- MBCOResult(statistic = 10.5, df = 2, p_value = 0.005, type = "asymp")
  
  # Legacy code expects a list with components: chisq, df, p
  # And accesses them via $ or [[
  
  # Check $ access
  # Note: The property names in MBCOResult are statistic, df, p_value, type.
  # Legacy names were: chisq, df, p.
  # So we need to map:
  # $chisq -> @statistic
  # $df -> @df
  # $p -> @p_value
  
  # These expectations will likely FAIL initially
  expect_equal(res$chisq, 10.5)
  expect_equal(res$df, 2)
  expect_equal(res$p, 0.005)
  
  # Check [[ access
  expect_equal(res[["chisq"]], 10.5)
})
