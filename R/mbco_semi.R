############# semi-parametric mbco

mbco_semi <- function(h0 = NULL,
                      h1 = NULL,
                      R = 10L,
                      alpha = .05,
                      checkHess = "No",
                      checkSE = "No",
                      optim = "NPSOL",
                      precision = 1e-9) {
  res <- OpenMx::mxCompare(h1, h0)
  mbco_chisq <- res$diffLL[2] # Asymptotic chi-square
  mbco_df <- res$df[2] - res$df[1] # df
  OpenMx::mxOption(NULL, "Calculate Hessian", checkHess)
  OpenMx::mxOption(NULL, "Standard Errors", checkSE)
  OpenMx::mxOption(NULL, "Function precision", precision)
  OpenMx::mxOption(NULL, "Default optimizer", optim)

  df <-
    h1$data$observed    #Extract the original data from the fitted model
  df <-
    df[h1$manifestVars]   #Seubset of teh manifest variables used to fit the model
  if (any(lapply(df, class) %in% c('factor', 'character')))
    #Check to see if any of the manisfest variables is factor or character
    stop("One of the variables is factor or character. Please change it to numeric.")

  S <- cov(df)   #  sample covariance marix
  US <- chol(S) # Cholesky upper triangular matrix
  US_inv <- solve(US)   # Cholesky inverse
  Sigma0 <- OpenMx::mxGetExpected(h0, "covariance")   # Reproduced population Matrix under the null
  # if(!matrixcalc::is.positive.definite(Sigma0) ){
  #   Sigma0 <- Matrix::nearPD(Sigma0)
  #   warning(" Sigma0 is not positive defnite. A near positive defnite Sigma0 will be used instead.")
  # }

  USigma0 <- tryCatch(chol(Sigma0), error=function(e) stop("Sigma0 is not positive defnite") )  # Cholesky of Sigma0
  df_trans <- as.matrix(df) %*% US_inv %*% USigma0
  df_trans <- data.frame(df_trans)
  compare_boot <- function(x, df) {
    df <- modelr::resample_bootstrap(df)
    df <- data.frame(df)
    h0 <-
      OpenMx::mxRun(
        OpenMx::mxModel(h0, OpenMx::mxData(df, type = 'raw')),
        silent = TRUE,
        suppressWarnings = TRUE
      )
    h1 <-
      OpenMx::mxRun(
        OpenMx::mxModel(h1, OpenMx::mxData(df, type = 'raw')),
        silent = TRUE,
        suppressWarnings = TRUE
      )
    OpenMx::mxCompare(h1, h0)$diffLL[2]
  }

  null_samp <- sapply(seq.int(R), compare_boot, df = df_trans)
  mbco_pvalue <- mean(null_samp > mbco_chisq)
  mbco_boot <- list(chisq = mbco_chisq,
                    df = mbco_df,
                    p = mbco_pvalue)
  return(mbco_boot)
}
