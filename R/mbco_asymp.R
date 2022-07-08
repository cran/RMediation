
mbco_asymp <- function(h0 = NULL,
                       h1 = NULL,
                       alpha = .05) {
  mbco <- OpenMx::mxCompare(h1, h0)
  mbco_chisq <- mbco$diffLL[2]    # Chi-square
  mbco_pvalue <- mbco$p[2]      # p-value
  mbco_df <- mbco$df[2]-mbco$df[1]
  mbcoTest <-
    list(
      chisq = mbco_chisq,
      df = mbco_df,
      p = mbco_pvalue
    )
  return(mbcoTest)
}
