#' @export
`$.RMediation::MBCOResult` <- function(x, name) {
  if (name == "chisq") {
    return(x@statistic)
  } else if (name == "p") {
    return(x@p_value)
  } else if (name %in% S7::prop_names(x)) {
    return(S7::prop(x, name))
  } else {
    return(NULL)
  }
}

#' @export
`[[.RMediation::MBCOResult` <- function(x, i, exact = TRUE) {
  if (is.character(i)) {
    if (i == "chisq") {
      return(x@statistic)
    } else if (i == "p") {
      return(x@p_value)
    } else if (i %in% S7::prop_names(x)) {
      return(S7::prop(x, i))
    }
  }
  NextMethod()
}

#' @export
`names.RMediation::MBCOResult` <- function(x) {
  # Return legacy names + new names
  unique(c("chisq", "p", S7::prop_names(x)))
}

#' @export
S7::method(print, MBCOResult) <- function(x, ...) {
  cat("MBCO Test Result\n")
  cat("================\n")
  cat("Test type:", x@type, "\n")
  cat("Chi-squared statistic:", round(x@statistic, 4), "\n")
  cat("Degrees of freedom:", x@df, "\n")
  cat("P-value:", format.pval(x@p_value, digits = 4), "\n")
  if (x@p_value < 0.001) {
    cat("Significance: *** (p < 0.001)\n")
  } else if (x@p_value < 0.01) {
    cat("Significance: ** (p < 0.01)\n")
  } else if (x@p_value < 0.05) {
    cat("Significance: * (p < 0.05)\n")
  } else {
    cat("Significance: ns (not significant)\n")
  }
  invisible(x)
}

#' @export
S7::method(summary, MBCOResult) <- function(object, ...) {
  cat("MBCO Test Summary\n")
  cat("=================\n")
  cat("Model-Based Constrained Optimization Test\n\n")
  cat("Test type:", object@type, "\n")
  cat("Chi-squared statistic:", round(object@statistic, 4), "\n")
  cat("Degrees of freedom:", object@df, "\n")
  cat("P-value:", format.pval(object@p_value, digits = 4), "\n\n")

  cat("Interpretation:\n")
  if (object@p_value < 0.05) {
    cat("The null hypothesis is rejected at the 0.05 level.\n")
    cat("There is significant evidence for the alternative model.\n")
  } else {
    cat("The null hypothesis is not rejected at the 0.05 level.\n")
    cat("There is insufficient evidence for the alternative model.\n")
  }

  invisible(object)
}

#' @export
S7::method(show, MBCOResult) <- function(object) {
  print(object)
}
