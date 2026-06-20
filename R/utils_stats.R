# Internal statistical utility functions
# These replace e1071 and modelr dependencies

#' @noRd
#' @keywords internal
.skewness <- function(x, type = 2, na.rm = TRUE) {

  # Replicates e1071::skewness with type = 2 (sample skewness)
  # Type 2 is the SAS and SPSS default: g1 * sqrt(n*(n-1)) / (n-2)
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)

  m <- mean(x)
  s <- sqrt(sum((x - m)^2) / n)  # population sd
  if (s == 0) return(NA_real_)

  g1 <- sum((x - m)^3) / (n * s^3)  # moment coefficient of skewness

  if (type == 1) {
    return(g1)
  } else if (type == 2) {
    # Adjusted Fisher-Pearson (SAS/SPSS default)
    return(g1 * sqrt(n * (n - 1)) / (n - 2))
  } else if (type == 3) {
    # b1 type
    return(g1 * ((n - 1) / n)^(3/2))
  } else {
    return(g1)
  }
}

#' @noRd
#' @keywords internal
.kurtosis <- function(x, type = 2, na.rm = TRUE) {
  # Replicates e1071::kurtosis with type = 2 (excess kurtosis, SAS/SPSS style)
  # Type 2 uses: ((n+1)*G2 + 6) * (n-1) / ((n-2)*(n-3))
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  if (n < 4) return(NA_real_)

  m <- mean(x)
  s2 <- sum((x - m)^2) / n  # population variance
  if (s2 == 0) return(NA_real_)

  m4 <- sum((x - m)^4) / n
  g2 <- m4 / s2^2 - 3  # excess kurtosis (moment coefficient)

  if (type == 1) {
    return(g2)
  } else if (type == 2) {
    # Adjusted (SAS/SPSS default)
    return(((n + 1) * g2 + 6) * (n - 1) / ((n - 2) * (n - 3)))
  } else if (type == 3) {
    # b2 type
    return((g2 + 3) * ((n - 1) / n)^2 - 3)
  } else {
    return(g2)
  }
}

#' @noRd
#' @keywords internal
.resample_bootstrap <- function(data) {
  # Replicates modelr::resample_bootstrap behavior

  # Returns a data frame with bootstrap sample (sampling with replacement)
  n <- nrow(data)
  idx <- sample.int(n, n, replace = TRUE)
  data[idx, , drop = FALSE]
}
