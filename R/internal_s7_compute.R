# Internal S7-native compute functions
# These functions form the computational core and work with ProductNormal objects

#' Internal: Compute CI using Distribution of Product (Meeker & Escobar 1994)
#'
#' Computes confidence intervals for the product of two normal random variables
#' using the exact distribution method. The point estimate is
#' \eqn{\mu_x\mu_y + \sigma_{xy}} and the standard error follows Craig (1936):
#' \deqn{SE = \sqrt{\sigma_y^2\mu_x^2 + \sigma_x^2\mu_y^2 + 2\mu_x\mu_y\rho\sigma_x\sigma_y + \sigma_x^2\sigma_y^2 + \sigma_x^2\sigma_y^2\rho^2}}
#'
#' @param object ProductNormal object
#' @param alpha Significance level (default: 0.05)
#' @return List with CI, Estimate, SE
#' @keywords internal
.compute_ci_dop <- function(object, alpha = 0.05) {
  mu <- object@mu
  Sigma <- object@Sigma

  if (length(mu) != 2) {
    stop("Distribution of Product method currently only supports 2 variables. Use type='mc' for N-variable products.")
  }

  # Extract parameters
  se.x <- sqrt(Sigma[1, 1])
  se.y <- sqrt(Sigma[2, 2])
  rho <- Sigma[1, 2] / (se.x * se.y)
  mu.x <- mu[1]
  mu.y <- mu[2]

  # Compute quantiles using Meeker's algorithm
  p <- alpha / 2
  q.l <- .compute_quantile_dop_internal(p, mu.x, mu.y, se.x, se.y, rho, lower.tail = TRUE)
  q.u <- .compute_quantile_dop_internal(p, mu.x, mu.y, se.x, se.y, rho, lower.tail = FALSE)

  # Compute point estimate and SE (Craig 1936)
  quantMean <- mu.x * mu.y + se.x * se.y * rho
  quantSE <- sqrt(
    se.y^2 * mu.x^2 +
    se.x^2 * mu.y^2 +
    2 * mu.x * mu.y * rho * se.x * se.y +
    se.x^2 * se.y^2 +
    se.x^2 * se.y^2 * rho^2
  )

  list(
    CI = c(q.l$q, q.u$q),
    Estimate = quantMean,
    SE = quantSE
  )
}

#' Internal: Compute quantile using Meeker & Escobar (1994) algorithm
#'
#' @keywords internal
.compute_quantile_dop_internal <- function(p, mu.x, mu.y, se.x, se.y, rho = 0, lower.tail = TRUE) {
  max.iter <- 1000

  # Rescale distributions so SD = 1
  mu.a <- mu.x / se.x
  mu.b <- mu.y / se.y
  se.ab <- sqrt(1 + mu.a^2 + mu.b^2 + 2 * mu.a * mu.b * rho + rho^2)
  s.a.on.b <- sqrt(1 - rho^2) # SD of b conditional on a

  if (lower.tail == FALSE) {
    u0 <- mu.a * mu.b + 6 * se.ab # upper
    l0 <- mu.a * mu.b - 6 * se.ab
    alpha <- 1 - p
  } else {
    l0 <- mu.a * mu.b - 6 * se.ab # lower
    u0 <- mu.a * mu.b + 6 * se.ab
    alpha <- p
  }

  # Define CDF function
  gx <- function(x, z) {
    mu.a.on.b <- mu.a + rho * (x - mu.b) # mean of a conditional on b
    integ <- pnorm(sign(x) * (z / x - mu.a.on.b) / s.a.on.b) * dnorm(x - mu.b)
    return(integ)
  }

  fx <- function(z) {
    return(integrate(gx, lower = -Inf, upper = Inf, z = z)$value - alpha)
  }

  # Find bracketing interval
  p.l <- fx(l0)
  p.u <- fx(u0)
  iter <- 0

  while (p.l > 0) {
    iter <- iter + 1
    l0 <- l0 - 0.5 * se.ab
    p.l <- fx(l0)
    if (iter > max.iter) {
      stop("Numerical algorithm does not work in type='dop'! please use type='MC'.\n")
    }
  }

  iter <- 0 # Reset iteration counter
  while (p.u < 0) {
    iter <- iter + 1
    u0 <- u0 + 0.5 * se.ab
    p.u <- fx(u0)
    if (iter > max.iter) {
      stop("Numerical algorithm does not work in type='dop'! please use type='MC'.\n")
    }
  }

  # Root finding
  res <- uniroot(fx, c(l0, u0))
  new <- res$root * se.x * se.y # Scale back
  error.new <- res$estim.prec

  list(q = new, error = error.new)
}

#' Internal: Compute CI using Monte Carlo method
#'
#' @param object ProductNormal object
#' @param alpha Significance level (default: 0.05)
#' @param n.mc Monte Carlo sample size (default: 1e5)
#' @return List with CI, Estimate, SE, MC.Error
#' @keywords internal
.compute_ci_mc <- function(object, alpha = 0.05, n.mc = 1e5) {
  mu <- object@mu
  Sigma <- object@Sigma

  # Generate multivariate normal samples
  samples <- MASS::mvrnorm(n = n.mc, mu = mu, Sigma = Sigma)

  # Compute product
  if (length(mu) == 2) {
    # 2-variable case: simple product
    ab <- samples[, 1] * samples[, 2]
  } else {
    # N-variable case: product across all variables
    ab <- apply(samples, 1, prod)
  }

  # Compute statistics
  quantMean <- mean(ab)
  quantSE <- sd(ab)
  quantError <- quantSE / sqrt(n.mc)
  CI <- quantile(ab, c(alpha / 2, 1 - alpha / 2))
  names(CI) <- NULL

  list(
    CI = CI,
    Estimate = quantMean,
    SE = quantSE,
    MC.Error = quantError
  )
}

#' Internal: Compute CI using Asymptotic method (Delta method)
#'
#' Computes asymptotic confidence intervals using normal approximation (Sobel test).
#' The standard error is computed using Craig's (1936) formula:
#' \deqn{SE = \sqrt{\sigma_y^2\mu_x^2 + \sigma_x^2\mu_y^2 + 2\mu_x\mu_y\rho\sigma_x\sigma_y + \sigma_x^2\sigma_y^2 + \sigma_x^2\sigma_y^2\rho^2}}
#' and the CI is \eqn{Estimate \pm z_{1-\alpha/2} \times SE}, where \eqn{z_{1-\alpha/2}}
#' is the standard normal quantile.
#'
#' @param object ProductNormal object
#' @param alpha Significance level (default: 0.05)
#' @return List with CI, Estimate, SE
#' @keywords internal
.compute_ci_asymp <- function(object, alpha = 0.05) {
  mu <- object@mu
  Sigma <- object@Sigma

  if (length(mu) != 2) {
    stop("Asymptotic method currently only supports 2 variables. Use type='mc' for N-variable products.")
  }

  # Extract parameters
  se.x <- sqrt(Sigma[1, 1])
  se.y <- sqrt(Sigma[2, 2])
  rho <- Sigma[1, 2] / (se.x * se.y)
  mu.x <- mu[1]
  mu.y <- mu[2]

  # Compute point estimate and SE (Craig 1936)
  quantMean <- mu.x * mu.y + se.x * se.y * rho
  quantSE <- sqrt(
    se.y^2 * mu.x^2 +
    se.x^2 * mu.y^2 +
    2 * mu.x * mu.y * rho * se.x * se.y +
    se.x^2 * se.y^2 +
    se.x^2 * se.y^2 * rho^2
  )

  # Asymptotic CI using normal approximation (Sobel test)
  CI <- quantMean + c(qnorm(alpha / 2), qnorm(1 - alpha / 2)) * quantSE

  list(
    CI = CI,
    Estimate = quantMean,
    SE = quantSE
  )
}

#' Internal: Compute CDF using Distribution of Product
#'
#' Computes the cumulative distribution function \eqn{P(XY \le q)} for the product
#' of two normal random variables using numerical integration (Meeker & Escobar, 1994).
#'
#' @param object ProductNormal object
#' @param q Quantile value
#' @return Probability \eqn{P(XY \le q)}
#' @keywords internal
.compute_cdf_dop <- function(object, q) {
  mu <- object@mu
  Sigma <- object@Sigma

  if (length(mu) != 2) {
    stop("Distribution of Product CDF currently only supports 2 variables. Use type='mc' for N-variable products.")
  }

  # Extract parameters
  se.x <- sqrt(Sigma[1, 1])
  se.y <- sqrt(Sigma[2, 2])
  rho <- Sigma[1, 2] / (se.x * se.y)
  mu.x <- mu[1]
  mu.y <- mu[2]

  # Rescale
  mu.a <- mu.x / se.x
  mu.b <- mu.y / se.y
  s.a.on.b <- sqrt(1 - rho^2)
  z <- q / (se.x * se.y)

  # Integration function
  gx <- function(x, z) {
    mu.a.on.b <- mu.a + rho * (x - mu.b)
    integ <- pnorm(sign(x) * (z / x - mu.a.on.b) / s.a.on.b) * dnorm(x - mu.b)
    return(integ)
  }

  result <- integrate(gx, lower = -Inf, upper = Inf, z = z)
  list(p = result$value, error = result$abs.error)
}

#' Internal: Compute CDF using Monte Carlo
#'
#' Computes the cumulative distribution function \eqn{P(XY \le q)} for the product
#' of normal random variables using Monte Carlo simulation. Works for any number
#' of variables.
#'
#' @param object ProductNormal object
#' @param q Quantile value
#' @param n.mc Monte Carlo sample size
#' @return Probability \eqn{P(XY \le q)}
#' @keywords internal
.compute_cdf_mc <- function(object, q, n.mc = 1e5) {
  mu <- object@mu
  Sigma <- object@Sigma

  # Generate samples
  samples <- MASS::mvrnorm(n = n.mc, mu = mu, Sigma = Sigma)

  # Compute product
  if (length(mu) == 2) {
    prod_samples <- samples[, 1] * samples[, 2]
  } else {
    prod_samples <- apply(samples, 1, prod)
  }

  # Empirical CDF
  p <- mean(prod_samples <= q)
  mc_error <- sqrt(p * (1 - p) / n.mc)

  list(p = p, error = mc_error)
}

#' Internal: Compute quantile using Distribution of Product
#'
#' @param object ProductNormal object
#' @param p Probability
#' @param lower.tail Logical; if TRUE (default), probabilities are P(X <= x)
#' @return Quantile value
#' @keywords internal
.compute_quantile_dop <- function(object, p, lower.tail = TRUE) {
  mu <- object@mu
  Sigma <- object@Sigma

  if (length(mu) != 2) {
    stop("Distribution of Product quantile currently only supports 2 variables. Use type='mc' for N-variable products.")
  }

  # Extract parameters
  se.x <- sqrt(Sigma[1, 1])
  se.y <- sqrt(Sigma[2, 2])
  rho <- Sigma[1, 2] / (se.x * se.y)
  mu.x <- mu[1]
  mu.y <- mu[2]

  .compute_quantile_dop_internal(p, mu.x, mu.y, se.x, se.y, rho, lower.tail)
}

#' Internal: Compute quantile using Monte Carlo
#'
#' @param object ProductNormal object
#' @param p Probability
#' @param n.mc Monte Carlo sample size
#' @param lower.tail Logical; if TRUE (default), probabilities are P(X <= x)
#' @return Quantile value
#' @keywords internal
.compute_quantile_mc <- function(object, p, n.mc = 1e5, lower.tail = TRUE) {
  mu <- object@mu
  Sigma <- object@Sigma

  # Generate samples
  samples <- MASS::mvrnorm(n = n.mc, mu = mu, Sigma = Sigma)

  # Compute product
  if (length(mu) == 2) {
    prod_samples <- samples[, 1] * samples[, 2]
  } else {
    prod_samples <- apply(samples, 1, prod)
  }

  # Compute quantile
  if (lower.tail) {
    q <- quantile(prod_samples, probs = p)
  } else {
    q <- quantile(prod_samples, probs = 1 - p)
  }

  # Estimate error
  mc_error <- sd(prod_samples) / sqrt(n.mc)

  list(q = as.numeric(q), error = mc_error)
}
