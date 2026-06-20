#' @importFrom S7 method
NULL

#' @export
S7::method(cdf, ProductNormal) <- function(object, q, type = "dop", n.mc = 1e5, lower.tail = TRUE, ...) {
  checkmate::assert_numeric(q)
  type <- tolower(type)  # Handle case insensitivity
  type <- match.arg(type, c("dop", "mc", "all"))
  checkmate::assert_count(n.mc, positive = TRUE)
  checkmate::assert_logical(lower.tail)

  # Dispatch to S7 core computation
  result <- switch(tolower(type),
    "dop" = .compute_cdf_dop(object, q),
    "mc" = .compute_cdf_mc(object, q, n.mc),
    "all" = list(
      dop = .compute_cdf_dop(object, q),
      mc = .compute_cdf_mc(object, q, n.mc)
    )
  )

  # Handle upper tail if requested
  if (!lower.tail) {
    if (type == "all") {
      result$dop$p <- 1 - result$dop$p
      result$mc$p <- 1 - result$mc$p
    } else {
      result$p <- 1 - result$p
    }
  }

  # Return probability value (or list for "all")
  if (type == "all") {
    return(result)
  } else {
    return(result$p)
  }
}

#' @export
S7::method(dist_quantile, ProductNormal) <- function(object, p, type = "dop", n.mc = 1e5, lower.tail = TRUE, ...) {
  checkmate::assert_numeric(p, lower = 0, upper = 1)
  type <- tolower(type)  # Handle case insensitivity
  type <- match.arg(type, c("dop", "mc", "all"))
  checkmate::assert_count(n.mc, positive = TRUE)
  checkmate::assert_logical(lower.tail)

  # Dispatch to S7 core computation
  result <- switch(tolower(type),
    "dop" = .compute_quantile_dop(object, p, lower.tail),
    "mc" = .compute_quantile_mc(object, p, n.mc, lower.tail),
    "all" = list(
      dop = .compute_quantile_dop(object, p, lower.tail),
      mc = .compute_quantile_mc(object, p, n.mc, lower.tail)
    )
  )

  # Return quantile value (or list for "all")
  if (type == "all") {
    return(result)
  } else {
    return(result$q)
  }
}

#' @export
S7::method(ci, ProductNormal) <- function(mu, level = 0.95, type = "dop", n.mc = 1e5, ...) {
  object <- mu # S7 method signature requires 'mu' as first arg
  checkmate::assert_number(level, lower = 0, upper = 1)
  type <- tolower(type)  # Handle case insensitivity
  type <- match.arg(type, c("dop", "mc", "asymp", "all", "prodclin"))
  checkmate::assert_count(n.mc, positive = TRUE)

  alpha <- 1 - level

  # Map prodclin to dop for backward compatibility
  if (type == "prodclin") type <- "dop"

  # Dispatch to S7 core computation
  result <- switch(type,
    "dop" = .compute_ci_dop(object, alpha),
    "mc" = .compute_ci_mc(object, alpha, n.mc),
    "asymp" = .compute_ci_asymp(object, alpha),
    "all" = list(
      dop = .compute_ci_dop(object, alpha),
      mc = .compute_ci_mc(object, alpha, n.mc),
      asymp = .compute_ci_asymp(object, alpha)
    )
  )

  return(result)
}

#' @export
S7::method(print, ProductNormal) <- function(x, ...) {
  cat("ProductNormal Distribution\n")
  cat("Number of variables:", length(x@mu), "\n")
  cat("Means:", paste(round(x@mu, 4), collapse = ", "), "\n")
  if (length(x@mu) <= 3) {
    cat("Covariance matrix:\n")
    base::print.default(x@Sigma)
  } else {
    cat("Covariance matrix: ", nrow(x@Sigma), "x", ncol(x@Sigma), "\n")
  }
  invisible(x)
}

#' @export
S7::method(summary, ProductNormal) <- function(object, ...) {
  cat("ProductNormal Distribution Summary\n")
  cat("==================================\n")
  cat("Number of variables:", length(object@mu), "\n\n")

  cat("Means:\n")
  base::print.default(data.frame(Variable = paste0("V", seq_along(object@mu)),
                         Mean = object@mu))

  cat("\nStandard Deviations:\n")
  sds <- sqrt(diag(object@Sigma))
  base::print.default(data.frame(Variable = paste0("V", seq_along(sds)),
                         SD = sds))

  cat("\nCorrelation Matrix:\n")
  cor_mat <- cov2cor(object@Sigma)
  base::print.default(round(cor_mat, 4))

  invisible(object)
}

#' @export
S7::method(show, ProductNormal) <- function(object) {
  print(object)
}
