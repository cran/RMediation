#' @importFrom S7 method
#' @importFrom lavaan parameterEstimates
NULL

# Helper function to parse product strings
parse_product_string <- function(expr_str) {
  expr_str <- gsub("\\s+", "", expr_str)
  parts <- strsplit(expr_str, "\\*")[[1]]
  if (any(grepl("[^a-zA-Z0-9_.]", parts))) {
    return(NULL)
  }
  return(parts)
}

#' @export
#' @export
S7::method(ci, S7::class_numeric) <- function(mu, Sigma, quant, alpha = 0.05, type = "MC", ...) {
  # Dispatch to legacy .ci_core
  # mu maps to mu
  checkmate::assert_numeric(mu)
  checkmate::assert(
    checkmate::check_matrix(Sigma, mode = "numeric"),
    checkmate::check_numeric(Sigma)
  )
  checkmate::assert_formula(quant)
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  type <- match.arg(type, c("MC", "asymp", "all"))
  
  # We pass all arguments explicitly to .ci_core
  .ci_core(mu = mu, Sigma = Sigma, quant = quant, alpha = alpha, type = type, ...)
}

#' @export
#' @export
S7::method(ci, S7::class_any) <- function(mu, level = 0.95, type = "dop", n.mc = 1e5, ...) {
  object <- mu # Alias for internal logic
  checkmate::assert_number(level, lower = 0, upper = 1)
  type <- match.arg(type, c("dop", "MC", "asymp", "all", "prodclin"))
  checkmate::assert_count(n.mc, positive = TRUE)
  
  if (!inherits(object, "lavaan")) {
    # If we want to support other classes later (like MxModel), add checks here.
    # For now, if it's not lavaan, we throw an error (or let S7 dispatch fail if we didn't define this).
    # But since we defined it for class_any, we must handle it.
    stop(paste("No method defined for class:", paste(class(object), collapse = ", ")))
  }

  # Lavaan logic
  args <- list(...)
  
  # Check if 'quant' is provided in ...
  if ("quant" %in% names(args)) {
    # Legacy behavior: user provided a specific formula
    if ("alpha" %in% names(args)) {
      alpha <- args[["alpha"]]
      args[["alpha"]] <- NULL
    } else {
      alpha <- 1 - level
    }
    return(do.call(.ci_core, c(list(mu = object, alpha = alpha), args)))
  }

  # New behavior: Auto-detect defined parameters
  est <- lavaan::parameterEstimates(object)
  defined <- est[est$op == ":=", ]
  
  if (nrow(defined) == 0) {
    warning("No defined parameters (:=) found in lavaan object.")
    return(NULL)
  }
  
  results <- list()
  
  # Determine alpha for new behavior
  if ("alpha" %in% names(args)) {
    alpha <- args[["alpha"]]
  } else {
    alpha <- 1 - level
  }
  
  for (i in 1:nrow(defined)) {
    label <- defined$lhs[i]
    rhs <- defined$rhs[i]
    
    vars <- parse_product_string(rhs)
    
    if (!is.null(vars) && length(vars) >= 2) {
      # Product of coefficients
      coefs <- lavaan::coef(object)
      vcov_mat <- lavaan::vcov(object)
      
      if (all(vars %in% names(coefs))) {
        mu_vals <- coefs[vars]
        Sigma_vals <- vcov_mat[vars, vars]
        
        pn <- ProductNormal(mu = as.numeric(mu_vals), Sigma = as.matrix(Sigma_vals))
        
        # Calculate CI using ProductNormal method
        # Note: ci(ProductNormal) returns vector.
        # We pass level/alpha correctly. ProductNormal ci uses level.
        # If user passed alpha, we convert back to level for consistency with S7 generic?
        # Or just pass level if we have it.
        # The generic has 'level'.
        res_ci <- ci(pn, level = 1 - alpha, type = type, n.mc = n.mc)
        results[[label]] <- res_ci
      } else {
        # Fallback to general MC via .ci_core
        f_str <- paste("~", rhs)
        quant <- as.formula(f_str)
        
        res <- .ci_core(mu = object, quant = quant, alpha = alpha, type = "MC", n.mc = n.mc, plot = FALSE)
        results[[label]] <- res
      }
    } else {
      # Not a simple product, use general MC
      f_str <- paste("~", rhs)
      quant <- as.formula(f_str)
      res <- .ci_core(mu = object, quant = quant, alpha = alpha, type = "MC", n.mc = n.mc, plot = FALSE)
      results[[label]] <- res
    }
  }
  
  return(results)
}

# Note: removed quantile methods for class_numeric since dist_quantile is for distributions only
# Data quantiles should use stats::quantile directly

#' @export
S7::method(cdf, S7::class_numeric) <- function(object, q, ...) {
  # Empirical CDF for numeric vectors
  stats::ecdf(object)(q)
}
