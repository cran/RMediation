#' Enhanced validation and utility functions for ProductNormal class
#'
#' @name utils_validation
#' @importFrom S7 method new_class
#' @importFrom checkmate assert check_numeric check_matrix qtest
NULL

#' Additional validation for ProductNormal objects
#'
#' @param object A ProductNormal object
#' @param verbose Whether to show detailed validation messages
#' @return TRUE if valid, throws error if invalid
validate_ProductNormal <- function(object, verbose = FALSE) {
  # Check if it's actually a ProductNormal object
  if (!S7::S7_inherits(object, ProductNormal)) {
    stop("Object is not of class ProductNormal")
  }
  
  # Validate mu vector
  if (length(object@mu) < 2) {
    stop("ProductNormal: mu must have at least 2 elements")
  }
  
  if (!is.numeric(object@mu)) {
    stop("ProductNormal: mu must be numeric")
  }
  
  # Validate Sigma matrix
  if (!is.matrix(object@Sigma)) {
    stop("ProductNormal: Sigma must be a matrix")
  }
  
  if (nrow(object@Sigma) != ncol(object@Sigma)) {
    stop("ProductNormal: Sigma must be a square matrix")
  }
  
  if (nrow(object@Sigma) != length(object@mu)) {
    stop("ProductNormal: Dimensions of Sigma must match length of mu")
  }
  
  # More robust positive semi-definiteness check
  eigen_vals <- tryCatch({
    eigen(object@Sigma, symmetric = TRUE, only.values = TRUE)$values
  }, error = function(e) {
    stop("ProductNormal: Cannot compute eigenvalues of Sigma matrix - is it symmetric?")
  })
  
  # Check for positive semi-definiteness with tolerance
  if (any(eigen_vals < -1e-10)) {
    stop(paste(
      "ProductNormal: Sigma must be positive semi-definite.",
      "Smallest eigenvalue:", min(eigen_vals)
    ))
  }
  
  # Additional check for numerical stability
  if (any(is.na(object@mu)) || any(is.infinite(object@mu))) {
    stop("ProductNormal: mu contains NA or infinite values")
  }
  
  if (any(is.na(object@Sigma)) || any(is.infinite(object@Sigma))) {
    stop("ProductNormal: Sigma contains NA or infinite values")
  }
  
  return(TRUE)
}

#' Enhanced ProductNormal constructor with better validation
#'
#' @param mu Numeric vector of means
#' @param Sigma Covariance matrix
#' @param validate Whether to run additional validation (default: TRUE)
#' @export
ProductNormal2 <- function(mu, Sigma, validate = TRUE) {
  # Basic input validation before creating the object
  assert_that <- function(condition, message) {
    if (!condition) stop(message)
  }
  
  assert_that(is.numeric(mu), "mu must be numeric")
  assert_that(length(mu) >= 2, "mu must have at least 2 elements")
  assert_that(is.matrix(Sigma) || (is.numeric(Sigma) && length(Sigma) > 0), 
              "Sigma must be a matrix or numeric vector")
  
  # Convert vector to matrix if needed (lower triangle format)
  if (!is.matrix(Sigma)) {
    n_params <- length(mu)
    expected_length <- n_params * (n_params + 1) / 2
    if (length(Sigma) == expected_length) {
      # Assume it's a vech (lower triangle) vector
      Sigma <- lavaan::lav_matrix_vech_reverse(Sigma)
    } else {
      stop(paste(
        "Sigma vector has", length(Sigma), "elements,",
        "but for", n_params, "parameters, expected", expected_length,
        "elements for lower triangle matrix."
      ))
    }
  }
  
  # Ensure symmetric
  if (nrow(Sigma) != ncol(Sigma)) {
    stop("Sigma must be a square matrix")
  }
  
  if (nrow(Sigma) != length(mu)) {
    stop("Dimensions of Sigma must match length of mu")
  }
  
  # Create the object with enhanced validation
  obj <- ProductNormal(mu = mu, Sigma = Sigma)
  
  # Additional validation if requested
  if (validate) {
    validate_ProductNormal(obj)
  }
  
  return(obj)
}

#' Method to check if ProductNormal object is properly specified for computation
#' 
#' @param object A ProductNormal object
#' @return TRUE if object is valid for computation methods
is_valid_for_computation <- function(object) {
  if (!S7::S7_inherits(object, ProductNormal)) {
    return(FALSE)
  }
  
  # Check eigenvalues for computational stability
  eigen_vals <- tryCatch({
    eigen(object@Sigma, symmetric = TRUE, only.values = TRUE)$values
  }, error = function(e) {
    return(FALSE)
  })
  
  # Need positive semi-definite for stable computation
  if (any(eigen_vals < -1e-8)) {
    return(FALSE)
  }
  
  # Check for reasonable scale (avoid numerical issues)
  if (any(abs(object@mu) > 1e6) || any(abs(object@Sigma) > 1e6)) {
    warning("ProductNormal: Very large parameter values may cause numerical issues")
  }
  
  return(TRUE)
}

#' Utility function to create ProductNormal from lavaan parameter estimates
#'
#' @param lavaan_model A fitted lavaan model object
#' @param param_names Names of parameters to include in the product (e.g., c("a", "b"))
#' @return A ProductNormal object
ProductNormal_from_lavaan <- function(lavaan_model, param_names) {
  if (!inherits(lavaan_model, "lavaan")) {
    stop("lavaan_model must be a lavaan object")
  }
  
  # Extract parameter estimates
  coef_est <- lavaan::coef(lavaan_model)
  vcov_est <- lavaan::vcov(lavaan_model)
  
  # Check if all requested parameters exist
  missing_params <- setdiff(param_names, names(coef_est))
  if (length(missing_params) > 0) {
    stop(paste("Parameters not found in model:", paste(missing_params, collapse = ", ")))
  }
  
  # Extract relevant coefficients and covariance matrix
  mu_vals <- coef_est[param_names]
  Sigma_vals <- as.matrix(vcov_est[param_names, param_names])
  
  # Validate that the covariance matrix is valid
  if (any(is.na(Sigma_vals)) || any(is.infinite(Sigma_vals))) {
    warning("Covariance matrix contains NA or infinite values. Results may be unreliable.")
  }
  
  return(ProductNormal2(mu = as.numeric(mu_vals), Sigma = Sigma_vals))
}