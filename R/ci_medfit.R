# ---- internal name-based covariance resolvers --------------------------------

#' Resolve path parameter labels to indices in a medfit object
#'
#' Locates each requested path label by NAME in the covariance matrix
#' (\code{mu@vcov}), falling back to \code{names(mu@estimates)} when the
#' covariance matrix carries no dimnames. There is no positional or
#' value-based matching: if any label cannot be resolved by name the function
#' stops with an informative error. This guarantees that path parameters are
#' never silently mismatched or assumed independent.
#'
#' @param mu A medfit \code{MediationData} or \code{SerialMediationData} object.
#' @param path_labels Ordered character vector of parameter labels to resolve
#'   (e.g. \code{c("a", "b")} or \code{c("a", "d1", "b")}).
#'
#' @return Integer index vector in the same order as \code{path_labels}.
#' @keywords internal
#' @noRd
.resolve_path_indices <- function(mu, path_labels) {
  vcov_mat <- mu@vcov
  estimates <- mu@estimates

  # Prefer the covariance matrix's own labels; fall back to estimate names.
  lookup <- rownames(vcov_mat)
  if (is.null(lookup)) {
    lookup <- names(estimates)
  }

  if (is.null(lookup)) {
    stop(
      "Cannot resolve path parameters by name: the covariance matrix has no ",
      "dimnames and the estimates are unnamed. medfit must supply named ",
      "estimates/vcov so that path labels (",
      paste(path_labels, collapse = ", "),
      ") can be located.",
      call. = FALSE
    )
  }

  idx <- match(path_labels, lookup)
  if (anyNA(idx)) {
    missing_labels <- path_labels[is.na(idx)]
    stop(
      "Could not resolve path label(s): ",
      paste(missing_labels, collapse = ", "),
      ". Available names: ",
      paste(lookup, collapse = ", "),
      ". medfit must supply named estimates/vcov whose names include the ",
      "required path labels.",
      call. = FALSE
    )
  }

  as.integer(idx)
}

#' Extract the named covariance sub-matrix for a set of path parameters
#'
#' Returns the full square sub-matrix of \code{mu@vcov} for the requested
#' path labels, preserving off-diagonal covariances. This is correct whether
#' the off-diagonals are zero or non-zero.
#'
#' @inheritParams .resolve_path_indices
#'
#' @return A square numeric matrix of dimension
#'   \code{length(path_labels)} by \code{length(path_labels)}.
#' @keywords internal
#' @noRd
.extract_path_vcov <- function(mu, path_labels) {
  idx <- .resolve_path_indices(mu, path_labels)
  mu@vcov[idx, idx, drop = FALSE]
}

# ---- public ci methods -------------------------------------------------------

#' Confidence Interval for MediationData Objects
#'
#' @description
#' Computes confidence intervals for the indirect effect from a medfit
#' MediationData object using RMediation's methods (DOP, Monte Carlo, etc.).
#'
#' @param mu A MediationData object from the medfit package
#' @param level Confidence level (default 0.95 for 95% CI)
#' @param type Type of CI method: "dop" (Distribution of Product),
#'   "MC" (Monte Carlo), or "asymp" (asymptotic normal)
#' @param n.mc Number of Monte Carlo samples (for type = "MC")
#' @param ... Additional arguments passed to underlying methods
#'
#' @return A list with components:
#'   \item{CI}{The confidence interval (lower, upper)}
#'   \item{Estimate}{Point estimate of indirect effect (a*b)}
#'   \item{SE}{Standard error of indirect effect}
#'   \item{type}{Method used for CI computation}
#'
#' @details
#' This method extracts the a and b path coefficients from the MediationData
#' object, along with their standard errors and covariance, and computes
#' confidence intervals using RMediation's methods.
#'
#' ## Method Options
#'
#' - **"dop"**: Distribution of Product method. Uses the exact or approximate
#'   distribution of the product of two normal random variables. Recommended
#'   for most applications.
#'
#' - **"MC"**: Monte Carlo simulation. Samples from the joint distribution
#'   of a and b to estimate the CI. Use `n.mc` to control precision.
#'
#' - **"asymp"**: Asymptotic normal approximation using the delta method.
#'   Fast but may be inaccurate for small samples or non-normal distributions.
#'
#' @examples
#' \dontrun{
#' library(medfit)
#' library(RMediation)
#'
#' # Fit mediation models
#' fit_m <- lm(M ~ X + C, data = mydata)
#' fit_y <- lm(Y ~ X + M + C, data = mydata)
#'
#' # Extract mediation structure
#' med_data <- extract_mediation(fit_m, model_y = fit_y,
#'                                treatment = "X", mediator = "M")
#'
#' # Compute CI using Distribution of Product
#' ci(med_data, type = "dop")
#'
#' # Compute CI using Monte Carlo
#' ci(med_data, type = "MC", n.mc = 10000)
#' }
#'
#' @seealso
#' \code{\link{ci}} for the generic function,
#' \code{\link[medfit]{MediationData}} for the input class,
#' \code{\link{ProductNormal}} for the underlying distribution class
#'
#' @export
ci_mediation_data <- function(mu, level = 0.95, type = "dop",
                              n.mc = 1e5, ...) {
  if (!requireNamespace("medfit", quietly = TRUE)) {
    stop("Package 'medfit' is required for this method.", call. = FALSE)
  }

  # Locate the a- and b-paths by name and build the full 2x2 covariance.
  Sigma_2x2 <- .extract_path_vcov(mu, c("a", "b"))

  # ProductNormal's validator requires a plain square numeric matrix; strip
  # dimnames so the named sub-matrix is accepted unconditionally.
  dimnames(Sigma_2x2) <- NULL

  pn <- ProductNormal(mu = c(mu@a_path, mu@b_path), Sigma = Sigma_2x2)
  ci(pn, level = level, type = type, n.mc = n.mc, ...)
}

#' @rdname ci_mediation_data
#' @export
ci_serial_mediation_data <- function(mu, level = 0.95, type = "MC",
                                     n.mc = 1e5, ...) {
  if (!requireNamespace("medfit", quietly = TRUE)) {
    stop("Package 'medfit' is required for this method.", call. = FALSE)
  }

  checkmate::assert_count(n.mc, positive = TRUE)
  checkmate::assert_number(level, lower = 0, upper = 1)

  a_path <- mu@a_path
  d_path <- mu@d_path
  b_path <- mu@b_path
  all_paths <- c(a_path, d_path, b_path)

  # Documented label contract: the serial d-path parameters are addressed
  # purely by NAME, using the literal labels "d1", "d2", ..., "dk" in order
  # (k = length(mu@d_path)). There is NO positional or value-matching
  # fallback (spec sections 3/4.1). medfit's serial extractor MUST emit
  # named estimates/vcov whose names include "a", "b", and these "d<i>"
  # labels (cross-reference: medfit blocker spec Open Question on d-labels).
  # If any required label is absent, .resolve_path_indices() stops with an
  # informative error.
  d_labels <- .serial_d_labels(mu)

  Sigma <- .extract_path_vcov(mu, c("a", d_labels, "b"))
  dimnames(Sigma) <- NULL

  # Defensive guard against a mean/covariance order mismatch: the mean vector
  # passed to mvrnorm must align row-for-row with Sigma.
  stopifnot(length(all_paths) == nrow(Sigma))
  draws <- MASS::mvrnorm(n = n.mc, mu = all_paths, Sigma = Sigma)
  products <- apply(draws, 1, prod)
  alpha <- 1 - level
  ci_bounds <- stats::quantile(products, probs = c(alpha / 2, 1 - alpha / 2))

  # Backward-compatible return shape (spec section 4.4): preserve the names
  # and components of the original ci_serial_mediation_data() output.
  list(
    CI = c(lower = unname(ci_bounds[1]), upper = unname(ci_bounds[2])),
    Estimate = prod(all_paths),
    SE = stats::sd(products),
    type = "MC (serial mediation)",
    level = level,
    n.mc = n.mc,
    k = length(d_path)
  )
}

#' Derive the d-path labels for a serial mediation object
#'
#' Returns the literal, NAME-ONLY label contract for the serial d-paths:
#' \code{paste0("d", seq_along(mu@d_path))} (i.e. "d1", "d2", ...). These are
#' resolved purely by name downstream via \code{.resolve_path_indices()}; there
#' is no positional or value-matching fallback (spec sections 3/4.1). medfit's
#' serial extractor must emit estimates/vcov carrying these names.
#'
#' @inheritParams .resolve_path_indices
#' @return Character vector of d-path labels of length \code{length(mu@d_path)}.
#' @keywords internal
#' @noRd
.serial_d_labels <- function(mu) {
  paste0("d", seq_along(mu@d_path))
}

# Dynamic method registration in .onLoad (called from zzz.R)
.register_medfit_methods <- function() {
  if (requireNamespace("medfit", quietly = TRUE)) {
    tryCatch({
      # Register ci method for MediationData
      MediationData_class <- medfit::MediationData
      S7::method(ci, MediationData_class) <- ci_mediation_data

      # Register ci method for SerialMediationData
      SerialMediationData_class <- medfit::SerialMediationData
      S7::method(ci, SerialMediationData_class) <- ci_serial_mediation_data
    }, error = function(e) {
      # Make registration failure visible (but non-fatal): the functions remain
      # available as regular exported functions, but a broken S7 registration
      # should be diagnosable rather than silently swallowed.
      packageStartupMessage(
        "RMediation: failed to register medfit S7 ci methods: ",
        conditionMessage(e)
      )
    })
  }
}
