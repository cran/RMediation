#' @importFrom S7 new_class new_generic
NULL

#' ProductNormal Class
#'
#' Represents the distribution of the product of normal random variables.
#'
#' @param mu Numeric vector of means.
#' @param Sigma Covariance matrix.
#' @export
ProductNormal <- S7::new_class("ProductNormal",
  properties = list(
    mu = S7::class_numeric,
    Sigma = S7::class_numeric
  ),
  validator = function(self) {
    if (length(self@mu) < 2) {
      stop("mu must have at least 2 elements")
    }
    # Check if Sigma is a matrix or vector
    if (is.matrix(self@Sigma)) {
      if (nrow(self@Sigma) != ncol(self@Sigma)) {
        stop("Sigma must be a square matrix")
      }
      if (nrow(self@Sigma) != length(self@mu)) {
        stop("Dimensions of Sigma must match length of mu")
      }
      # Check positive semi-definiteness (simple check)
      eigen_vals <- eigen(self@Sigma, symmetric = TRUE, only.values = TRUE)$values
      if (any(eigen_vals < -1e-8)) {
        stop("Sigma must be positive semi-definite")
      }
    } else {
      # If Sigma is passed as a vector (e.g. just variances/SEs), we might want to enforce it being a matrix
      # For now, let's strictly require a matrix for the class property to be safe,
      # or allow the constructor to handle conversion.
      # The property definition `class_numeric` allows vector or matrix.
      # Let's enforce matrix in validator for consistency.
      if (!is.matrix(self@Sigma)) {
         stop("Sigma must be a matrix")
      }
    }
    NULL
  }
)

# Register S4 for compatibility with base generics
S7::S4_register(ProductNormal)

#' Cumulative Distribution Function
#'
#' Generic function for computing cumulative distribution function.
#'
#' @param object A distribution object.
#' @param ... Additional arguments passed to methods.
#' @export
cdf <- S7::new_generic("cdf", "object")

#' Distribution Quantile Function
#'
#' Compute quantiles for distribution objects. This function computes quantiles
#' for product normal distributions, not data quantiles (use \code{stats::quantile}
#' for data).
#'
#' @param object A distribution object (e.g., ProductNormal).
#' @param ... Additional arguments passed to methods.
#' @export
dist_quantile <- S7::new_generic("dist_quantile", "object")

#' Confidence Interval
#'
#' Generic function for computing confidence intervals.
#'
#' @param mu A distribution object or numeric vector of means.
#' @param ... Additional arguments passed to methods.
#' @export
ci <- S7::new_generic("ci", "mu")

# Note: print, summary, and show generics are NOT exported.
# S7 methods will register directly with base generics to avoid masking.

#' MBCO Result Class
#'
#' A class representing the results of a Model-Based Constrained Optimization (MBCO) test.
#'
#' @param statistic Numeric test statistic value.
#' @param df Numeric degrees of freedom.
#' @param p_value Numeric p-value.
#' @param type Character string indicating the type of test.
#' @export
MBCOResult <- S7::new_class("MBCOResult",
  properties = list(
    statistic = S7::class_numeric,
    df = S7::class_numeric,
    p_value = S7::class_numeric,
    type = S7::class_character
  )
)

# Register S4 for compatibility with base generics
S7::S4_register(MBCOResult)

#' Model-based Constrained Optimization (MBCO) Chi-squared Test
#'
#' This function computes asymptotic MBCO chi-squared test for a smooth function of model parameters including a function of indirect effects.
#'
#' @param h0 An \code{OpenMx} model estimated under a null hypothesis, which is a more constrained model
#' @param h1 An \code{OpenMx} model estimated under an alternative hypothesis, which is a less constrained model. This is usually a model hypothesized by a researcher.
#' @param ... Additional arguments.
#' @return An object of class \code{MBCOResult} containing \item{statistic}{asymptotic chi-squared test statistic value} \item{df}{chi-squared df} \item{p_value}{chi-squared p-value computed based on the method specified by the argument \code{type}} \item{type}{The type of test performed}
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#' @importFrom checkmate assert_class assert_count assert_number assert_choice
#' @export
#' @examples
#' \dontrun{
#' data(memory_exp)
#' memory_exp$x <- as.numeric(memory_exp$x)-1 # manually creating dummy codes
#' endVar <- c('x', 'repetition', 'imagery', 'recall')
#' manifests <- c('x', 'repetition', 'imagery', 'recall')
#' full_model <- OpenMx::mxModel(
#'  "memory_example",
#'  type = "RAM",
#'  manifestVars = manifests,
#'  OpenMx::mxPath(
#'    from = "x",
#'    to = endVar,
#'    arrows = 1,
#'    free = TRUE,
#'    values = .2,
#'    labels = c("a1", "a2", "cp")
#'  ),
#'  OpenMx::mxPath(
#'    from = 'repetition',
#'   to = 'recall',
#'   arrows = 1,
#'    free = TRUE,
#'    values = .2,
#'    labels = 'b1'
#'  ),
#'  OpenMx::mxPath(
#'    from = 'imagery',
#'  to = 'recall',
#'  arrows = 1,
#'  free = TRUE,
#'  values = .2,
#'  labels = "b2"
#' ),
#' OpenMx::mxPath(
#'  from = manifests,
#'  arrows = 2,
#'  free = TRUE,
#'  values = .8
#' ),
#' OpenMx::mxPath(
#'  from = "one",
#'  to = endVar,
#'  arrows = 1,
#'  free = TRUE,
#'  values = .1
#' ),
#' OpenMx::mxAlgebra(a1 * b1, name = "ind1"),
#' OpenMx::mxAlgebra(a2 * b2, name = "ind2"),
#' OpenMx::mxCI("ind1", type = "both"),
#' OpenMx::mxCI("ind2", type = "both"),
#' OpenMx::mxData(observed = memory_exp, type = "raw")
#' )
#' ## Reduced  Model for indirect effect: a1*b1
#' null_model1 <- OpenMx::mxModel(
#' model= full_model,
#' name = "Null Model 1",
#' OpenMx::mxConstraint(ind1 == 0, name = "ind1_eq0_constr")
#' )
#' full_model <- OpenMx::mxTryHard(full_model, checkHess=FALSE, silent = TRUE )
#' null_model1 <- OpenMx::mxTryHard(null_model1, checkHess=FALSE, silent = TRUE )
#' mbco(null_model1,full_model)
#' }
mbco <- S7::new_generic("mbco", c("h0", "h1"))
