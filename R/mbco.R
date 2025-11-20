#' Model-based Constrained Optimization (MBCO) Chi-squared Test
#'
#' This function computes asymptotic MBCO chi-squared test for a smooth function of model parameters including a function of indirect effects.
#'
#' @param h0 An \code{OpenMx} model estimated under a null hypothesis, which is a more constrained model
#' @param h1 An \code{OpenMx} model estimated under an alternative hypothesis, which is a less constrained model. This is usually a model hypothesized by a researcher.
#' @param R The number of bootstrap draws.
#' @param type If 'asymp' (default), the asymptotic MBCO chi-squares test comparing fit of h0 and h1. If 'parametric', the parametric bootstrap MBCO chi-squared test is computed. If 'semi', the semi-parametric MBCO chi-squared is computed.
#' @param alpha Significance level with the default value of .05
#' @param checkHess If 'No' (default), the Hessian matrix would not be calculated.
#' @param checkSE if 'No' (default), the standard errors would not be calculated.
#' @param optim Choose optimizer available in OpenMx. The default optimizer is "SLSQP". Other optimizer choices are available. See \link[OpenMx]{mxOption} for more details.
#' @param precision Functional precision. The default value is set to 1e-9. See \link[OpenMx]{mxOption} for more details.
#' @return A \link{list} that contains \item{chisq}{asymptotic chi-squared test statistic value} \item{\code{df}}{chi-squared df} \item{p}{chi-squared p-value computed based on the method specified by the argument \code{type}}
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#' @import OpenMx
#' @importFrom modelr resample_bootstrap
#' @importFrom checkmate assert_class assert_count assert_number assert_choice
#' @export
#' @examples
#' data(memory_exp)
#' memory_exp$x <- as.numeric(memory_exp$x)-1 # manually creating dummy codes
#' endVar <- c('x', 'repetition', 'imagery', 'recall')
#' manifests <- c('x', 'repetition', 'imagery', 'recall')
#'full_model <- OpenMx::mxModel(
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
#'),
#'OpenMx::mxPath(
#'  from = manifests,
#'  arrows = 2,
#'  free = TRUE,
#'  values = .8
#'),
#'OpenMx::mxPath(
#'  from = "one",
#'  to = endVar,
#'  arrows = 1,
#'  free = TRUE,
#'  values = .1
#'),
#'OpenMx::mxAlgebra(a1 * b1, name = "ind1"),
#'OpenMx::mxAlgebra(a2 * b2, name = "ind2"),
#'OpenMx::mxCI("ind1", type = "both"),
#'OpenMx::mxCI("ind2", type = "both"),
#'OpenMx::mxData(observed = memory_exp, type = "raw")
#')
#' ## Reduced  Model for indirect effect: a1*b1
#'null_model1 <- OpenMx::mxModel(
#'model= full_model,
#'name = "Null Model 1",
#'OpenMx::mxConstraint(ind1 == 0, name = "ind1_eq0_constr")
#')
#' full_model <- OpenMx::mxTryHard(full_model, checkHess=FALSE, silent = TRUE )
#' null_model1 <- OpenMx::mxTryHard(null_model1, checkHess=FALSE, silent = TRUE )
#' mbco(null_model1,full_model)

mbco <- function(
  h0 = NULL,
  h1 = NULL,
  R = 10L,
  type = "asymp",
  alpha = .05,
  checkHess = "No",
  checkSE = "No",
  optim = "SLSQP",
  precision = 1e-9
) {
  # Input validation
  checkmate::assert_class(h0, "MxModel")
  checkmate::assert_class(h1, "MxModel")
  checkmate::assert_count(R, positive = TRUE)
  type <- match.arg(type, c("asymp", "parametric", "semi"))
  checkmate::assert_number(alpha, lower = 0, upper = 1, finite = TRUE)
  checkmate::assert_choice(checkHess, c("Yes", "No"))
  checkmate::assert_choice(checkSE, c("Yes", "No"))
  optim <- match.arg(optim, c("NPSOL", "CSOLNP", "SLSQP"))
  checkmate::assert_number(precision, lower = 0, finite = TRUE)

  if (!OpenMx::imxHasNPSOL()) {
    optim <- "SLSQP"
  } # if NPSOL is not available, use SLSQP

  res <-
    if (type == "asymp") {
      # Asymptotic MBCO LRT
      mbco_asymp(h0 = h0, h1 = h1, alpha = alpha)
    } else if (type == "parametric") {
      # Parametric bootstrap MBCO LRT
      mbco_parametric(
        h0 = h0,
        h1 = h1,
        R = R,
        alpha = alpha,
        checkHess = checkHess,
        checkSE = checkSE,
        optim = optim,
        precision = precision
      )
    } else if (type == "semi") {
      # Semiparametric bootstrap MBCO LRT
      mbco_semi(
        h0 = h0,
        h1 = h1,
        R = R,
        alpha = alpha,
        checkHess = checkHess,
        checkSE = checkSE,
        optim = optim,
        precision = precision
      )
    }

  return(res)
}
