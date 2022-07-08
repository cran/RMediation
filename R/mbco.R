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
#' @param optim Choose optimizer available in OpenMx. The default optimizer is "SLSQP". Other optimizer choices are available. See \link{mxOption} for more details.
#' @param precision Functional precision. The default value is set to 1e-9. See \link{mxOption} for more details.
#' @return A \link{list} that contains \item{chisq}{asymptotic chi-squared test statistic value} \item{\code{df}}{chi-squared df} \item{p}{chi-squared p-value computed based on the method specified by the argument \code{type}}
#'@author Davood Tofighi \email{dtofighi@@gmail.com}
#'@export
#' @references  Tofighi, D., & Kelley, K. (2020). Indirect effects in sequential mediation models: Evaluating methods for hypothesis testing and confidence interval formation. \emph{Multivariate Behavioral Research}, \bold{55}, 188–210. \doi{https://doi.org/10.1080/00273171.2019.1618545}
#'
#'  Tofighi, D. (2020). Bootstrap Model-Based Constrained Optimization Tests of Indirect Effects. \emph{Frontiers in Psychology}, \bold{10}, 2989. \doi{https://doi.org/10.3389/fpsyg.2019.02989}

#'@examples
#' data(memory_exp)
#' memory_exp$x <- as.numeric(memory_exp$x)-1 # manually creating dummy codes
#' endVar <- c('x', 'repetition', 'imagery', 'recall')
#' manifests <- c('x', 'repetition', 'imagery', 'recall')
#'full_model <- mxModel(
#'  "memory_example",
#'  type = "RAM",
#'  manifestVars = manifests,
#'  mxPath(
#'    from = "x",
#'    to = endVar,
#'    arrows = 1,
#'    free = TRUE,
#'    values = .2,
#'    labels = c("a1", "a2", "cp")
#'  ),
#'  mxPath(
#'    from = 'repetition',
#'   to = 'recall',
#'   arrows = 1,
#'    free = TRUE,
#'    values = .2,
#'    labels = 'b1'
#'  ),
#'  mxPath(
#'    from = 'imagery',
#'  to = 'recall',
#'  arrows = 1,
#'  free = TRUE,
#'  values = .2,
#'  labels = "b2"
#'),
#'mxPath(
#'  from = manifests,
#'  arrows = 2,
#'  free = TRUE,
#'  values = .8
#'),
#'mxPath(
#'  from = "one",
#'  to = endVar,
#'  arrows = 1,
#'  free = TRUE,
#'  values = .1
#'),
#'mxAlgebra(a1 * b1, name = "ind1"),
#'mxAlgebra(a2 * b2, name = "ind2"),
#'mxCI("ind1", type = "both"),
#'mxCI("ind2", type = "both"),
#'mxData(observed = memory_exp, type = "raw")
#')
#' ## Reduced  Model for indirect effect: a1*b1
#'null_model1 <- mxModel(
#'model= full_model,
#'name = "Null Model 1",
#'mxConstraint(ind1 == 0, name = "ind1_eq0_constr")
#')
#' full_model <- mxTryHard(full_model, checkHess=FALSE, silent = TRUE )
#' null_model1 <- mxTryHard(null_model1, checkHess=FALSE, silent = TRUE )
#' mbco(null_model1,full_model)


mbco <- function(h0 = NULL,
                 h1 = NULL,
                 R = 10L,
                 type = "asymp",
                 alpha = .05,
                 checkHess = "No",
                 checkSE = "No",
                 optim = "SLSQP",
                 precision = 1e-9) {
  if (missing(h0))
    stop("'h0' argument be a MxModel object")

  if (missing(h1))
    stop("'h1' argument be a MxModel object")

  if (!all(sapply(c(h0, h1), is, "MxModel")))
    stop("The 'h0' and 'h1' argument must be MxModel objects")

  type <-
    match.arg(type, c("asymp", "parametric", "semi")) #checks if one of the types of partially matches
  optim <- match.arg(optim, c("NPSOL", "CSOLNP", "SLSQP"))

  if (!OpenMx::imxHasNPSOL())
    optim <- 'SLSQP' #if NPSOL is not available, use SLSQP

  res <-
    if (type == 'asymp')
      #Asymptotic MBCO LRT
      mbco_asymp(h0 = h0, h1 = h1, alpha = alpha)
  else if (type == 'parametric')
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
  else if (type == 'semi')
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

  return(res)
}
