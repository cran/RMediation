#' Percentile for the Distribution of Product of Two Normal Variables
#'
#' Generates percentiles (100 based quantiles) for the distribution of product
#' of two normal random variables and the mediated effect
#'
#' @param q quantile or value of the product
#' @param mu.x  mean of \eqn{x}
#' @param mu.y mean of \eqn{y}
#' @param se.x standard error (deviation) of \eqn{x}
#' @param se.y  standard error (deviation) of \eqn{y}
#' @param rho correlation between \eqn{x} and \eqn{y}, where -1 <\code{rho} < 1.
#'   The default value is 0.
#' @param lower.tail logical; if \code{TRUE} (default), the probability is
#'   \eqn{P[X*Y < q]}; otherwise, \eqn{P[X*Y > q]}
#' @param type method used to compute confidence interval. It takes on the
#'   values \code{"dop"} (default), \code{"MC"}, \code{"asymp"} or \code{"all"}
#' @param n.mc when \code{type="MC"}, \code{n.mc} determines the sample size for
#'   the Monte Carlo method. The default sample size is 1E5.
#' @details    This function returns the percentile (probability) and the
#'   associated error for the distribution of product of mediated effect (two
#'   normal random variables). To obtain a percentile using a specific method,
#'   the argument \code{type} should be specified. The default method is
#'   \code{type="dop"}, which is based on the method described by Meeker and
#'   Escobar (1994) to evaluate the CDF of the distribution of product of two
#'   normal random variables. \code{type="MC"} uses the Monte Carlo approach
#'   (Tofighi & MacKinnon, 2011). \code{type="all"} prints percentiles using all
#'   three options. For the method \code{type="dop"}, the error is the modulus
#'   of absolute error for the numerical integration (for more information see
#'   Meeker and Escobar, 1994). For \code{type="MC"}, the error refers to the
#'   Monte Carlo error.
#' @return   An object of the type \code{\link{list}} that contains the
#'   following values: \item{p}{probability (percentile) corresponding to
#'   quantile \code{q}} \item{error}{estimate of the absolute error}
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#' @references  Tofighi, D. and MacKinnon, D. P. (2011). RMediation: An R
#'   package for mediation analysis confidence intervals. \emph{Behavior
#'   Research Methods}, \bold{43}, 692--700. doi:10.3758/s13428-011-0076-x
#' @seealso \code{\link{medci}} \code{\link{RMediation-package}}
#' @examples
#' pprodnormal(q=0, mu.x=.5, mu.y=.3, se.x=1, se.y=1, rho= 0, type="all")
#' @export


pprodnormal<- function(q, mu.x, mu.y, se.x=1, se.y=1, rho=0, lower.tail=TRUE, type="dop", n.mc=1e5){
  if(!is.numeric(mu.x))
    stop("Argument mu.x must be numeric!")
  if(!is.numeric(mu.y))
    stop("Argument mu.y must be numeric!")
  if(!is.numeric(se.x))
    stop("Argument se.x must be numeric!")
  if(!is.numeric(se.y))
      stop("Argument se.y must be numeric!")
  if(!is.numeric(rho))
      stop("Argument rho  must be numeric!")
  if(rho<=-1 || rho>=1)
    stop("rho must be between -1 and 1!")
  if(!is.numeric(n.mc) || is.null(n.mc))
    n.mc=1e5 # sets n.mc to default

  if (type=="all" || type=="All" || type=="ALL")
    {
        p2 <- pprodnormalMeeker(q, mu.x, mu.y, se.x, se.y, rho, lower.tail)
        ##cat("Monte Carlo method:\n")
        p3 <- pprodnormalMC(q, mu.x, mu.y, se.x, se.y, rho, lower.tail, n.mc)
        res <- list(p2,p3)
        names(res) <- c( "Distribution of Product", "Monte Carlo")
        return(res)
    }
  else if (type=="DOP" || type=="dop")
    {
        ##cat("Meeker method:\n")
        p2 <- pprodnormalMeeker(q, mu.x, mu.y, se.x, se.y, rho, lower.tail)
        return(p2)
    }
  else if (type=="MC" || type=="mc" || type=="Mc")
    {
        ##cat("Monte Carlo method:\n")
        p3 <- pprodnormalMC(q, mu.x, mu.y, se.x, se.y, rho, lower.tail, n.mc)
        return(p3)
    }
  else stop("Wrong type! please specify type=\"all\", \"DOP\", or \"MC\" ")
}
