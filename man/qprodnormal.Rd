\name{qprodnormal}
\alias{qprodnormal}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Quantile for the Distribution of Product of Two Normal Variables}
\description{
  Generates quantiles for the distribution of product of two normal
  random variables
  
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
qprodnormal(p, mu.x, mu.y, se.x, se.y, rho=0, lower.tail=TRUE,
type="dop", n.mc=1e5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{ probability
%%     ~~Describe \code{p} here~~
}
  \item{mu.x}{ mean of \eqn{x}
  }
  \item{mu.y}{ mean of \eqn{y}
  }
  \item{se.x}{ standard error (deviation) of \eqn{x}
  }
  \item{se.y}{ standard error (deviation) of \eqn{y}
  }
  \item{rho}{ correlation between \eqn{x} and \eqn{y}, where -1 <
  \code{rho} < 1. The default value is 0.
  }
\item{lower.tail}{logical; if \code{TRUE} (default), the probability is
  \eqn{P[X*Y < q]}; otherwise, \eqn{P[X*Y > q]}
}
  \item{type}{method used to compute \eqn{P[X*Y < q]}. It takes on
    the values \code{"dop"} (default), \code{"MC"}, or \code{"all"}.
}
  \item{n.mc}{when \code{type="MC"}, \code{n.mc} determines the sample
  size of Monte Carlo method. The default sample size is 1E5.
}
}
\details{
  This function returns a quantile and the associated error (accuracy) corresponding
  the requested percentile (probability) \code{p} of the
  distribution of product of mediated effect (product of two normal random
  variables). To
  obtain a quantile using a specific method, the argument \code{type}
  should be specified. The default method is \code{type="dop"}, which uses the method described
  by Meeker and Escobar (1994) to evaluate the CDF of the distribution
  of product of two normal variables. \code{type="MC"} uses the Monte
  Carlo approach (Tofighi & MacKinnon, 2011). \code{type="all"} prints
  quantiles using all three options. For the method \code{type="dop"}, the
  error is the modulus of absolute error for the numerical
  integration (for more information see Meeker and Escobar, 1994). For
  \code{type="MC"}, the error refers to the Monte Carlo error.
  %%  ~~ If necessary, more details than the description above ~~

  
}
\value{
  An object of the type \code{\link{list}} that contains the
  following values:
  \item{q}{quantile corresponding to probability \code{p}}

  \item{error}{estimate of the absolute error}
}
\references{
MacKinnon, D. P., Fritz, M. S., Williams, J., and Lockwood, C. M. (2007).
Distribution of the product confidence limits for the indirect effect:
Program PRODCLIN. \emph{Behavior Research Methods}, \bold{39},
384--389.
    
Meeker, W. and Escobar, L. (1994). An algorithm to compute the CDF of
  the product of two normal random variables. \emph{Communications in
    Statistics: Simulation and Computation}, \bold{23}, 271--280.

 Tofighi, D. and MacKinnon, D. P. (2011). RMediation: An R package for
  mediation analysis confidence intervals. \emph{Behavior Research
    Methods}, \bold{43}, 692--700. doi:10.3758/s13428-011-0076-x
}
  \author{
Davood Tofighi \email{dtofighi@unm.edu} and
  David P. MacKinnon \email{davidpm@asu.edu}

}

\seealso{
  \code{\link{medci}}
  \code{\link{pprodnormal}}
  \code{\link{RMediation-package}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##lower tail
qprodnormal(p=.1, mu.x=.5, mu.y=.3, se.x=1, se.y=1, rho=0, lower.tail =
TRUE, type="all")
##upper tail
qprodnormal(p=.1, mu.x=.5, mu.y=.3, se.x=1, se.y=1, rho=0, lower.tail =
FALSE, type="all")
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{distribution }
\keyword{regression }% __ONLY ONE__ keyword per line
\concept{mediation}
\concept{mediated effect}
\concept{distribution of mediated effect}
\concept{distribution of product}
\concept{prodclin}