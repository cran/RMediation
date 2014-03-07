\name{pprodnormal}
\alias{pprodnormal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Percentile for the Distribution of Product of Two Normal Variables
}
\description{
    Generates percentiles (100 based quantiles) for the distribution of product of two normal random variables and the mediated effect
    %%  ~~ A concise (1-5 lines) description of what the function does. ~~

    
}
\usage{
pprodnormal(q, mu.x, mu.y, se.x=1, se.y=1, rho = 0, lower.tail=TRUE,
type="prodclin", n.mc=1e5)
}

\arguments{
  \item{q}{ quantile or value of the product
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
    the values \code{"prodclin"} (default), \code{"DOP"}, \code{"MC"}, or \code{"all"}.
  }
\item{n.mc}{ when \code{type="MC"}, \code{n.mc} determines the sample
  size for the Monte Carlo method. The default sample size is 1E5.
}
}
\details{
  This function returns the percentile (probability) and the associated error for the distribution of product of
  mediated effect (two normal random variables). To obtain a percentile using a
  specific method, the argument \code{type} should be specified. The
  default method is \code{type="prodclin"}, which uses the PRODCLIN
  program described by MacKinnon et al. (2007). \code{type="DOP"}
  uses the code we wrote in \R. Note that both \code{type="prodclin"}
  and \code{type="DOP"} use the method described by Meeker and Escobar
  (1994) to evaluate the CDF of the distribution of product of two
  normal random variables. \code{type="MC"} uses the Monte Carlo
  approach (Tofighi & MacKinnon, 2011). \code{type="all"} prints percentiles using all three
  options. For methods \code{type="prodclin"} and \code{type="DOP"}, the
  error is the modulus of absolute error for the numerical
  integration (for more information see Meeker and Escobar, 1994). For
  \code{type="MC"}, the error refers to the Monte Carlo error.
  %%  ~~ If necessary, more details than the description above ~~

  
}
\value{
   An object of the type \code{\link{list}} that contains the
  following values:
  \item{p}{probability (percentile) corresponding to quantile \code{q}}

  \item{error}{estimate of the absolute error}
}
\references{
  MacKinnon, D. P., Fritz, M. S., Williams, J., and Lockwood, C. M. (2007). Distribution of the product confidence limits for the indirect effect: Program PRODCLIN. \emph{Behavior Research Methods}, \bold{39}, 384--389.
  
  Meeker, W. and Escobar, L. (1994). An algorithm to compute the CDF of
  the product of two normal random variables. \emph{Communications in
    Statistics: Simulation and Computation}, \bold{23}, 271--280.

  Tofighi, D. and MacKinnon, D. P. (2011). RMediation: An R package for
  mediation analysis confidence intervals. \emph{Behavior Research
    Methods}, \bold{43}, 692--700. doi:10.3758/s13428-011-0076-x
}
\author{
Davood Tofighi \email{dtofighi@psych.gatech.edu} and
  David P. MacKinnon \email{davidpm@asu.edu}
 
}
\note{
  %%  ~~further notes~~
    The PRODCLIN programs may be downloaded from
   \url{http://www.public.asu.edu/~davidpm/ripl/Prodclin/}. A web application of the RMediation program is available from \url{http://amp.gatech.edu/RMediation}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{medci}}
  \code{\link{qprodnormal}}
  \code{\link{RMediation-package}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
pprodnormal(q=0, mu.x=.5, mu.y=.3, se.x=1, se.y=1, rho= 0, type="all")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution }
\keyword{regression}% __ONLY ONE__ keyword per line
\concept{mediation}
\concept{mediated effect}
\concept{distribution of mediated effect}
\concept{distribution of product}
\concept{prodclin}