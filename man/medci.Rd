\name{medci}
\alias{medci}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Confidence Interval for the Mediated Effect}
\description{
   Produces confidence intervals for the mediated effect and the product of
  two normal random variables.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~

  
}
\usage{
medci(mu.x, mu.y, se.x, se.y, rho = 0, alpha = 0.05, type = "dop",
plot=FALSE, plotCI=FALSE, n.mc = 1e+05, ...)
}

\arguments{
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
  \item{alpha}{ significance level for the confidence interval. The default value is .05.
  } 
  \item{type}{method used to compute confidence interval. It takes on
    the values \code{"dop"} (default),
    \code{"MC"}, \code{"asymp"} or \code{"all"}.
  }
  \item{plot}{ when \code{TRUE}, plots the distribution of \code{n.mc} data points
  from the distribution of product of two normal random variables using the density estimates
  provided by the function \code{\link{density}}. The default value is \code{FALSE}.
}
\item{plotCI}{ when \code{TRUE}, overlays a confidence interval with error
  bars on the plot for the mediated effect. Note that to obtain the CI
  plot, one must also specify \code{plot="TRUE"}. The default value is \code{FALSE}.
  }
  \item{n.mc}{ when \code{type="MC"}, \code{n.mc} determines the sample
    size for the Monte Carlo method. The default sample size is 1E5.
  }
  \item{\dots}{additional arguments to be passed on to the function.
  }
}
\details{
  This function returns a (\eqn{1-\alpha})\% confidence interval for the
  mediated effect (product of two normal random variables). To obtain a confidence interval
  using a specific method, the argument \code{type} should be
  specified. The default is \code{type="dop"}, which uses the code we wrote in \R to
  implement the distribution of product of the coefficients method described
  by Meeker and Escobar (1994) to evaluate the CDF of the distribution
  of product. \code{type="MC"} uses the Monte Carlo approach to compute
  the confidence interval (Tofighi & MacKinnon, 2011). \code{type="asymp"} produces the asymptotic normal confidence interval. Note that except for the Monte Carlo method, the standard error for the indirect effect is based on the analytical results by Craig (1936):
  \deqn{\sqrt(se.y^2 \mu.x^2+se.x^2 \mu.y^2+2 \mu.x \mu.y \rho se.x se.y+ se.x^2 se.y^2+se.x^2 se.y^2 \rho^2) } 
  In addition, the estimate of indirect effect is \eqn{\mu.x\mu.y +\sigma.xy }; \code{type="all"} prints confidence intervals using all four options.
  %%  ~~ If necessary, more details than the description above ~~
  
}
\value{
A vector of lower confidence limit and upper confidence limit.
When \code{type} is \code{"prodclin"} (default), \code{"DOP"}, \code{"MC"} or \code{"asymp"}, \code{medci} returns a \link{list} that contains:
\item{(\eqn{1-\alpha})\% CI}{a vector of lower and upper
confidence limits,} 
\item{Estimate}{a point estimate of the
quantity of interest,} 
\item{SE}{standard error of the
quantity of interest,} 
\item{MC Error}{When \code{type="MC"}, error of the Monte Carlo estimate.} 
Note that when \code{type="all"}, \code{medci} returns a \link{list} of \emph{four}
objects, each of which a \link{list} that contains the
results produced by each method as described above.
}
\references{
  Craig, C. C. (1936). On the frequency function of \eqn{xy}. \emph{The Annals of Mathematical Statistics}, \bold{7}, 1--15.
  
  MacKinnon, D. P., Fritz, M. S., Williams, J., and Lockwood, C. M. (2007). Distribution of the product confidence limits for the indirect effect: Program PRODCLIN. \emph{Behavior
    Research Methods}, \bold{39}, 384--389. 

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
\note{
  The PRODCLIN programs may be downloaded from
   \url{http://www.public.asu.edu/~davidpm/ripl/Prodclin/}. A web application of the RMediation program is available from \url{http://amp.gatech.edu/RMediation}. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{qprodnormal}}
  \code{\link{pprodnormal}}
  \code{\link{ci}}
  \code{\link{RMediation-package}}
%objects to See Also as \code{\link{help}}
}
\examples{
##produces CI using PRODCLIN and density plot of distribution of xy
(res <- medci(mu.x=.2, mu.y=.4, se.x=1, se.y=1, rho=0, alpha=.05,
  type="prodclin", plot=TRUE, plotCI=TRUE) )
  
  ## To get a vector of CI estimates
  res[[1]]
  ## To get the point estimate of the indirect effect
    res[["Estimate"]] # Estimate
  ## To get the SE of the indirect effect
    res[["SE"]] # SE  

}


\keyword{distribution }
\keyword{regression}% __ONLY ONE__ keyword per line
\concept{mediation}
\concept{mediated effect}
\concept{distribution of mediated effect}
\concept{distribution of product}
