\name{RMediation-package}
\alias{RMediation-package}
\docType{package}
\title{
RMediation: An R Package for Mediation Analysis Confidence Intervals
}
\description{
\emph{RMediation} package provides functions to compute confidence
intervals (CIs) for a well--defined nonlinear function of the model parameters (e.g., product of k coefficients) in single--level and multilevel structural equation models.
}
\details{
\tabular{ll}{
Package: \tab RMediation\cr
Type: \tab Package\cr
Version: \tab 1.1.3\cr
Date: \tab 2014-3-6\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
%~~ An overview of how to use the package, including the most important ~~
%~~ functions ~~
\code{\link{medci}} produces a CI for the product of two normal random variables using three methods: the distribution of the product of coefficients, Monte Carlo, and asymptotic normal theory with the multivariate-delta standard error (Asymptotic-Delta) method. 
\code{\link{pprodnormal}} produces percentiles for the distribution of product of two normal random variables. 
\code{\link{qprodnormal}} generates quantiles for the distribution of product of two normal random variables.
\code{\link{ci}} produces a CI for a well--defined nonlinear function of the model parameters in single--level and multilevel structural equation models using the Monte Carlo and Asymptotic-Delta method.
}
\author{
  Davood Tofighi \email{dtofighi@psych.gatech.edu} and
  David P. MacKinnon \email{davidpm@asu.edu}
  
Maintainer: Davood Tofighi <dtofighi@psych.gatech.edu>
%~~ The author and/or maintainer of the package ~~
}
\references{
MacKinnon, D. P., Fritz, M. S., Williams, J., and Lockwood, C. M. (2007). Distribution of the product confidence limits for the indirect effect: Program PRODCLIN. \emph{Behavior Research Methods}, \bold{39}, 384--389. 
  
Meeker, W. and Escobar, L. (1994). An algorithm to compute the CDF of the product of two normal random variables. \emph{Communications in Statistics: Simulation and Computation}, \bold{23}, 271--280.

Tofighi, D. and MacKinnon, D. P. (2011). RMediation: An R package for mediation analysis confidence intervals. \emph{Behavior Research Methods}, \bold{43}, 692--700. doi:10.3758/s13428-011-0076-x
%~~ Literature or other references for background information ~~
}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
%~~ the R documentation directory ~~
\keyword{distribution }
\keyword{regression}
\seealso{
  \code{\link{qprodnormal}}
  \code{\link{pprodnormal}}
  \code{\link{medci}}
  \code{\link{ci}}
%objects to See Also as \code{\link{help}}
}
\examples{
%~~ simple examples of the most important functions ~~
medci(mu.x=.2,mu.y=.4,se.x=.1,se.y=.05,rho=0,alpha=.05)
pprodnormal(q=.4, mu.x=.5, mu.y=.3, se.x=.03, se.y=.08, rho= 0)
qprodnormal(p=.1, mu.x=.5, mu.y=.3, se.x=.03, se.y=.8, rho=0)
ci(mu=c(b1=0,b2=0),Sigma=c(1,2,10), quant=~b1*b2)
ci(mu=c(b1=1,b2=.7,b3=.6, b4= .45), Sigma=c(.05,0,0,0,.05,0,0, .03, 0, .03),
quant=~b1*b2*b3*b4, type="all", plot=TRUE, plotCI=TRUE)
}

\note{
  The PRODCLIN programs may be downloaded from
   \url{http://www.public.asu.edu/~davidpm/ripl/Prodclin/}. A web application of the RMediation program is available from \url{http://amp.gatech.edu/RMediation}. 
}
