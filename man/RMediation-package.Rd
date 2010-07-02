\name{RMediation-package}
\alias{RMediation-package}
\docType{package}
\title{
RMediation: An R Package for Mediation Analysis
}
\description{
\emph{RMediation} package provides functions to compute confidence
intervals (CIs), percentiles and quantiles for the mediated effect and the
product of two random normal variables.
}
\details{
\tabular{ll}{
Package: \tab RMediation\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-2-08\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
%~~ An overview of how to use the package, including the most important ~~
%~~ functions ~~
The \emph{RMediation} package provides functions to compute confidence interval (CIs), percentiles,
and quantiles for the mediated effect and the product of two random normal
variables. \code{\link{medci}} produces CIs for the product of two random normal
variables using three methods: the distribution of the product, the Monte Carlo, and
the asymptotic normal method. \code{\link{pprodnormal}} provides percentiles for
the distribution of product of two random normal
variables. \code{\link{qprodnormal}} generates quantiles for the distribution
of product of two random normal variables.
}
\author{
  Davood Tofighi and David P. MacKinnon
  
Maintainer: Davood Tofighi <dtofighi@psych.gatech.edu>
%~~ The author and/or maintainer of the package ~~
}
\references{
    MacKinnon, D. P., Fritz, M. S., Williams, J., and Lockwood, C. M. (2007) Distribution of the product confidence limits for the indirect effect: Program PRODCLIN. \emph{Behavior
  Research Methods}, \bold{39}(3), 384--389. 
  
  Meeker, W. and Escobar, L. (1994) An algorithm to compute the CDF of
  the product of two normal random variables. \emph{Communications in
    Statistics: Simulation and Computation}, \bold{23}(1), 271--280.

   Tofighi, D. and MacKinnon, D. P. (2010). RMediation: An R package for
  mediation analysis. Submitted for publication. 
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
%objects to See Also as \code{\link{help}}
}
\examples{
%~~ simple examples of the most important functions ~~
medci(mu.x=.2,mu.y=.4,se.x=.1,se.y=.05,rho=0,alpha=.05)
pprodnormal(q=.4, mu.x=.5, mu.y=.3, se.x=.03, se.y=.08, rho= 0)
qprodnormal(p=.1, mu.x=.5, mu.y=.3, se.x=.03, se.y=.8, rho=0)
}
