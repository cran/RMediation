#' Confidence Interval for the Mediated Effect
#'
#' Produces confidence intervals for the mediated effect and the product of two
#' normal random variables
#'
#' @param mu.x  mean of \eqn{x}
#' @param mu.y mean of \eqn{y}
#' @param se.x standard error (deviation) of \eqn{x}
#' @param se.y  standard error (deviation) of \eqn{y}
#' @param rho correlation between \eqn{x} and \eqn{y}, where -1 <\code{rho} < 1.
#'   The default value is 0.
#' @param alpha significance level for the confidence interval. The default
#'   value is .05.
#' @param type method used to compute confidence interval. It takes on the
#'   values \code{"dop"} (default), \code{"MC"}, \code{"asymp"} or \code{"all"}
#' @param plot when \code{TRUE}, plots the distribution of \code{n.mc} data
#'   points from the distribution of product of two normal random variables
#'   using the density estimates provided by the function \code{\link{density}}.
#'   The default value is \code{FALSE}.
#' @param plotCI  when \code{TRUE}, overlays a confidence interval with error
#'   bars on the plot for the mediated effect. Note that to obtain the CI plot,
#'   one must also specify \code{plot="TRUE"}. The default value is
#'   \code{FALSE}.
#' @param n.mc when \code{type="MC"}, \code{n.mc} determines the sample size for
#'   the Monte Carlo method. The default sample size is 1E5.
#' @param \dots additional arguments to be passed on to the function.
#'
#' @details  This function returns a (\eqn{1-\alpha})% confidence interval for
#'   the mediated effect (product of two normal random variables). To obtain a
#'   confidence interval using a specific method, the argument \code{type}
#'   should be specified. The default is \code{type="dop"}, which uses the code
#'   we wrote in \R to implement the distribution of product of the coefficients
#'   method described by Meeker and Escobar (1994) to evaluate the CDF of the
#'   distribution of product. \code{type="MC"} uses the Monte Carlo approach to
#'   compute the confidence interval (Tofighi & MacKinnon, 2011).
#'   \code{type="asymp"} produces the asymptotic normal confidence interval.
#'   Note that except for the Monte Carlo method, the standard error for the
#'   indirect effect is based on the analytical results by Craig (1936):
#'   \deqn{\sqrt(se.y^2 \mu.x^2+se.x^2 \mu.y^2+2 \mu.x \mu.y \rho se.x se.y+
#'   se.x^2 se.y^2+se.x^2 se.y^2 \rho^2) }. In addition, the estimate of
#'   indirect effect is \eqn{\mu.x \mu.y +\sigma.xy }; \code{type="all"} prints
#'   confidence intervals using all four options.
#'
#' @return A vector of lower confidence limit and upper confidence limit. When
#'   \code{type} is \code{"prodclin"} (default), \code{"DOP"}, \code{"MC"} or
#'   \code{"asymp"}, \code{medci} returns a \link{list} that contains:
#'   \item{(\eqn{1-\alpha})% CI}{a vector of lower and upper confidence
#'   limits,} \item{Estimate}{a point estimate of the quantity of interest,}
#'   \item{SE}{standard error of the quantity of interest,} \item{MC Error}{When
#'   \code{type="MC"}, error of the Monte Carlo estimate.} Note that when
#'   \code{type="all"}, \code{medci} returns a \link{list} of \emph{four}
#'   objects, each of which a \link{list} that contains the results produced by
#'   each method as described above.
#'
#' @references Craig, C. C. (1936). On the frequency function of \eqn{xy}.
#'   \emph{The Annals of Mathematical Statistics}, \bold{7}, 1--15.
#'
#'   MacKinnon, D. P., Fritz, M. S., Williams, J., and Lockwood, C. M. (2007).
#'   Distribution of the product confidence limits for the indirect effect:
#'   Program PRODCLIN. \emph{Behavior Research Methods}, \bold{39}, 384--389.
#'
#'   Meeker, W. and Escobar, L. (1994). An algorithm to compute the CDF of the
#'   product of two normal random variables. \emph{Communications in Statistics:
#'   Simulation and Computation}, \bold{23}, 271--280.
#'
#'   Tofighi, D. and MacKinnon, D. P. (2011). RMediation: An R package for mediation analysis
#'   confidence intervals. \emph{Behavior Research Methods}, \bold{43},
#'   692--700. \doi{doi:10.3758/s13428-011-0076-x}
#'
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#'
#' @examples
#'  res <- medci(mu.x=.2, mu.y=.4, se.x=1, se.y=1, rho=0, alpha=.05,
#'  type="prodclin", plot=TRUE, plotCI=TRUE)
#'
#' @seealso \code{\link{qprodnormal}} \code{\link{pprodnormal}} \code{\link{ci}}
#'   \code{\link{RMediation-package}}
#'
#' @keywords mediation
#' @export

medci <-function(mu.x,mu.y,se.x,se.y,rho=0,alpha=.05,type="dop", plot=FALSE,plotCI=FALSE, n.mc=1e5,...)
{
    if(!is.numeric(mu.x))
        stop("Argument mu.x must be numeric!")
    if(!is.numeric(mu.y))
        stop("Argument mu.y must be numeric!")
    if(!is.numeric(se.x))
        stop("Argument se.x must be numeric!")
    if(!is.numeric(se.y))
        stop("Argument se.y must be numeric!")
    if(!is.numeric(alpha))
        stop("Argument alpha  must be numeric!")
    if(!is.numeric(rho))
        stop("Argument rho  must be numeric!")
    if(alpha<=0 || alpha >=1)
        stop("alpha must be between 0 and 1!")
    if(rho<=-1 || rho>=1)
        stop("rho must be between -1 and 1!")
    if(!is.numeric(n.mc) || is.null(n.mc))
        n.mc=1e5 # sets n.mc to default
    if (plot==TRUE)
      {
        mean.v <- c(mu.x,mu.y)
        var.mat <- matrix(c(se.x^2,se.x*se.y*rho,se.x*se.y*rho,se.y^2),2)
        x_y <- matrix(rnorm(2*n.mc),ncol=n.mc)
        x_y <- crossprod(chol(var.mat),x_y)+mean.v
        x_y <- t(x_y)
        xy <- x_y[,1]*x_y[,2]
        se.xy <- sqrt(se.y^2*mu.x^2+se.x^2*mu.y^2+2*mu.x*mu.y*rho*se.x*se.y+se.x^2*se.y^2+se.x^2*se.y^2*rho^2);
        mu.xy <- mu.x*mu.y+rho*se.x*se.y
        max1<-mu.xy+6*se.xy
        min1<-mu.xy-6*se.xy
        if (min1>0 || max1<0 )
          xrange<-round(seq(min1,max1,length=7),1)
	else
          xrange<-round(cbind(seq(min1,0,length=3),seq(0,max1,length=3)),1)
        xy <- xy[xy>min1 & xy<max1]
        plot(density(xy),xlab=expression(paste("Product ", (italic(xy)))),ylab="Density",axes=FALSE,xlim=c(min1,max1),main="",...)
        axis(1,xrange);axis(2)
        smidge <- par("cin")*abs(par("tcl"))
        text(max1-(max1-min1)/7,(par("usr")[4]),pos=1, bquote(mu== .(round(mu.xy,3)) ),...)
        text(max1-(max1-min1)/7,(par("usr")[4]-1.5*par("cxy")[2]),pos=1, bquote(sigma== .(round(se.xy,3)) ),...)
        if(plotCI){
          yci<-par("usr")[3]+diff(par("usr")[3:4])/25
          yci<-0
          MedCI <- medciMeeker(mu.x, mu.y, se.x, se.y, rho, alpha)
          arrows(MedCI[[1]][1],yci,MedCI[[1]][2],yci,length=smidge,angle=90,code=3,cex=1.5,...)
          points(mu.xy,yci,pch=19,cex=1.5,...)
          text(max1-(max1-min1)/7,(par("usr")[4]-3*par("cxy")[2]),pos=1, paste("LL=",round(MedCI[[1]][1],3)),...)
          text(max1-(max1-min1)/7,(par("usr")[4]-4.5*par("cxy")[2]),pos=1, paste("UL=",round(MedCI[[1]][2],3)),...)
        }
      }

    if (type=="all" || type=="All" || type=="ALL")
      {
        MCCI= medciMC(mu.x, mu.y, se.x, se.y, rho , alpha , n.mc = n.mc)
        asympCI <- medciAsymp(mu.x, mu.y, se.x, se.y, rho, alpha) # added 3/28/14-DT
        res <- list( MeekerCI, MCCI, asympCI)
        names(res) <- c( "Monte Carlo", "Asymptotic Normal")
        return(res)
      }
    else if (type=="DOP" || type=="dop" || type=="prodclin")
      {
        MeekerCI=medciMeeker(mu.x, mu.y, se.x, se.y, rho, alpha)
        return(MeekerCI)
      }
    else if (type=="MC" || type=="mc" || type=="Mc")
    {
      MCCI= medciMC(mu.x, mu.y, se.x, se.y, rho , alpha , n.mc = n.mc)
      return(MCCI)
    }
    else if(type=="Asymp" || type=="asymp")
      {
        asympCI <- medciAsymp(mu.x, mu.y, se.x, se.y, rho, alpha) # Modified/ added 3/28/14
        return(asympCI)
      }
    else stop("Wrong type! please specify type=\"all\", \"DOP\", \"prodclin\",\"MC\", or \"asymp\" ")
  }
