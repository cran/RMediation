pprodnormalProdclin<-
    function(q, mu.x, mu.y, se.x, se.y, rho=0, lower.tail=TRUE){
      mu.x <- mu.x/se.x
      mu.y <- mu.y/se.y
      z <- q/(se.x*se.y)
      ans <- .Fortran('fnprod', mu.x=as.numeric(mu.x), mu.y=as.numeric(mu.y), rho=as.numeric(rho),  z=as.numeric(z), answer=numeric(1), ier=integer(1), abserr=numeric(1), last=integer(1)) # Return the value of the result parameter
      percentile<-ans$answer
      error <- ans$abserr
      if (!lower.tail)
          percentile <- 1-percentile
      return(list(p=percentile,error=error))
  }
