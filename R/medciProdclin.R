medciProdclin <-
  function(mu.x, mu.y, se.x, se.y, rho=0, alpha=.05) {
    ## library.dynam("RMediation",PACKAGE="RMediation")
    ## ans <- .Fortran('test_fnprod', a=as.numeric(a), b=as.numeric(b), sea=as.numeric(sea), seb=as.numeric(seb), rho=as.numeric(rho), alpha=as.numeric(alpha), lowz=numeric(1), highz=numeric(1), ier=integer(1), abserr=numeric(1), last=integer(1)) # Return the value of the result parameter
    ## CI <- c(ans$lowz, ans$highz)
      p <- alpha/2
      q.l <- qprodnormalProdclin(p, mu.x=mu.x, mu.y=mu.y, se.x=se.x, se.y=se.y, rho=rho, lower.tail=TRUE)
      q.u <- qprodnormalProdclin(p, mu.x=mu.x, mu.y=mu.y, se.x=se.x, se.y=se.y, rho=rho, lower.tail=FALSE)
      CI <- c(q.l[[1]],q.u[[1]]) #confidence interval
      names(CI) <- c(paste((alpha/2*100),"%"),paste((1-alpha/2)*100,"%"))
      return(CI)
  }
