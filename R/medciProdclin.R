medciProdclin <-
  function(mu.x, mu.y, se.x, se.y, rho=0, alpha=.05) {
      p <- alpha/2
      q.l <- qprodnormalProdclin(p, mu.x=mu.x, mu.y=mu.y, se.x=se.x, se.y=se.y, rho=rho, lower.tail=TRUE)
      q.u <- qprodnormalProdclin(p, mu.x=mu.x, mu.y=mu.y, se.x=se.x, se.y=se.y, rho=rho, lower.tail=FALSE)
      CI <- c(q.l[[1]],q.u[[1]]) #confidence interval
      names(CI) <- c(paste((alpha/2*100),"%"),paste((1-alpha/2)*100,"%"))
      return(CI)
  }
