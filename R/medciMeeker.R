medciMeeker <-
function(mu.x,mu.y,se.x,se.y,rho=0,alpha=.05,root.eps=1e-12,...){
  p <- alpha/2
  q.l <- qprodnormalMeeker(p, mu.x=mu.x, mu.y=mu.y, se.x=se.x, se.y=se.y, rho=rho, lower.tail=TRUE, root.eps=root.eps)
  q.u <- qprodnormalMeeker(p, mu.x=mu.x, mu.y=mu.y, se.x=se.x, se.y=se.y, rho=rho, lower.tail=FALSE, root.eps=root.eps)
  CI <- c(q.l[[1]],q.u[[1]]) #confidence interval
  names(CI) <- c(paste((alpha/2*100),"%"),paste((1-alpha/2)*100,"%"))
  return(CI)
}

