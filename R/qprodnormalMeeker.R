qprodnormalMeeker <-
function(p, mu.x, mu.y, se.x, se.y, rho=0, lower.tail=TRUE, root.eps=1e-12){
    ##This function takes the probability value alpha, lower limit, and upper limit of
    ##the support of distribution of product.
    ##It uses bi-section method to find percentile for upper and lower percetiles
    ##(p.u and p.l, respectively)
    ##At each iteration of bi-section loop, it calculates the upper and lower
    ##percetiles (p.u and p.l, respectively)
    ##and the respective quantiles (new,u0,l0) until it reaches the
    ##quantile (i.e., variable "new"" ) that matches the target alpha percentile.
    max.iter=10000
    eps <- 1; #intial value for precision
    ##bisection loop, root.eps is the precison of bi-section root finding algorithm
    mu.a <- mu.x/se.x #Rescaling distribution of a so that SD of a is 1
    mu.b <- mu.y/se.y #Rescaling distribution of b so that SD of b is 1
    s.a.on.b <- sqrt(1-rho^2) #SD of b conditional on a
    if (lower.tail==FALSE) {
        u0<- mu.a*mu.b +6*sqrt(1+mu.a^2+mu.b^2+2*mu.a*mu.b*rho+rho^2) #upper
        l0=mu.a*mu.b
        alpha<- 1- p
    }
    else{
        l0<-mu.a*mu.b - 6*sqrt(1+mu.a^2+mu.b^2+2*mu.a*mu.b*rho+rho^2)#lower
        u0<-mu.a*mu.b
        alpha<- p
    }
    gx=function(x, z) {
        ##Defining the integrand (Meeker & Escobar, 1995, p. 273)
        mu.a.on.b <- mu.a+rho*(x-mu.b) #mean of a conditional on b
        integ <- pnorm(sign(x)*(z / x-mu.a.on.b)/s.a.on.b)*dnorm(x-mu.b)
        ## alternatively, I could write the integrand as follows:
        ## integ <- pnorm(sign(x)*(z/x-mu.a.on.b),mean=0, sd=s.a.on.b)*dnorm(x,mean=mu.b, sd=1)
        return(integ)
    }
    p.l<-integrate(gx,lower=-Inf,upper=Inf, z=l0)$value-alpha
    p.u<-integrate(gx,lower=-Inf,upper=Inf, z=u0)$value-alpha
    iter=0
    while (p.l>0) {
        iter=iter+1
        l0=l0-0.5
        p.l<-integrate(gx,lower=-Inf,upper=Inf, z=l0)$value-alpha
        if (iter> 1000 )
            {
                cat(" No initial valid lower bound interval!\n")
                return(list(q=NA,error=NA))
            }
    }
    iter=0 #Reset iteration counter
    while (p.u<0) {
        iter=iter+1
        u0=u0+0.5
        p.u<-integrate(gx,lower=-Inf,upper=Inf, z=u0)$value-alpha
        if (iter> 1000 ){
            cat(" No initial valid upper bound interval!\n")
            return(list(q=NA,error=NA))
        }

    }
    iter=0 #r Reset iteration counter
    while(eps > root.eps) {
        ## Bi-section algorithm to find quantile
        new<- (u0+l0)/2
        p.l<-integrate(gx,lower=-Inf,upper=Inf, z=l0)$value-alpha
        p.u<-integrate(gx,lower=-Inf,upper=Inf, z=u0)$value-alpha
        integ.new <- integrate(gx,lower=-Inf,upper=Inf, z=new)
        p.new<-integ.new$value-alpha
        error.new <- integ.new$abs.error
        iter<-iter+1
        if (p.l*p.u>0) stop("Bisection algorithm faild!") #this is in case the initial algorithm to find the initial interval for bisection algorithm fails
        else if(iter>max.iter) {
            ## if bisection algorithm does not converge after max.iter reached
            eps<-root.eps/10
            new<-NA }
        else if (abs(p.new)<root.eps)  eps<-root.eps/10
        else if (p.l*p.new<0) {
            u0<-new
            eps=abs(p.new)
        }
        else {
            l0<-new
            eps=abs(p.new)
        }
    }     #end of while loop
    new <- new*se.x*se.y
    ##cat('quantile= ',new,'\n')
    return(list(q=new,error=error.new))              #returns quantile corresponding to p
}

