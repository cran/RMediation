qprodnormalProdclin<-
    function(p, mu.x, mu.y, se.x, se.y, rho=0, lower.tail=TRUE){
        if (!lower.tail)
            p <- 1-p
        ans <- .Fortran('quantile_prodclin', p=as.numeric(p), mu.x=as.numeric(mu.x), mu.y=as.numeric(mu.y), se.x=as.numeric(se.x),se.y=as.numeric(se.y), rho=as.numeric(rho),  answer=numeric(1), ier=integer(1), abserr=numeric(1), last=integer(1)) # Return the value of the result parameter
        q<-ans$answer
        error <- ans$abserr
        if (q==-702)
            {q <- NA
             error <- NA
             cat('type=\"PRODCLIN\" error! \n Bisection algorithm failed. Please report this case to dtofighi@psych.gatech.edu\n\n')
             }
        if (q==-703)
            {q <- NA
             error <- NA
             cat('type=\"PRODCLIN\" error! \n Maximum iteration in the Bisection algorithm has been reached. Please report this case to dtofighi@psych.gatech.edu\n\n')
                }
        return(list(q=q,error=error))
    }
