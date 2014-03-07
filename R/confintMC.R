confintMC <- function(mu, Sigma, quant=NULL, alpha=0.05, type="MC", plot=FALSE, plotCI=FALSE, n.mc = 1e+06, ...){
  q1 <- quant
  quant <- parse(text=sub("~","",quant))
  df <- data.frame(mvrnorm(n.mc,mu,Sigma))
  colnames(df) <-names(mu)
  quant.vec <- eval(quant,df)
  CI <- (quantile(quant.vec,c(alpha/2,1-alpha/2)))
  names(CI) <- c(paste((alpha/2*100),"%"),paste((1-alpha/2)*100,"%"))
  quantMean <- mean(quant.vec)
  quantSE <- sd(quant.vec)
  quantError <- quantSE/n.mc
  res <- list(CI,quantMean,quantSE,quantError) #Results
  attr(res,"quant")  <- q1
  names(res) <- c(paste((1-alpha/2)*100,"% ", "CI",sep=""), "Estimate", "SE","MC Error")
  outer <- FALSE #outer position
  mcex <- .8
  if (plot==TRUE)
  {
    range1 <- (quantile(quant.vec,c(alpha/4,1-alpha/4)))
    max1 <- range1[2]
    min1 <- range1[1]
    res.asymp <- confintAsymp(mu=mu, Sigma=Sigma, quant=quant, type=type, alpha=alpha)
    if(type=="all"){
      range.asymp <- c(res.asymp$Estimate-3*res.asymp$SE, res.asymp$Estimate+3*res.asymp$SE)
      max1 <- max(max1,range.asymp)
      min1 <- min(min1,range.asymp)      
    }
    xrange<-seq(min1,max1,length=7)
    xrange<-pretty(xrange,n=9)
    xy <- quant.vec[quant.vec>min1 & quant.vec<max1]
    
    plot(density(xy),xlab="",ylab="",axes=FALSE,xlim=c(min1,max1), main="", lwd=2)
    mtext(quant,1,5)
    axis(1,xrange,xrange, line=2.5);
    
    axis(2,line=1.1)
    smidge <- par("cin")*abs(par("tcl"))

    if(type %in% c("mc", "MC")){
      #New- 1/24/14-DT
      mtext(paste("Kurtosis=",round(kurtosis(quant.vec, type=2),3)), side=3, line=0, outer=outer, at=max1-2*(max1-min1)/9, cex=mcex)
      mtext(paste("Skewness=",round(skewness(quant.vec, type=2),3)), side=3, line=1, outer=outer, at=max1-2*(max1-min1)/9, cex=mcex)      
      mtext(paste("LL=",round(CI[1],3)),side=3,line=2,outer=outer, at=max1-1*(max1-min1)/9, cex=mcex)
      mtext(paste("UL=",round(CI[2],3)),side=3,line=3,outer=outer,at=max1-1*(max1-min1)/9, cex=mcex)
      mtext("Monte Carlo",side=3,line=4,outer=outer,at=max1-1*(max1-min1)/9, cex=mcex)
    }
    
    if(type=="all"){
      #New- 1/24/14-DT
      mtext(paste("Kurtosis=",round(kurtosis(quant.vec, type=2),3)), side=3,line=1,outer=outer, at=max1-2*(max1-min1)/9, cex=mcex, adj=0)
      mtext(paste("Skewness=",round(skewness(quant.vec, type=2) ,3)) ,side=3,line=2,outer=outer, at=max1-2*(max1-min1)/9, cex=mcex, adj=0)
      mtext(paste("LL=", round(CI[1],3)), side=3,line=3, outer=outer, at=max1-2*(max1-min1)/9, cex=mcex, adj=0)
      mtext(paste("UL=", round(CI[2],3)), side=3,line=4, outer=outer, at=max1-2*(max1-min1)/9, cex=mcex, adj=0)
      mtext("Monte Carlo", side=3,line=5, outer=outer, at=max1-2*(max1-min1)/9, cex=mcex, adj=0)
      if (res.asymp$SE>40*.Machine$double.eps) legend(x=max1,y= par("usr")[4],c("Monte Carlo","Asymptotic Normal"),col=c("black","blue"),lty=c(1,2),lwd=c(2:2),bty="n",title="", cex=mcex, y.intersp=.5, xpd=FALSE, xjust=.5) #, inset=c(-.3,0), xpd=FALSE)
    }

    if(plotCI){
      yci<-par("usr")[3]-1.2*diff(par("usr")[3:4])/25
      arrows(CI[1],yci,CI[2],yci,length=0,angle=90,code=3,cex=1.5,lwd=2)
      points(quantMean,yci,pch=19,cex=1.5)
    }
    
  } #if
  
  return(res)
}
