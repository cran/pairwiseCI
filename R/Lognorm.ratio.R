"Lognorm.ratio" <- function(x, y, conf.level=0.95, alternative="two.sided", ...)
{
args<-list(...)

nx <- length(x)
ny <- length(y)

if(all(x>0) & all(y>0))
 {lx <- log(x); ly <- log(y); active<-TRUE}
 else
  {
   if(any(x<0) | any(y<0))
    {estimate<-NA; conf.int<-c(NA,NA); active=FALSE
     warning("negative values occured")
    }
    else
     {
     lx <- log(x+0.1); ly <- log(y+0.1); active<-TRUE
     warning("0.1 added to x and y, because 0 accured")
     }

  }

if(active)
 {
 mlx <- mean(lx); varlx <- var(lx)
 mly <- mean(ly); varly <- var(ly)

 estpsi <- mlx + 0.5*varlx - mly - 0.5*varlx

 invImatest <- diag(c(
 sqrt(varlx)/nx, (2*varlx)/nx,
 sqrt(varly)/ny, (2*varly)/ny )) 

 pdpsi <- matrix(c(1,0.5,-1,-0.5), ncol=1)

 stderr <- sqrt(t(pdpsi) %*% (invImatest) %*% pdpsi)
 estimate <- exp(estpsi)

 if(alternative=="two.sided")
  {
   quantile <- qnorm(p=1-(1-conf.level)/2 ) 
   conf.int<-exp(c(estpsi-quantile*stderr, estpsi+quantile*stderr ))
  }

 if(alternative=="less")
  {
   quantile <- qnorm(p=1-(1-conf.level)) 
   conf.int <- exp(c(-Inf, estpsi+quantile*stderr ))
  }

 if(alternative=="greater")
  {
   quantile <- qnorm(p=1-(1-conf.level)) 
   conf.int <- exp(c(estpsi-quantile*stderr, Inf ))
  }
 }

return(list(
conf.int=conf.int,
estimate=estimate))
}
