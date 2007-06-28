

"Lognorm.diff" <- function(x, y, conf.level=0.95, alternative="two.sided", ...)
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

 mx <- exp(mlx + 0.5*varlx)
 my <- exp(mly + 0.5*varly)

 estdelta <- mx - mly 

 invImatest <- diag(c(
 sqrt(varlx)/nx, (2*varlx)/nx,
 sqrt(varly)/ny, (2*varly)/ny )) 

 pddelta <- matrix(c(mx, 0.5*mx, -my, -0.5*my), ncol=1)

 stderr <- sqrt(t(pddelta) %*% (invImatest) %*% pddelta)
 estimate <- estdelta

 if(alternative=="two.sided")
  {
   quantile <- qnorm(p=1-(1-conf.level)/2 ) 
   conf.int<-c(estdelta-quantile*stderr, estdelta+quantile*stderr )
  }

 if(alternative=="less")
  {
   quantile <- qnorm(p=1-(1-conf.level)) 
   conf.int <- c(-Inf, estdelta+quantile*stderr )
  }

 if(alternative=="greater")
  {
   quantile <- qnorm(p=1-(1-conf.level)) 
   conf.int <- c(estdelta-quantile*stderr, Inf )
  }
 }

return(list(
conf.int=conf.int,
estimate=estimate))
}
