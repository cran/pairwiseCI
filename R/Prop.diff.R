"Prop.diff" <-

function(x, y, conf.level=0.95, alternative="two.sided", CImethod=c("NHS","CC","AC"), ...)
{
 args<-list(...)

CImethod<-match.arg(CImethod)

 if( is.data.frame(x) && is.data.frame(y) )
  {
   colsx<-colSums(x)
   colsy<-colSums(y)
   n <- as.numeric(c( sum(colsx), sum(colsy) ))
   x <- as.numeric(c( colsx[1], colsy[1] ))
  }
  else
   {
    if((is.numeric(x) && is.numeric(y)) && ( length(x)==2 && length(y)==2 ))
     {
     n <- as.numeric(c( sum(x), sum(y) ))
     x <- as.numeric(c( x[1], y[1] ))
     }
   else{stop("Prop.test needs two data.frames or two numeric vectors of length 2 as input")}
   }

switch(CImethod,

CC={

 args$n <- n
 args$x <- x
 args$alternative <- alternative
 args$conf.level <- conf.level
 args$alternative <- alternative

 temp<- do.call("prop.test", args)
 conf.int<-temp$conf.int
 estimate<-temp$estimate[[1]]-temp$estimate[[2]]
 METHOD<-"Continuity corrected interval for the difference of proportions"
 },

AC={

   args$nx <- n[1]
   args$ny <- n[2]
   args$X <- x[1]
   args$Y <- x[2]

switch(alternative,
"two.sided"={args$quantile<-qnorm( 1-(1-conf.level)/2 ) },
"less"={args$quantile<-qnorm(conf.level)},
"greater"={args$quantile<-qnorm(1-conf.level)}
)

 args$alternative <- alternative

temp<- do.call("Add4", args)

estimate<-temp$estimate
conf.int<-temp$conf.int

METHOD<-"Agresti-Caffo interval for the difference of proportions"

},


NHS={


   args$nx <- n[1]
   args$ny <- n[2]
   args$X <- x[1]
   args$Y <- x[2]

switch(alternative,
"two.sided"={args$quantile<-qnorm( 1-(1-conf.level)/2 ) },
"less"={args$quantile<-qnorm(conf.level)},
"greater"={args$quantile<-qnorm(1-conf.level)}
)

 args$alternative <- alternative

temp<- do.call("NHS", args)

estimate<-temp$estimate
conf.int<-temp$conf.int

METHOD<-"Newcombes Hybrid Score interval for the difference of proportions"

}
)



attr(conf.int, which="methodname")<-METHOD

return(
list(conf.int=conf.int,
estimate=estimate)   
)
}


"NHS" <-
  function(nx, ny, X, Y, quantile=qnorm(0.95), alternative="two.sided" )
  {
    
    m<-nx
    n<-ny
    
    # Wilson Score CI for one proportion:
    
    WilsonScore <- function(n,Y,quant=qnorm(0.95),alternative="two.sided") {
      
      t=Y/n
      if(alternative =="two.sided") 
      {   
        est.int=(Y+(quant^2)/2)/(n+(quant)^2)
        w.se=((quant)*sqrt(n*t*(1-t)+(quant^2)/4))/(n+quant^2)
        KI=c( est.int-w.se, est.int+w.se )
      }
      else{
        if(alternative=="less")
        {   
          est.int=(Y+(quant^2)/2)/(n+(quant)^2)
          w.se=((quant)*sqrt(n*t*(1-t)+(quant^2)/4))/(n+quant^2)
          KI=c( 0, est.int+w.se )
        } 
        else{
          if(alternative=="greater")
          {  
            est.int=(Y+(quant^2)/2)/(n+(quant)^2)
            w.se=((quant)*sqrt(n*t*(1-t)+(quant^2)/4))/(n+quant^2)
            KI=c( est.int-w.se , 1 )
          }
          else{stop("argument alternative misspecified")}}}
      
      conf.int = KI
      conf.int
    }
    
    pX <- X/m
    pY <- Y/n
    estimate <- pX-pY
    
    
    if(alternative=="two.sided")
    {
      quant<-quantile
      CIX <- WilsonScore(n=m, Y=X, quant=quant, alternative="two.sided")
      lX <- CIX[1]
      uX <- CIX[2]
      
      CIY <- WilsonScore(n=n, Y=Y, quant=quant, alternative="two.sided") 
      lY <- CIY[1]
      uY <- CIY[2]
      
      conf.int <- c(estimate - quant * sqrt( (lX*(1-lX)/m) + (uY*(1-uY)/n) ) , 
                    estimate + quant * sqrt( (uX*(1-uX)/m) + (lY*(1-lY)/n) ) )
    }
    
    if(alternative=="less")
    {
      quant<-quantile
      CIX <- WilsonScore(n=m, Y=X, quant=quant, alternative="less")
      uX <- CIX[2]
      
      CIY <- WilsonScore(n=n, Y=Y, quant=quant, alternative="greater") 
      lY <- CIY[1]
      
      
      conf.int <- c( -1 , 
                     estimate + quant * sqrt( (uX*(1-uX)/m) + (lY*(1-lY)/n) ) )
    }
    
    
    if(alternative=="greater")
    {
      quant<- (-quantile)
      CIX <- WilsonScore(n=m, Y=X, quant=quant, alternative="greater")
      lX <- CIX[1]
      
      
      CIY <- WilsonScore(n=n, Y=Y, quant=quant, alternative="less") 
      uY <- CIY[2]
      
      conf.int <- c(estimate - quant * sqrt( (lX*(1-lX)/m) + (uY*(1-uY)/n) ) , 1 )
    }
    
    # if(all("two.sided", "less", "greater") != alternative)
    #  { stop("argument alternative mis-specified") }
    
    
    list(conf.int = conf.int,
         estimate = estimate)
    
    # end of HybScore
  }


"Add4" <-
  function(nx, ny, X, Y, quantile, alternative) 
  {
    
    pxI <- (X+1)/(nx+2)
    pyI <- (Y+1)/(ny+2)
    
    nxI <- nx+2
    nyI <- ny+2
    estI <- pxI - pyI
    stderr <- sqrt( pxI*(1-pxI)/nxI + pyI*(1-pyI)/nyI )
    
    if(alternative=="two.sided")
    {
      lower <- estI - quantile*stderr
      upper <- estI + quantile*stderr
    }
    
    if(alternative=="less")
    {
      lower <- (-1)
      upper <- estI + quantile*stderr
    }
    
    if(alternative=="greater")
    {
      lower <- estI + quantile*stderr
      upper <- 1
    }
    
    px <- X/nx
    py <- Y/ny
    estimate <- px-py
    
    list(conf.int=c(lower,upper),
         estimate=estimate)
    
  }

