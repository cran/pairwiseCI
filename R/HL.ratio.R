
HL.ratio <- function(x, y, conf.level=0.95, alternative="two.sided", ...)

{
require(exactRankTests)

x<-as.numeric(x)
y<-as.numeric(y)

addargs<-list(...)

  if(all(x>0) && all(y>0))
   { 
   logx <- log(x); logy <- log(y); active <- TRUE
   }
   else 
    {
    if(any(x<0) || any(y<0)) 
      {
      conf.int <- c(NA,NA); estimate <- NA; active <- FALSE
      warning("negative values occured")
      }
     else
      {
      logx <- log(x + 0.1); logy <- log(y + 0.1); active <- TRUE
      warning("0.1 is added, because 0 occured")
      }
    } 

if(active)
 {
   addargs$x <- logx
   addargs$y <- logy
   addargs$alternative <- alternative
   addargs$conf.level <- conf.level
   addargs$conf.int <- TRUE

   temp <- do.call(what="wilcox.exact", args=addargs)
   conf.int <- exp(temp$conf.int)
   estimate <- exp(temp$estimate)
   names(estimate) <- "ratio of locations"
 }
return(list(
conf.int=conf.int,
estimate=estimate
))

}

HL.ratio(x=c(2,3,4,56,7,5), y=c(1,2,3,2,3,2,1,1,1))

