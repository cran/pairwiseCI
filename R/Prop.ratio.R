"Prop.ratio" <-
function(x, y, conf.level=0.95, alternative="two.sided")
{

 if( is.data.frame(x) && is.data.frame(y) )
  {
   colsx<-colSums(x)
   colsy<-colSums(y)

   nx<-sum(colsx)
   ny<-sum(colsy)
   XI<-colsx[1]+0.5
   YI<-colsy[1]+0.5
   nxI<-nx+0.5
   nyI<-ny+0.5
   estimate <- (colsx[1]/nx)/(colsy[1]/ny)
  }
  else
   {
    if((is.numeric(x) && is.numeric(y)) && ( length(x)==2 && length(y)==2 ))
     {
      nx<-sum(x)
      ny<-sum(y)
      XI<-x[1]+0.5
      YI<-y[1]+0.5
      nxI<-nx+0.5
      nyI<-ny+0.5
      estimate <- (x[1]/nx)/(y[1]/ny)
     }
   else{stop("Prop.or needs two data.frames or two numeric vectors of length 2 as input")}
   }

 estI <- log( (XI/nxI)/(YI/nyI) )

 stderrlog <- sqrt( 1/XI + 1/YI - 1/nxI - 1/nyI )

 if(alternative=="two.sided")
  {
   zts <- qnorm(p = 1-(1-conf.level)/2 ) 
   lower <- estI - zts * stderrlog 
   upper <- estI + zts * stderrlog
  }

 if(alternative=="less")
  {
   zos <- qnorm(p = conf.level ) 
   lower <- (-Inf)
   upper <- estI + zos * stderrlog
  }

 if(alternative=="greater")
  { 
   zos <- qnorm(p = conf.level )
   lower <- estI - zos * stderrlog
   upper <- Inf
  }
 if(is.na(lower)){lower <- -Inf}
 if(is.na(upper)){upper <- Inf}

return(
list(conf.int=exp(c(lower, upper)),
estimate=estimate)  
) 
}

