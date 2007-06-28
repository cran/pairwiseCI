"Prop.diff" <-

function(x, y, conf.level=0.95, alternative="two.sided", ...)
{
 args<-list(...)

 if( is.data.frame(x) && is.data.frame(y) )
  {
   colsx<-colSums(x); colsy<-colSums(y)
   args$n <- as.numeric(c( sum(colsx), sum(colsy) ))
   args$x <- as.numeric(c( colsx[1], colsy[1] ))
  }
  else
   {
    if((is.numeric(x) && is.numeric(y)) && ( length(x)==2 && length(y)==2 ))
     {
     args$n <- as.numeric(c( sum(x), sum(y) ))
     args$x <- as.numeric(c( x[1], y[1] ))
     }
   else{stop("Prop.test needs two data.frames or two numeric vectors of length 2 as input")}
   }

 args$alternative <- alternative

 args$conf.level <- conf.level
 args$alternative <- alternative

temp<- do.call("prop.test", args)

return(
list(conf.int=temp$conf.int,
estimate=temp$estimate[[1]]-temp$estimate[[2]])   
)
}

