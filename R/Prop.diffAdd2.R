"Prop.diffAdd2" <-

function(x, y, conf.level=0.95, alternative="two.sided", ...)
{

require(binMto)

 args<-list(...)

 if( is.data.frame(x) && is.data.frame(y) )
  {
   colsx<-colSums(x); colsy<-colSums(y)
   args$nx <- as.numeric(sum(colsx))
   args$ny <-as.numeric(sum(colsy))
   args$X <- as.numeric(colsx[1])
   args$Y <- as.numeric(colsy[1])
  }
  else
   {
    if((is.numeric(x) && is.numeric(y)) && ( length(x)==2 && length(y)==2 ))
     {
     args$nx <- as.numeric(sum(x))
     args$ny <- as.numeric(sum(y))
     args$X <- as.numeric(x[1])
     args$Y <- as.numeric(y[1])
     }
   else{stop("Prop.test needs two data.frames or two numeric vectors of length 2 as input")}
   }

switch(alternative,
"two.sided"={args$quantile<-qnorm( 1-(1-conf.level)/2 ) },
"less"={args$quantile<-qnorm(conf.level)},
"greater"={args$quantile<-qnorm(conf.level)}
)

 args$alternative <- alternative

temp<- do.call("Add4", args)

return(
list(conf.int=temp$conf.int,
estimate=temp$estimate)   
)

}
