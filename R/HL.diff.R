
HL.diff <- function(x, y, conf.level=0.95, alternative="two.sided", ...)

{
require(exactRankTests)

x<-as.numeric(x)
y<-as.numeric(y)

addargs<-list(...)

addargs$x <- x
addargs$y <- y
addargs$alternative <- alternative
addargs$conf.level <- conf.level
addargs$conf.int <- TRUE



 temp <- do.call(what="wilcox.exact", args=addargs)

 conf.int <- temp$conf.int 
 estimate <- temp$estimate
 METHOD <- "Difference of location using the Hodges-Lehmann estimator"

attr(conf.int, which="methodname")<-METHOD

return(list(
conf.int=conf.int,
estimate=estimate
))

}
