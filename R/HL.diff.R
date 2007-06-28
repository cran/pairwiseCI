
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

return(list(
conf.int=conf.int,
estimate=estimate
))

}

HL.diff(x=c(2,3,4,56,7,5), y=c(1,2,3,2,3,2,1,1,1))

