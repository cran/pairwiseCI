
Param.ratio <- function(x, y, conf.level=0.95, alternative="two.sided", ...)

{
require(mratios)

addargs<-list(...)

   addargs$x <- x
   addargs$y <- y
   addargs$alternative <- alternative
   addargs$conf.level <- conf.level

   temp <- do.call(what="t.test.ratio", args=addargs)
   conf.int <- temp$conf.int
   estimate <- temp$estimate[3]
   names(estimate) <- "ratio of means"
  

return(list(
conf.int=conf.int,
estimate=estimate
))

}
