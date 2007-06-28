
Param.diff <- function(x, y, conf.level=0.95, alternative="two.sided", ...)

{

addargs<-list(...)

   addargs$x <- x
   addargs$y <- y
   addargs$alternative <- alternative
   addargs$conf.level <- conf.level

   temp <- do.call(what="t.test", args=addargs)
   conf.int <- temp$conf.int
   estimate <- temp$estimate[1]-temp$estimate[2]
   names(estimate) <- "difference of means"
  

return(list(
conf.int=conf.int,
estimate=estimate
))

}
