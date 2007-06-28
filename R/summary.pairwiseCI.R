`summary.pairwiseCI` <-
function( object, digits=4, ...)
{
byout <- object$byout
bynames <- object$bynames
method <- object$method

  if(method=="Param.diff")
   {methodI <- "Difference of means"}

  if(method=="Param.ratio")
   {methodI <- "Fieller: ratio of means"}

 if(method=="Lognorm.diff")
   {methodI <- "Difference of means of Lognormals"}

  if(method=="Lognorm.ratio")
   {methodI <- "Ratio of means of Lognormals"}

  if(method=="HL.diff")
   {methodI <- "Exact Hodges-Lehmann intervals: difference of locations"}

  if(method=="HL.ratio")
   {methodI <- "Exact Hodges-Lehmann intervals: ratio of locations"}

  if(method=="HD.diff")
   {methodI <- "Difference of Harrell-Davis estimators: Percentile bootstrap"}

  if(method=="Median.diff")
   {methodI <- "Difference of Medians: Percentile bootstrap"}

  if(method=="HD.ratio")
   {methodI <- "Ratio of Harrell-Davis estimators: Percentile bootstrap"}

  if(method=="Median.ratio")
   {methodI <- "Ratio of Medians: Percentile bootstrap"}

  if(method=="Prop.diff")
   {methodI <- "Score: difference of proportions"}

  if(method=="Prop.ratio")
   {methodI <- "Add4: ratio of proportions"}

  if(method=="Prop.or")
   {methodI <- "Adjusted Woolf: odds ratio"}


if(length(bynames)==1)
 {
   estimate <- as.data.frame(matrix(round( byout[[1]]$estimate, digits=digits), ncol=1))
   conf.int <- as.data.frame(round( cbind(byout[[1]]$lower, byout[[1]]$upper), digits=digits))

   colnames(estimate) <- c("estimate")
   rownames(estimate) <- byout[[1]]$compnames

   colnames(conf.int) <- c("lower", "upper")
   rownames(conf.int) <- byout[[1]]$compnames

   out<-list(estimate=estimate, conf.int=conf.int)
 }
else
 {
  
  if(length(byout) != length(bynames))
   {stop("INTERNAL: bynames and byout of different length!! ")}

   out<-list()
   for (i in 1:length(byout))
    {
     estimate <- as.data.frame(matrix(round( byout[[i]]$estimate, digits=digits), ncol=1))
     conf.int <- as.data.frame(round( cbind(byout[[i]]$lower, byout[[i]]$upper), digits=digits))

     colnames(estimate) <- c("estimate")
     rownames(estimate) <- byout[[i]]$compnames

     colnames(conf.int) <- c("lower", "upper")
     rownames(conf.int) <- byout[[i]]$compnames

     out[[i]]<-list(estimate=estimate, conf.int=conf.int)   
    }
   names(out)<-bynames
 }

attr(out, "methodname") <- methodI
attr(out, "conf.level") <- object$conf.level

class(out)<-"summary.pairwiseCI"
return(out)
}

