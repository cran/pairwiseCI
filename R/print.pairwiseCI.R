"print.pairwiseCI" <-
function( x , digits=4, ...)

{
byout <- x$byout
bynames <- x$bynames
method <- x$method
pargs<-list(...)
pargs$digits<-digits


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
   {methodI <- "Difference of proportions, Continuity corrected"}

  if(method=="Prop.diffAdd2")
   {methodI <- "Difference of proportions, Agresti-Caffo"}

  if(method=="Prop.ratio")
   {methodI <- "Ratio of proportions"}

  if(method=="Prop.or")
   {methodI <- "Odds ratio"}

  cat(" ","\n")
  cat(x$conf.level*100, " %-confidence intervals", "\n")
  cat(" Method: ", methodI, "\n")
  cat(" ","\n")

if(length(byout) != length(bynames)) {stop("INTERNAL: bynames and byout of different length!! ")}

 for (i in 1:length(byout))
  {
   CItable <- round( cbind(byout[[i]]$estimate, byout[[i]]$lower, byout[[i]]$upper), digits=digits)
   colnames(CItable) <- c("estimate", "lower", "upper")
   rownames(CItable ) <- byout[[i]]$compnames

     cat(" ","\n")
     if(length(byout)>1)
      {cat(" BY GROUP ", bynames[i], "\n")}
     pargs$x<-CItable
     do.call("print", pargs)
     cat(" ","\n")
     cat(" ","\n")
  }

invisible(x)
}

