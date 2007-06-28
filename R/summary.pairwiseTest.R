"summary.pairwiseTest"<-

function( object, digits=4, p.adjust.method="none", letters=FALSE, ... )
{

args<-list(...)

 byout <- object$byout
 bynames <- object$bynames
 method <- object$method
 by <- object$by



if(is.null(by))
 {
 # no by-factor:

 p.adjust.args<-list()
 p.adjust.args$method <- p.adjust.method
 p.adjust.args$p <-  byout[[1]][,"p.value", drop=TRUE]


 if(!is.null(args$n))
  {p.adjust.args$n<-args$n}

 p.val.adj <- do.call("p.adjust", p.adjust.args )

 
 out<-data.frame(cbind(p.val.adj, byout[[1]]))

 names(out)<-c("p.val.adj", "p.val.raw", "comparison",
 "groupx", "groupy")

if(letters & is.null(object$control))
 {
  nslargs <- list()
  if(!is.null(args$alpha))
   {nslargs$alpha <- args$alpha}
  nslargs$factor1 <- out$groupx
  nslargs$factor2 <- out$groupy
  nslargs$pvalue <- out$p.val.adj
  letters <- do.call("nonsigletters", nslargs)
  print(letters)

 }

 }
else
 {
# with by-factor(i.e. byout has length>1)

 byv <- factor()

  byoutdat<-data.frame()

  for (i in 1:length(byout))
   {
    byoutdat <- rbind(byoutdat, byout[[i]])
    byv=c( byv, rep( bynames[i], times=length(byout[[i]][,"p.value"]) ))
   }

 p.adjust.args<-list()
 p.adjust.args$method <- p.adjust.method
 p.adjust.args$p <- byoutdat[,"p.value"]
 if(!is.null(args$n))
  {p.adjust.args$n<-args$n}

 p.val.adj <- do.call("p.adjust", p.adjust.args )

byvarout<-paste(by, collapse=".")

 out<-data.frame(cbind(p.val.adj, byoutdat, byv))

names(out)<-c("p.val.adj", "p.val.raw", "comparison",
 "groupx", "groupy", byvarout)
 }

attr(x=out, which="testmethod")<-object$method
attr(x=out, which="p.adjust.method")<-p.adjust.method
attr(x=out, which="digits")<-digits
class(out)<-"summary.pairwiseTest"

return(out)
}



"print.summary.pairwiseTest" <- 

function(x, ...)
{
cat("P-values calculated using", attr(x,"testmethod"),",\n")
cat("Adjustment for multiplicity:", attr(x,"p.adjust.method")  , "\n")
digits<-attr(x, "digits")
cat("\n")
class(x)<-"data.frame"

x[,"p.val.adj"]<-round(x[,"p.val.adj"], digits=digits)
x[,"p.val.raw"]<-round(x[,"p.val.raw"], digits=digits)

print(x, ...)
invisible(x)
}


