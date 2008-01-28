"plot.pairwiseCI" <-
function(x, CIvert=NULL, CIlty = 1, CIlwd=1, CIcex=1, H0line=NULL, H0lty=1, H0lwd=1, main=NULL, ylab="", xlab="", ... )
{

old.par <- par(no.readonly=TRUE)

# # # Check the arguments:

if(is.null(CIvert) == FALSE && is.logical(CIvert) == FALSE ) 
 {stop("argument CIvert must be specified as single logical value: either TRUE or FALSE ")}

if(length(CIvert) > 1) 
 {stop("argument CIvert must be specified as single logical value: either TRUE or FALSE ")}

if(is.numeric(CIlty) == FALSE || length(CIlty) != 1)
 {stop("CIlty must be specified as single integer (as appropriate for argument lty in lines)")}

if(is.numeric(CIlwd) == FALSE || length(CIlty) != 1)
 {stop("CIlwd must be specified as single integer (as appropriate for argument lwd in lines)")}

if(is.numeric(CIcex) == FALSE || length(CIcex) != 1)
 {stop("CIcex must be specified as single integer (as appropriate for argument cex in par)")}

if(is.numeric(H0lty) == FALSE)
 {stop("H0lty must be specified as integer (as appropriate for argument lty in lines)")}

if(is.numeric(H0lwd) == FALSE)
 {stop("H0lwd must be specified as integer (as appropriate for argument lwd in lines)")}


byout <- x$byout
bynames <- x$bynames
method <- x$method
alternative <- x$alternative
conf.level <- x$conf.level


# # # shall the CI be plotted vertical or horizontal

if(is.null(CIvert))
 {
  CIvert <- FALSE
 }


# # # which H0line shall be plotted: find a default


if(is.null(H0line))
 {
  if( any(c("Param.diff", "Lognorm.diff", "HL.diff", "HD.diff", "Median.diff", "Prop.diff") == method )) { H0line <- 0 } 
   else { H0line <- 1 }
 }
 else
 {
  if(mode(H0line)!="numeric")
   {stop("Specify a numeric value to be plotted as H0line")}

  if( any(c("Param.ratio", "Lognorm.ratio", "HL.ratio", "HD.ratio", "Median.ratio", "Prop.ratio", "Prop.or") == method ) && any(H0line <= 0) ) 
   {stop("the ratio is defined positive, specify H0line as a value greater 0 ")}
 }

# # H0line, H0lty and H0lwd to equal length:

H0par <- cbind(H0line, H0lty, H0lwd)
H0line <- H0par[,1]
H0lty <- H0par[,2]
H0lwd <- H0par[,3]



# # # only one by-group:

if(length(byout)==1)

{

 lower <- byout[[1]]$lower
 upper <- byout[[1]]$upper
 estimate <- byout[[1]]$estimate
 compnames <- byout[[1]]$compnames
 num <- 1:length(estimate) # index vector for the number of comparisons
 
 


 # # # all vectors of equal length??

 if( any( c(length(lower), length(upper), length(estimate)) != length(compnames) ) )
  { stop("plot.pairwiseCI INTERNAL: length of any (lower, upper,estimate, compnames) is not the same") }

 # # # remove NAs from the vectors of upper or lower limit:
 # # # replace them by the corresponding estimates

 if( any(c("Param.ratio", "Lognorm.ratio", "HL.ratio", "HD.ratio", "Median.ratio", "prop.ratio", "prop.or") == method) )
  { 
  if( any(is.na(lower)) || any(is.na(upper)) ) 
   
  lower[ which(is.na(lower)) ] <- estimate[ which(is.na(lower)) ] 
  upper[ which(is.na(upper)) ] <- estimate[ which(is.na(upper)) ] 
  }


 # # # find the PLOT margins:

 # # # alternative="two.sided"
 
 if(alternative=="two.sided")
  {
   lplot <- min(lower, H0line )  
   uplot <- max(upper, H0line )
  }

 if(alternative=="less")
  {
   lplot <- min(H0line, estimate) 
   uplot <- max(upper, H0line) 
  }

 if(alternative=="greater")
  {
   lplot <- min(H0line, lower) 
   uplot <- max(estimate, H0line) 
  }


 # # # define MAIN, SUB, YLAB,. . .

if(is.null(main)){main <- ""}

 # # # produce the RATIO-PLOT:

if( CIvert==TRUE )
 {

 plot.new() 

 # the default margin size in inches
  mymai <- par(no.readonly=TRUE)$mai

 # adjust margin under the x axis according to length of comparison names
  xwidth<- 1.1 * max(strwidth(compnames, units = "inches", cex = par("cex.axis"))) 

 if (mymai[1] < xwidth) 
        mymai[1] <- xwidth
 par(mai=mymai, new=TRUE)


plot(x = num, y = estimate, axes = FALSE, ylim = c(lplot, uplot), 
 type="p", pch=16, cex=CIcex,
 xlab=" ",
 ylab=ylab,
 main=main
 )


axis(side = 1, at = num, labels=compnames, las=2, ...)
axis(side=2,...)
box()


if(alternative=="two.sided")
 {
 for(i in 1:length(num))
  {
  lines(x = c(num[i],num[i]), y = c(lower[i], upper[i]), lty = CIlty, lwd=CIlwd)
  points(x = num[i], y = lower[i], pch="-", cex = CIcex*1.5)
  points(x = num[i], y = upper[i], pch="-", cex = CIcex*1.5)
  }

 for(l in 1:length(H0line))
  {lines(x=range(num)+c(-1,1), y=rep(H0line[l],times=2), lty=H0lty[l], lwd=H0lwd[l])}


 }


if(alternative=="less")
 {
 for(i in 1:length(num))
  {
  lines(x = c(num[i],num[i]), y = c(lplot, upper[i]), lty = CIlty, lwd=CIlwd)
  points(x = num[i], y = upper[i], pch="-", cex = CIcex*1.5)
  }

 for(l in 1:length(H0line))
  {lines(x=range(num)+c(-1,1), y=rep(H0line[l],times=2), lty=H0lty[l], lwd=H0lwd[l])}


 }


if(alternative=="greater")
 {
 for(i in 1:length(num))
  {
  lines(x = c(num[i],num[i]), y = c(lower[i], uplot), lty = CIlty, lwd=CIlwd)
  points(x = num[i], y = lower[i], pch="--", cex = CIcex*1.5)
  }

 for(l in 1:length(H0line))
  {lines(x=range(num)+c(-1,1), y=rep(H0line[l],times=2), lty=H0lty[l], lwd=H0lwd[l])}

 }


}



 # # # produce the CI vertical-PLOT

if( CIvert==FALSE )
 {

 


 plot.new()

# the default margin size in inches
   mymai <- par(no.readonly=TRUE)$mai

 # adjust margin left of the y axis according to length of comparison names
  ywidth<- 1.1 * max(strwidth(compnames, units = "inches", cex = par("cex.axis"))) 

 if (mymai[2] < ywidth) 
        mymai[2] <- ywidth
 par(mai=mymai, new=TRUE)

num<-rev(num)

plot(x = estimate, y = num, axes = FALSE, xlim = c(lplot, uplot), 
 type="p", pch=16, cex=CIcex,
 ylab=" ",
 xlab=xlab,
  main=main
 )


axis(side = 2, at = num, labels=compnames, las=2, ...)
axis(side=1, ...)
box()


if(alternative=="two.sided")
 {
 for(i in 1:length(num))
  {
  lines(y = c(num[i],num[i]), x = c(lower[i], upper[i]), lty = CIlty, lwd=CIlwd)
  points(y = num[i], x = lower[i], pch="[", cex = CIcex)
  points(y = num[i], x = upper[i], pch="]", cex = CIcex)
  }

 for(l in 1:length(H0line))
  {lines(y=range(num)+c(-1,1), x=rep(H0line[l],times=2), lty=H0lty[l], lwd=H0lwd[l])}

 }


if(alternative=="less")
 {
 for(i in 1:length(num))
  {
  lines(y = c(num[i],num[i]), x = c(lplot, upper[i]), lty = CIlty, lwd=CIlwd)
  points(y = num[i], x = upper[i], pch="]", cex = CIcex)
  }

 for(l in 1:length(H0line))
  {lines(y=range(num)+c(-1,1), x=rep(H0line[l],times=2), lty=H0lty[l], lwd=H0lwd[l])}

 }


if(alternative=="greater")
 {
 for(i in 1:length(num))
  {
  lines(y = c(num[i],num[i]), x = c(lower[i], uplot), lty = CIlty, lwd=CIlwd)
  points(y = num[i], x = lower[i], pch="[", cex = CIcex)
  }

 for(l in 1:length(H0line))
  {lines(y=range(num)+c(-1,1), x=rep(H0line[l],times=2), lty=H0lty[l], lwd=H0lwd[l])}

 }

}


}
# end ONE BY-GROUP





# # # several BY-groups:

if(length(byout)>1)
 {


  k <- length(bynames)
  
  upperk <- numeric()   
  lowerk <- numeric()
  estimatek <- numeric()
  compnamesk <- character()
  
   for(e in 1:k)
    {
    upperk <- c(upperk, byout[[e]]$upper)  
    lowerk <- c(lowerk, byout[[e]]$lower)     
    estimatek <- c(estimatek, byout[[e]]$estimate)     
    compnamesk <- c(compnamesk, paste(bynames[[e]], ":", byout[[e]]$compnames) ) 
    }
  num <- 1:length(estimatek)



 # # # all vectors of equal length??

# if( any( c(length(lowerk), length(upperk), length(estimatek)) != length(compnamesk) ) )
#  { stop("plot.pairwiseCI INTERNAL: length of any (lowerk, upperk,estimatek, compnamesk) is not the same") }

 # # # remove NAs from the vectors of upper or lower limit:
 # # # replace them by the corresponding estimates

 if( any(c("Param.ratio", "Lognorm.ratio", "HL.ratio", "HD.ratio", "Median.ratio", "prop.or", "prop.ratio") == method) )
  { 
  if( any(is.na(lowerk)) || any(is.na(upperk)) ) 
   
  lowerk[ which(is.na(lowerk)) ] <- estimatek[ which(is.na(lowerk)) ] 
  upperk[ which(is.na(upperk)) ] <- estimatek[ which(is.na(upperk)) ] 
  }


 # # # find the PLOT margins:
 if(alternative=="two.sided")
  {plotlim <- range(lowerk, estimatek, upperk, H0line)}

 if(alternative=="less")
  {plotlim <- range( estimatek, upperk, H0line)}

 if(alternative=="greater")
  {plotlim <- range(lowerk, estimatek, H0line)}

 # # # define MAIN, SUB, YLAB,. . .

if(is.null(main)){main <- " "}


 # # # produce the CI vertical-PLOT:

if( CIvert==TRUE )
 {

 plot.new()
 # the default margin size in inches
  mymai <- par(no.readonly=TRUE)$mai

 # adjust margin under the x axis according to length of comparison names
  xwidth<- 1.5 * max(strwidth(compnamesk, units = "inches", cex = par("cex.axis"))) 

 if (mymai[1] < xwidth) 
        mymai[1] <- xwidth
 par(mai=mymai, new=TRUE)


plot(x = num, y = estimatek, axes = FALSE, ylim = plotlim, 
 type="p", pch=16, cex=CIcex,
 ylab=ylab,
 xlab="",
  main=main
 )


axis(side = 1, at = num, labels=compnamesk, las=2 )
axis(side=2, ...)
box()


if(alternative=="two.sided")
 {
 for(i in 1:length(num))
  {
  lines(x = c(num[i],num[i]), y = c(lowerk[i], upperk[i]), lty = CIlty, lwd=CIlwd)
  points(x = num[i], y = lowerk[i], pch="-", cex = CIcex*1.5)
  points(x = num[i], y = upperk[i], pch="-", cex = CIcex*1.5)
  }

 for(l in 1:length(H0line))
  {lines(x=range(num)+c(-1,1), y=rep(H0line[l],times=2), lty=H0lty[l], lwd=H0lwd[l])}

 }


if(alternative=="less")
 {
 for(i in 1:length(num))
  {
  lines(x = c(num[i],num[i]), y = c(min(plotlim), upperk[i]), lty = CIlty, lwd=CIlwd)
  points(x = num[i], y = upperk[i], pch="-", cex = CIcex*1.5)
  }

 for(l in 1:length(H0line))
  {lines(x=range(num)+c(-1,1), y=rep(H0line[l],times=2), lty=H0lty[l], lwd=H0lwd[l])}


 }


if(alternative=="greater")
 {
 for(i in 1:length(num))
  {
  lines(x = c(num[i],num[i]), y = c(lowerk[i], max(plotlim)), lty = CIlty, lwd=CIlwd)
  points(x = num[i], y = lowerk[i], pch="--", cex = CIcex*1.5)
  }

 for(l in 1:length(H0line))
  {lines(x=range(num)+c(-1,1), y=rep(H0line[l],times=2), lty=H0lty[l], lwd=H0lwd[l])}

 }

}



 # # # produce the CI horizontal-PLOT

if( CIvert==FALSE )
 {



 plot.new()

 # the default margin size in inches
   mymai <- par(no.readonly=TRUE)$mai

 # adjust margin left of the y axis according to length of comparison names
  ywidth<- 1.5 * max(strwidth(compnamesk, units = "inches", cex = par("cex.axis"))) 

 if (mymai[2] < ywidth) 
        mymai[2] <- ywidth
 par(mai=mymai, new=TRUE)

num<-rev(num)

plot(x = estimatek, y = num, axes = FALSE, xlim = plotlim, 
 type="p", pch=16, cex=CIcex,
 ylab="",
 xlab=xlab,
  main=main
 )


axis(side = 2, at = num, labels=compnamesk, las=2 )
axis(side=1, ...)
box()


if(alternative=="two.sided")
 {
 for(i in 1:length(num))
  {
  lines(y = c(num[i],num[i]), x = c(lowerk[i], upperk[i]), lty = CIlty, lwd=CIlwd)
  points(y = num[i], x = lowerk[i], pch="[", cex = CIcex)
  points(y = num[i], x = upperk[i], pch="]", cex = CIcex)
  }

 for(l in 1:length(H0line))
  {lines(y=range(num)+c(-1,1), x=rep(H0line[l],times=2), lty=H0lty[l], lwd=H0lwd[l])}
 }


if(alternative=="less")
 {
 for(i in 1:length(num))
  {
  lines(y = c(num[i],num[i]), x = c(min(plotlim), upperk[i]), lty = CIlty, lwd=CIlwd)
  points(y = num[i], x = upperk[i], pch="]", cex = CIcex)
  }

 for(l in 1:length(H0line))
  {lines(y=range(num)+c(-1,1), x=rep(H0line[l],times=2), lty=H0lty[l], lwd=H0lwd[l])}

 }


if(alternative=="greater")
 {
 for(i in 1:length(num))
  {
  lines(y = c(num[i],num[i]), x = c(lowerk[i], max(plotlim)), lty = CIlty, lwd=CIlwd)
  points(y = num[i], x = lowerk[i], pch="[", cex = CIcex)
  }

 for(l in 1:length(H0line))
  {lines(y=range(num)+c(-1,1), x=rep(H0line[l],times=2), lty=H0lty[l], lwd=H0lwd[l])}

 }

}

}

par(old.par)

}

