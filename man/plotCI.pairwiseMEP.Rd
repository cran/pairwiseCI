\name{plotCI.pairwiseMEP}
\alias{plotCI.pairwiseMEP}

\title{ Plot confidence intervals calculated by "pairwiseMEP"}

\description{
 Creates plot of confidence intervals calculated by calling "pairwiseMEP".
}

\usage{
\method{plotCI}{pairwiseMEP}(x, whichep = NULL, ...)
}

\arguments{
  \item{x}{ an object of class \kbd{'pairwiseMEP'} as can be created by calling \code{\link{pairwiseMEP}} }
  \item{whichep}{ an optional vector of character strings (or integers); specifying the names (or indices in the element \kbd{conf.int} of the list returned by \kbd{pairwiseMEP}) of those response variables for which the confidence intervals shall be plotted }
  \item{\dots}{ further arguments to be passed to \kbd{plotCI} in package \pkg{MCPAN}, see \kbd{?plotCI} for details}
}

\value{
A plot.
}
\examples{


x1<-rnorm(120,20,2)
x2<-rnorm(120,100,8)
x3<-rpois(120,10)
x4<-rpois(120,50)
A<-rep(c("a1","a2","a3"), c(40,40,40))
B<-rep(rep(c("b1","b2","b3","b4"), c(10,10,10,10)), times=3)
dat<-data.frame(x1=x1,x2=x2,x3=x3,x4=x4,A=A, B=B)


test<-pairwiseMEP(x=dat, ep=c("x1","x2","x3", "x4"),
 f="A", by="B", conf.level=0.9, control="a1",
 method=c("Param.ratio","Param.ratio","Poisson.ratio","Poisson.ratio"))

plotCI(test, whichep=c("x1","x2"), lines=c(0.8,1.25))

plotCI(test, whichep=c(3,4))


}
\keyword{ hplot }
\concept{ confidence intervals}
