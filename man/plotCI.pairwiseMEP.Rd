\name{plotCI.pairwiseMEP}
\alias{plotCI.pairwiseMEP}

\title{ Plot confidence intervals calculated by "pairwiseMEP".}
\description{
 Creates plot of confidence intervals calculated by calling "pairwiseMEP".
}

\usage{
\method{plotCI}{pairwiseMEP}(x, whichep = NULL, ...)
}

\arguments{
  \item{x}{ an object of class \code{'pairwiseMEP'} as can be created by calling \code{\link{pairwiseMEP}} }
  \item{whichep}{ an optional vector of character strings (or integers); specifying the names (or indices in \code{x$conf.int}) of those response variables for which the confidence intervals shall be plotted }
  \item{\dots}{ further arguments to be passed to \code{plotCI} in package \pkg{MCPAN}, see \code{?plotCI} for details}
}

\value{
A plot.
}
\examples{


x1<-rnorm(120,20,2)
x2<-rnorm(120,100,8)
x3<-rpois(120,10)
x4<-rnbinom(120,mu=10, size=10)
A<-rep(c("a1","a2","a3"), c(40,40,40))
B<-rep(rep(c("b1","b2","b3","b4"), c(10,10,10,10)), times=3)
dat<-data.frame(x1=x1,x2=x2,x3=x3,x4=x4,A=A, B=B)


test<-pairwiseMEP(x=dat, ep=c("x1","x2","x3", "x4"), f="A", by="B")

plotCI(test, whichep=c("x1","x2"), lines=c(-5,5))

plotCI(test, whichep=c(3,4))


}
\keyword{ hplot }
\concept{ confidence intervals}
