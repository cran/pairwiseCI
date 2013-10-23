\name{as.data.frame.pairwiseMEP}
\alias{as.data.frame.pairwiseMEP}

\title{Coerce pairwiseMEP objects to data.frames}
\description{
 Coerces the output of the function pairwiseMEP to a data.frame. 
}
\usage{
\method{as.data.frame}{pairwiseMEP}(x, row.names = NULL,
 optional = FALSE, whichep = NULL, ...)
}

\arguments{
  \item{x}{an object of class \dQuote{pairwiseMEP} as can be obtained by calling \code{\link{pairwiseMEP}} }
  \item{row.names}{as in \code{as.data.frame}}
  \item{optional}{as in \code{as.data.frame}}
  \item{whichep}{ a vector of integers or character strings, indexing which endpoints (which response variables)
from object \code{x} shall be coerced to a data.frame; if omitted (default), all endpoints are coerced to a data.frame }
  \item{\dots}{ Further arguments to be passed to \code{as.data.frame} }
}

\value{
  A data.frame with columns

  \item{estimate }{ numeric, the point estimates }
  \item{lower }{ numeric, the lower confidence limits  }
  \item{upper }{ numeric, the upper confidence limits }
  \item{comparison }{ character, the name of the groupwise comparison }
  \item{by }{ optional, character, the name of subset of the original data.frame}
  \item{response}{ character, the name of the response variable }
  \item{method }{ character, the name of the method used for calculation of the lower and upper limits, see \code{\link{pairwiseMEP}} }
}

\examples{

x1<-rnorm(120,20,2)
x2<-rnbinom(120,mu=10, size=10)
A<-rep(c("a1","a2","a3"), c(40,40,40))
B<-rep(rep(c("b1","b2"), c(20,20)), times=3)

dat<-data.frame(x1=x1, x2=x2, A=A, B=B)

test<-pairwiseMEP(x=dat,
 ep=c("x1","x2"),
 f="A", by="B",
 method=c("Param.ratio","Negbin.ratio"))

test

as.data.frame(test)

as.data.frame(test, whichep=c(1,2))

as.data.frame(test, whichep=c("x1","x2"))



}

\keyword{ misc }

