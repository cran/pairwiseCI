\name{as.data.frame.pairwiseCI}
\alias{as.data.frame.pairwiseCI}

\title{Coercing pairwiseCI objects to data.frames}
\description{
 Creates a data.frame from the output of pairwiseCI.
}
\usage{
\method{as.data.frame}{pairwiseCI}(x, ...)
}

\arguments{
  \item{x}{ an object of class \dQuote{pairwiseCI} }
  \item{\dots}{ currently not used }
}

\value{
  A data.frame with the columns

  \item{estimate }{ containing the estimates}
  \item{lower }{containing the lower bounds}
  \item{upper }{containing the upper bounds}
  \item{comparison }{containing character strings, specifying which levels have been compared}
and if the argument by has been specified, 
  \item{by }{containing the levels by which the original data set has been split}

and the conf.level and a character string naming the used method.

}


\seealso{ \code{\link{pairwiseCI}}, \code{\link{summary.pairwiseTest}} }
\examples{

data(bean)

out<-pairwiseCI(yield~P, data=bean, by="T", method="Param.diff",
 var.equal=FALSE, alternative="greater")

out

dat<-as.data.frame(out)

dat

str(dat)

# # # #

data(repellent)

out2<-pairwiseCI(decrease~treatment, data=repellent, control="H",
 alternative="two.sided", method="Param.ratio")

out2

as.data.frame(out2)



}
\keyword{ misc }