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

\keyword{ misc }

