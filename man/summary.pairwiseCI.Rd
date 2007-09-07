\name{summary.pairwiseCI}

\alias{summary.pairwiseCI}

\title{ Summary function for pairwiseCI}
\description{ Creates a list of data.frames from the output of pairwiseCI}
\usage{
\method{summary}{pairwiseCI}(object, digits = 4, ...)
}

\arguments{
  \item{object}{ An object of class "pairwiseCI", created using the function \code{\link{pairwiseCI}} }
  \item{digits}{ number of digits for rounding of results }
  \item{\dots}{ Currently not used. }
}

\value{
A list.
}

\author{ Frank Schaarschmidt }


\examples{

data(rooting)

rootRR<-pairwiseCI(cbind(root,noroot) ~ IBA,
 data=rooting, by="Age", method="Prop.ratio")

# after calling summary,
# extracting parts of the output is easier: 

srootRR<-summary(rootRR)

srootRR$'20'$conf.int$upper

}
\keyword{ print }
