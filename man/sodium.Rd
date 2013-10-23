\name{sodium}
\alias{sodium}
\docType{data}
\title{Sodium contents in transgenic and isogenic corn} 
\description{
 Sodium was measured in transgenic corn and the original isogenic corn variety.
}
\usage{data(sodium)}
\format{
  A data frame with 12 observations on the following 2 variables.
  \describe{
    \item{\code{Treatment}}{a factor with levels \code{transgenic} \code{xisogenic}}
    \item{\code{Sodiumcontent}}{a numeric vector}
  }
}

\source{
 \bold{Oberdoerfer, R.B.} Example dataset from composition analyses of genetically modified oilseed rape seeds. 2003; BCS GmbH.
}

\examples{

data(sodium)
boxplot(Sodiumcontent ~Treatment, data=sodium)

}
\keyword{datasets}


