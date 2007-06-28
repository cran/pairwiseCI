\name{sodium}
\alias{sodium}
\docType{data}
\title{Sodium measurements in transgenic and isogenic corn ???} 
\description{
  
}
\usage{data(sodium)}
\format{
  A data frame with 12 observations on the following 2 variables.
  \describe{
    \item{\code{Treatment}}{a factor with levels \code{transgenic} \code{xisogenic}}
    \item{\code{Sodiumcontent}}{a numeric vector}
  }
}
\details{
}
\source{
  ???? Oberdoerfer, R.B. Example dataset from composition analyses of genetically modified oilseed rape seeds. 2003; BCS GmbH.
}

}
\examples{

data(sodium)
boxplot(Sodiumcontent ~Treatment, data=sodium)

}
\keyword{datasets}


