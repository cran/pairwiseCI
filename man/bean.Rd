\name{bean}
\alias{bean}
\docType{data}
\title{Yield of beans in a design with one block and two treatment factors}
\description{
  Block design with four blocks. Within the blocks a 2x3 factorial design was randomized: Two types of 
horse beans (Vicia faba) were grown under three fertilization levels of phosphor.
 
}
\usage{data(bean)}
\format{
  A data frame with 24 observations on the following 4 variables.
  \describe{
    \item{\code{block}}{a numeric vector}
    \item{\code{T}}{a factor with levels, describing the plant type, where \code{T1} = short, and \code{T2} = tall}
    \item{\code{P}}{a factor with levels, the phosphorus rate with levels \code{P0} = 0, \code{P25} = 25 kg/ha \code{P50} = 50 kg/ha}
    \item{\code{yield}}{a numeric vector, giving the mean yields of beans in kg/ha}
  }
}
\details{
  
}
\source{
Petersen, RG: Design and Analysis of experiments. Marcel Dekker, Inc.
}
\examples{
data(bean)
boxplot(yield ~ P*T, data=bean)
}
\keyword{datasets}
