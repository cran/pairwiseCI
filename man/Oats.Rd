\name{Oats}
\alias{Oats}
\docType{data}
\title{The Oats data set}
\description{
The yield of three varieties of Oat was recorded in a field trial with 6 Blocks on 4 levels of nitrogen fertilization.
Originally a split plot design, here simply for demonstration of pairwiseCI.  

}
\usage{data(Oats)}
\format{
  A data frame with 72 observations on the following 4 variables.
  \describe{
    \item{\code{Block}}{an ordered factor with levels \code{VI} < \code{V} < \code{III} < \code{IV} < \code{II} < \code{I}}
    \item{\code{Variety}}{a factor with levels \code{Golden Rain} \code{Marvellous} \code{Victory}}
    \item{\code{nitro}}{a numeric vector}
    \item{\code{yield}}{a numeric vector}
  }
}
\source{See Oats(nlme).}

\examples{
data(Oats)
boxplot(yield ~ nitro*Variety, data=Oats)
}
\keyword{datasets}
