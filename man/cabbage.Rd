\name{cabbage}
\alias{cabbage}
\docType{data}
\title{Yield of cabbage in one-factorial field trial}
\description{
Cabbage yield in dependence of four doses of fertilizer. 
}
\usage{data(cabbage)}
\format{
  A data frame with 20 observations on the following 2 variables.
  \describe{
    \item{\code{Fert}}{a factor with levels \code{D1} \code{D2} \code{D3} \code{D4}, the different doses of fertilizer}
    \item{\code{yield}}{a numeric vector, cabbage yield}
  }
}

\examples{
data(cabbage)
boxplot(yield~Fert, data=cabbage)
}
\keyword{datasets}
