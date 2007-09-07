\name{repellent}
\alias{repellent}
\docType{data}
\title{ Repellent effect of sulphur in sugar solutions }
\description{ Sugar solutions were presented to certain number of bees. To assess the repellent effect
of the fungicide sulphur of bees, increasing concentration of sulphur was added to the sugar solutions.
The decrease of sugar solutions (i.e. uptake by bees) was measured. Low values of decrease therefore can
 be interpreted as high repellent effect of the sulphur concentration.
Assumed to be a completely randomized design. 
}
\usage{data(repellent)}
\format{
  A data frame with 64 observations on the following 2 variables.
  \describe{
    \item{\code{decrease}}{a numeric vector, the absolut decrease of sugar solutions from begin to end of the experiment}
    \item{\code{treatment}}{a factor with levels \code{A} \code{B} \code{C} \code{D} \code{E} \code{F} \code{G} \code{H}, the concentration of sulphur}
  }
}

\examples{
data(repellent)
boxplot(decrease ~ treatment, data=repellent)

}
\keyword{datasets}
