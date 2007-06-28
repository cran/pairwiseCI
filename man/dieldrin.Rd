\name{dieldrin}
\alias{dieldrin}
\docType{data}
\title{ Dieldrin in fish of two different rivers}
\description{
  Observations below a detection limit: the dieldrin concentration (microg/kg) in roach fish (Rutilus rutilus) were measured in the Thames river near Oxford, UK and as a control in an upper arm, the river Ray 
}
\usage{data(dieldrin)}
\format{
  A data frame with 11 observations on the following 2 variables.
  \describe{
    \item{\code{River}}{a factor with 2 levels \code{aThames} \code{Ray}}
    \item{\code{dieldrin}}{a numeric vector giving the measured dieldrin concentration, where 0.09 was the detection limit}
  }
}
\source{
  Yamaguchi N. et al. Concentrations and hazard assessment of PCBs, organochlorine pesticides and mercury in fish species from the upper Thames: River pollution and its potential effects on top predators. Chemosphere 2003, 50, 265-273.
}

\examples{
data(dieldrin)
boxplot(dieldrin~River, data=dieldrin)
}
\keyword{datasets}