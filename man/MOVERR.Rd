\name{MOVERR}
\alias{MOVERR}
\title{
MOVER-R method by Donner and Zhou (2012)
}
\description{
Compute confidence intervals for the ratio (theta1/theta0) of two parameters based on point estimates and confidence intervals for the two parameters, theta1 and theta0.
}
\usage{
MOVERR(theta0, ci0, theta1, ci1, alternative = "two.sided")
}

\arguments{
  \item{theta0}{a single numeric vale, the point estimate for the parameter to appear in the denominator}
  \item{ci0}{ vector with two numeric values, the lower and upper confidence limits for the parameter to appear in the denominator}
  \item{theta1}{a single numeric vale, the point estimate for the parameter to appear in the numerator}
  \item{ci1}{vector with two numeric values, the lower and upper confidence limits for the parameter to appear in the numerator}
  \item{alternative}{a character string, \code{"two.sided"} for two-sided intervals, \code{"less"} for upper limits, \code{"greater"} for lower limits only}
}
\details{
This function implements Eq. (9) in Donner and Zou (2012), for computing confidence intervals for a ratio of two parameters, when their estimators are uncorrelated.
}
\value{
a list with elements
\item{conf.int}{numeric, the lower and upper confidence limits for the ratio}
\item{estimate}{the point estimate for the ratio}
and the input estimates and confidence limits
}
\references{
\emph{Donner and Zou (2012):} Closed-form confidence intervals for functions of the normal mean and standard deviation. Statistical Methods in Medical Research 21(4):347-359.
}
\author{
Frank Schaarschmidt
}

\keyword{ htest }
