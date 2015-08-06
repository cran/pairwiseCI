\name{QBmover}
\alias{QBmover}

\title{Confidence intervals for ratios of proportions based on the quasibinomial assumption}

\description{
Confidence intervals for ratios of proportions with overdispersed binomial data in a one-factor quasibinomial generalized linear model.
Intervals are computed using the MOVER-R method on profile deviance intervals (as implemented in mcprofile) for the single proportions.
}

\usage{
QBmover(succ, fail, trt, conf.level = 0.95,
 alternative = "two.sided", grid = NULL)
}

\arguments{
  \item{succ}{vector of counts of successes}
  \item{fail}{vector of counts of failures}
  \item{trt}{factor variable distinguishing the treatment groups}
  \item{conf.level}{a single numeric value, the confidence level}
  \item{alternative}{a character string, \code{"two.sided"} for two-sided intervals, \code{"less"} for upper limits, \code{"greater"} for lower limits only}
  \item{grid}{optional, a numeric vector to be supplied to the profiling used internally in \code{quasibin.ratio} to obtain profile deviance intervals for each samples proportion on the logit-scale.}
}

\value{
A data.frame with three columns
\item{est }{estimated ratios}
\item{lower }{lower confidence limits}
\item{upper }{upper confidence limits}
}

\references{
\emph{Donner and Zou (2012):} Closed-form confidence intervals for functions of the normal mean and standard deviation. Statistical Methods in Medical Research 21(4):347-359.
\emph{Gerhard (2014):} Simultaneous Small Sample Inference For Linear Combinations Of Generalized Linear Model Parameters. Communications in Statistics - Simulation and Computation. DOI:10.1080/03610918.2014.895836}

\author{Frank Schaarschmidt}

\note{Experimental}

\examples{
QBmover(succ=c(0,0,1,  0,6,8), fail=c(20,20,18, 20,14,12), 
 trt=factor(rep(c("A", "B"), c(3,3))), conf.level = 0.95,
 alternative = "two.sided", grid = NULL)

}

\keyword{htest}
