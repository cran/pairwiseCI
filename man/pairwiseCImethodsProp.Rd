\name{pairwiseCImethodsProp}

\alias{pairwiseCImethodsProp}

\alias{Prop.diff}
\alias{Prop.ratio}
\alias{Prop.or}

\title{ Confidence interval methods implemented for proportions in pairwiseCI}
\description{
For the comparison of two independent samples of binomial observations, confidence intervals for
the difference (RD), ratio (RR) and odds ratio (OR) of proportions are implemented.
}
\usage{

Prop.diff(x, y, conf.level=0.95, alternative="two.sided", CImethod="CC", ...)
Prop.ratio(x, y, conf.level=0.95, alternative="two.sided", CImethod="GNC")
Prop.or(x, y, conf.level=0.95, alternative="two.sided", CImethod="Woolf")

}

\arguments{
  \item{x}{ observations of the first sample: either a vector with number of success and failure, or a data.frame with two columns (the success and failures)) }
  \item{y}{ observations of the second sample: either a vector with number of success and failure, or a data.frame with two columns (the success and failures)) }
  \item{alternative}{ character string, either "two.sided", "less" or "greater"  }
  \item{conf.level}{ the comparisonwise confidence level of the intervals, where 0.95 is default }
  \item{CImethod}{a single character string, see below for details}
  \item{\dots}{further arguments to be passed to the individual methods, see details}
}

\details{
Generally, the input are two vectors \code{x} and \code{y} giving the number of successes and failures in the two samples,
 or, alternatively, two \code{data.frame}s \code{x} and \code{y} each containing one column for the successes 
and one column for the failures, and the rows containing repeated observations from the same treatment.
Please note, that except for function \code{prop.or} with \code{CImethod="Quasibinomial"} the confidence intervals available in this function
 are based on sums over the rows of \code{x} and \code{y} and hence do NOT APPROPRIATELY account for extra.binomial variability between repeated observations for the same treatment! 

\itemize{
\item \code{Prop.diff} calculates the asymptotic Continuity Corrected confidence interval for the difference
 of proportions by calling \code{prop.test} in package \pkg{stats}.
 NOTE: When there are repeated observations (input as a data.frame with several rows), intervals are calculated based on the sums over the rows!

\item \code{Prop.diff} with \code{CImethod="AC"} Calculates the Agresti-Caffo-Interval (Agresti and Caffo, 2000).
 NOTE: When there are repeated observations (input as a data.frame with several rows), intervals are calculated based on the sums over the rows!

\item \code{Prop.diff} with \code{CImethod="NHS"} Calculates Newcombes Hybrid Score Interval (Newcombe, 1998).
 NOTE: When there are repeated observations (input as a data.frame with several rows), intervals are calculated based on the sums over the rows!

\item \code{Prop.ratio} with \code{CImethod="GNC"} calculates the crude interval for the ratio of proportions 
 according to Gart and Nam (1988), based on normal approximation on the log-scale.
 NOTE: When there are repeated observations (input as a data.frame with several rows), intervals are calculated based on the sums over the rows!

\item \code{Prop.ratio} with \code{CImethod="Score"} calculates the Score interval for the ratio of proportions 
 according to Gart and Nam (1988), based on a Chi-Square approximation.
  NOTE: When there are repeated observations (input as a data.frame with several rows), intervals are calculated based on the sums over the rows!

\item \code{Prop.or} with \code{CImethod="Woolf"} calculates the adjusted Woolf confidence interval for
 the odds ratio of proportions as, e.g., described in Lawson (2005).
 NOTE: When there are repeated observations (input as a data.frame with several rows), intervals are calculated based on the sums over the rows!

}
}


\value{
A list containing:

  \item{conf.int}{a vector containing the lower and upper confidence limit}
  \item{estimate}{a single named value}
}


\references{ 
\itemize{

\item \bold{Newcombe R.G. (1998):} Interval Estimation for the Difference Between Independent Proportions: Comparison of Eleven Methods. Statistics in Medicine 17, 873-890. 

\item \bold{Agresti, A. and Caffo, B. (2000):} Simple and effective confidence intervals for proportions and differences of proportions result from adding two successes and two failures. 
American Statistician 54 (4), 280-288.

\item \bold{Gart, JJ and Nam, J (1988)}: Approximate interval estimation of the ratio of binomial parameters: A review and corrections for skewness.
 Biometrics 44, 323-338.

\bold{Dann, RS and Koch, GG (2005)}: Review and evaluation of methods for computing confidence intervals for the ratio of two proportions and considerations for non-inferiority clinical trials.
 Journal of Biopharmaceutical Statistics, 15, 85-107.

\item \bold{Lawson, R (2005)}: Small sample confidence intervals for the odds ratio.
 Communication in Statistics Simulation and Computation, 33, 1095-1113.

\item \bold{Venables WN and Ripley BD (2002). Modern Applied Statistics with S. Fourth Edition. Springer New York.} 
 }
}

\examples{

# The rooting data.

data(rooting)

# the first comparison should be the same as:

Age5_PosB_IBA0 <- subset(rooting,
 Age=="5" & Position=="B" & IBA=="0")[,c("root", "noroot")]
Age5_PosB_IBA0.5 <- subset(rooting,
 Age=="5" & Position=="B" & IBA=="0.5")[,c("root", "noroot")]

Age5_PosB_IBA0
Age5_PosB_IBA0.5

Prop.diff(x=Age5_PosB_IBA0, y=Age5_PosB_IBA0.5)

Prop.ratio(x=Age5_PosB_IBA0, y=Age5_PosB_IBA0.5)

Prop.or(x=Age5_PosB_IBA0, y=Age5_PosB_IBA0.5)

# is the same as input two vectors x,y each containing
# the count of successes and the count of failures

 colSums(Age5_PosB_IBA0)
 colSums(Age5_PosB_IBA0.5)

Prop.diff(x=c(16,32),y=c(29,19))

Prop.ratio(x=c(16,32),y=c(29,19))

Prop.or(x=c(16,32),y=c(29,19))

 }

\keyword{ htest }