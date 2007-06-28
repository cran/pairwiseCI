\name{pairwiseCImethodsProp}

\alias{pairwiseCImethodsProp}

\alias{Prop.diff}
\alias{Prop.ratio}
\alias{Prop.or}

\title{ Different confidence interval methods available for pairwiseCI for comparison of two independent samples}
\description{
Methods for binomial variables.
}
\usage{

Prop.diff(x, y, conf.level=0.95, alternative="two.sided", ...)
Prop.ratio(x, y, conf.level=0.95, alternative="two.sided")
Prop.or(x, y, conf.level=0.95, alternative="two.sided")

}

\arguments{
  \item{x}{ observations of the first sample: either a vector with number of success and failure, or a data.frame with two columns (the success and failures)) }
  \item{y}{ observations of the second sample: either a vector with number of success and failure, or a data.frame with two columns (the success and failures)) }
  \item{alternative}{ character string, either "two.sided", "less" or "greater"  }
  \item{conf.level}{ the comparisonwise confidence level of the intervals, where 0.95 is default }
  \item{\dots}{further arguments to be passed to the individual methods, see details}
}

\details{
\itemize{
\item \code{Prop.diff} calculates the confidence interval for the difference
 of proportions by calling \code{prop.test} in package \pkg{stats}, where the input are two
 vectors x and y giving the number of successes and failures in the two samples; 


\item \code{Prop.ratio} calculates the confidence interval for the ratio
 of proportions according to Dann and Koch (2005);
 

\item \code{Prop.or} calculates the adjusted Woolf confidence interval for
 the odds ratio of proportions according to Lawson (2005). 
}
}


\value{
A list containing:

  \item{conf.int}{a vector containing the lower and upper confidence limit}
  \item{estimate}{a single named value}
}


\references{ 
\itemize{
\item \code{Prop.diff} uses \code{prop.test} in \pkg{stats}.

\item \code{Prop.ratio}
\bold{Dann, RS and Koch, GG (2005)}: Review and evaluation of methods for computing confidence intervals for the ratio of two proportions and considerations for non-inferiority clinical trials.
 Journal of Biopharmaceutical Statistics, 15, 85-107.

\item \code{Prop.or} e.g. in:
\bold{Lawson, R (2005)}: Small sample confidence intervals for the odds ratio.
 Communication in Statistics Simulation and Computation, 33, 1095-1113.
 }
}

\author{ Frank Schaarschmidt, Institute of Biostatistics, Leibniz University of Hannover, Germany }


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

Prop.diff(x=Age5_PosB_IBA0,y=Age5_PosB_IBA0.5)
Prop.or(x=Age5_PosB_IBA0,y=Age5_PosB_IBA0.5)
Prop.ratio(x=Age5_PosB_IBA0,y=Age5_PosB_IBA0.5)

# is the same as input two vectors x,y each containing
# the count of successes and the count of failures

 colSums(Age5_PosB_IBA0)
 colSums(Age5_PosB_IBA0.5)

Prop.diff(x=c(16,32),y=c(29,19))
Prop.or(x=c(16,32),y=c(29,19))
Prop.ratio(x=c(16,32),y=c(29,19))

 }

\keyword{ htest }