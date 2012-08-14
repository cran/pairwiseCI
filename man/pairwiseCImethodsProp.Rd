\name{pairwiseCImethodsProp}

\alias{pairwiseCImethodsProp}

\alias{Prop.diff}
\alias{Prop.ratio}
\alias{Prop.or}

\title{ Confidence intervals for two sample comparisons of binomial proportions}
\description{
For the comparison of two independent samples of binomial observations, confidence intervals for
the difference (RD), ratio (RR) and odds ratio (OR) of proportions are implemented.
}
\usage{

Prop.diff(x, y, conf.level=0.95, alternative="two.sided", CImethod=c("NHS", "CC", "AC"), ...)
Prop.ratio(x, y, conf.level=0.95, alternative="two.sided", CImethod=c("Score", "GNC"))
Prop.or(x, y, conf.level=0.95, alternative="two.sided", CImethod=c("Exact", "Woolf"), ...)

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
Generally, the input are two vectors \kbd{x} and \kbd{y} giving the number of successes and failures in the two samples,
 or, alternatively, two \kbd{data.frame}s \kbd{x} and \kbd{y} each containing one column for the successes 
and one column for the failures, and the rows containing repeated observations from the same treatment.
Please note, that except for function \kbd{prop.or} with \kbd{CImethod="Quasibinomial"} the confidence intervals available in this function
 are based on sums over the rows of \kbd{x} and \kbd{y} and hence do NOT APPROPRIATELY account for extra-binomial variability between repeated observations for the same treatment! 

  NOTE: When there are repeated observations (input as a data.frame with several rows), intervals are calculated based on the sums over the rows!
  
\itemize{
\item \kbd{Prop.diff} calculates the asymptotic Continuity Corrected confidence interval for the difference
 of proportions by calling \kbd{prop.test} in package \pkg{stats}. See \kbd{?prop.test} for details.

\item \kbd{Prop.diff} with \kbd{CImethod="AC"} Calculates the Agresti-Caffo-Interval (Agresti and Caffo, 2000).

\item \kbd{Prop.diff} with \kbd{CImethod="NHS"} Calculates Newcombes Hybrid Score Interval (Newcombe, 1998).

\item \kbd{Prop.ratio} with \kbd{CImethod="GNC"} calculates the crude interval for the ratio of proportions 
 according to Gart and Nam (1988), based on normal approximation on the log-scale.

\item \kbd{Prop.ratio} with \kbd{CImethod="Score"} calculates the Score interval for the ratio of proportions 
 according to Gart and Nam (1988), based on a Chi-Square approximation. These methods do NOT involve skewness corrections and extensions to stratification.

\item \kbd{Prop.or} with \kbd{CImethod="Woolf"} calculates the adjusted Woolf confidence interval for
 the odds ratio of proportions as, e.g., described in Lawson (2005).

\item \kbd{Prop.or} with \kbd{CImethod="Exact"} calculates the exact confidence interval for
 the odds ratio of proportions corresponding to Fishers exact test, by calling to \kbd{fisher.test} in \pkg{stats}. For details, see \kbd{?fisher.test}.

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

# # # 

# Comparison with original papers:

# Risk difference:

# Risk difference, CC

# Continuity corrected interval:

# 1.Comparison with results presented in Newcombe (1998),
# Table II, page 877, 10. Score, CC
# column 1 (a): 56/70-48/80: [0.0441; 0.3559]


Prop.diff(x=c(56,70-56),y=c(48,80-48), alternative="two.sided",
 conf.level=0.95, CImethod="CC")


# I. Risk difference, NHS

# Newcombes Hybrid Score interval:

# 1.Comparison with results presented in Newcombe (1998),
# Table II, page 877, 10. Score, noCC
# column 1 (a): 56/70-48/80: [0.0524; 0.3339]


Prop.diff(x=c(56,70-56),y=c(48,80-48), alternative="two.sided",
 conf.level=0.95, CImethod="NHS")


Prop.diff(x=c(56,70-56),y=c(48,80-48), alternative="greater",
 conf.level=0.975, CImethod="NHS")

Prop.diff(x=c(56,70-56),y=c(48,80-48), alternative="less",
 conf.level=0.975, CImethod="NHS")


# 2.Comparison with results presented in Newcombe (1998),
# Table II, page 877, 10. Score, noCC
# column 2 (b): 9/10-3/10: [0.1705; 0.8090]


Prop.diff(x=c(9,1),y=c(3,7), alternative="two.sided",
 conf.level=0.95, CImethod="NHS")


# 3.Comparison with results presented in Newcombe (1998),
# Table II, page 877, 10. Score, noCC
# column 2 (h): 10/10-0/10: [0.6075; 1.000]


Prop.diff(x=c(10,0),y=c(0,10), alternative="two.sided",
 conf.level=0.95, CImethod="NHS")


# II. Risk ratio, Score
# Score interval according to Gart and Nam (1988)

# 1.Comparison with results presented in Gart and Nam (1998),
# Section 5 (page 327), Example 1
# x1/n1=8/15 x0/n0=4/15:
# Log: [0.768, 4.65]
# Score: [0.815; 5.34]

# Log (GNC)
Prop.ratio(x=c(8,7),y=c(4,11), alternative="two.sided",
 conf.level=0.95, CImethod="GNC")

# Score (Score)
Prop.ratio(x=c(8,7),y=c(4,11), alternative="two.sided",
 conf.level=0.95, CImethod="Score")

Prop.ratio(x=c(8,7),y=c(4,11), alternative="less",
 conf.level=0.975, CImethod="Score")

Prop.ratio(x=c(8,7),y=c(4,11), alternative="greater",
 conf.level=0.975, CImethod="Score")



# 2.Comparison with results presented in Gart and Nam (1998),
# Section 5 (page 328), Example 2
# x1/n1=6/10 x0/n0=6/20:
# Log: [0.883, 4.32]
# Score: [0.844; 4.59]

# Log (GNC)
Prop.ratio(x=c(6,4),y=c(6,14), alternative="two.sided",
 conf.level=0.95, CImethod="GNC")

# Score (Score)
Prop.ratio(x=c(6,4),y=c(6,14), alternative="two.sided",
 conf.level=0.95, CImethod="Score")


 }

\keyword{ htest }