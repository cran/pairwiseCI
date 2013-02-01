\name{pairwiseCImethodsProp}

\alias{pairwiseCImethodsProp}

\alias{Prop.diff}
\alias{Prop.ratio}
\alias{Prop.or}

\title{Confidence intervals for two sample comparisons of binomial proportions}
\description{
For the comparison of two independent samples of binomial observations, confidence intervals for
the difference (RD), ratio (RR) and odds ratio (OR) of proportions are implemented.
}
\usage{

Prop.diff(x, y, conf.level=0.95, alternative="two.sided",
 CImethod=c("NHS", "CC", "AC"), ...)

Prop.ratio(x, y, conf.level=0.95, alternative="two.sided",
 CImethod=c("Score", "MNScore", "MOVER", "GNC"))

Prop.or(x, y, conf.level=0.95, alternative="two.sided",
 CImethod=c("Exact", "Woolf"), ...)

}

\arguments{
  \item{x}{ observations of the first sample: either a vector with number of successes and failures, or a data.frame with two columns (the successes and failures)) }
  \item{y}{ observations of the second sample: either a vector with number of successes and failures, or a data.frame with two columns (the successes and failures)) }
  \item{alternative}{ character string, either "two.sided", "less" or "greater"  }
  \item{conf.level}{ the comparisonwise confidence level of the intervals, where 0.95 is the default }
  \item{CImethod}{a single character string, see below for details}
  \item{\dots}{further arguments to be passed to the individual methods, see details}
}

\details{
Generally, the input are two vectors \kbd{x} and \kbd{y} giving the number of successes and failures in the two samples,
 or, alternatively, two \kbd{data.frame}s \kbd{x} and \kbd{y} each containing one column for the successes 
and one column for the failures, and the rows containing repeated observations from the same treatment.

Please note, that the confidence intervals available in this function assume counts of successes and failures from a binomial distribution and thus do NOT APPROPRIATELY account for extra-binomial variability between repeated observations for the same treatment! 
When there are repeated observations (input as a data.frame with several rows), intervals are calculated based on the sums over the rows of success and failure!

The following methods are available for the risk difference:

\itemize{
\item \kbd{Prop.diff}: asymptotic continuity corrected confidence interval for the difference of proportions by calling \kbd{prop.test} in package \pkg{stats}, see \kbd{?prop.test} for details,
\item \kbd{Prop.diff} with \kbd{CImethod="AC"}: asymptotic Wald-type interval after adding one pseudo-observation to each cell (Agresti and Caffo, 2000).
\item \kbd{Prop.diff} with \kbd{CImethod="NHS"}: asymptotic Newcombes Hybrid Score Interval (Newcombe, 1998).
}

For the risk ratio:
\itemize{
\item \kbd{Prop.ratio} with \kbd{CImethod="Score"}:  asymptotic Score interval for the ratio of proportions (by iteratively inverting a Chi-Squared test) according to Koopman (1984), following the description by Gart and Nam (1988). This method does NOT involve the skewness correction or extensions to stratification described in Gart and Nam (1988).
\item \kbd{Prop.ratio} with \kbd{CImethod="MNScore"}: asymptotic Score interval for the ratio of proportions (by iteratively inverting a Chi-Squared test) according to Miettinen and Nurminen (1985), following the description by Gart and Nam (1988). 
\item \kbd{Prop.ratio} with \kbd{CImethod="MOVER"}: asymptotic method of variance estimate recovery for ratios (Donner and Zou, 2012; Fagerland and Newcombe, 2013), relying on the Wilson Score interval to obtain the confidence limits for the single proportions.
\item \kbd{Prop.ratio} with \kbd{CImethod="GNC"}: crude normal approximation on the log-scale after adding 0.5 to the observed number of successes and the samples sizes.
}

For the odds ratio:
\itemize{
\item \kbd{Prop.or} with \kbd{CImethod="Woolf"} asymptotic adjusted Woolf confidence interval for the odds ratio of proportions (normal approximation after adding 0.5 to each cell count), as, e.g., described in Lawson (2005).
\item \kbd{Prop.or} with \kbd{CImethod="Exact"} calculates the exact confidence interval for the odds ratio of proportions corresponding to Fishers exact test, by calling to \kbd{fisher.test} in \pkg{stats}. For details, see \kbd{?fisher.test}.
}
}

\seealso{An alternative implementation of the Score interval for the risk ratio in package \kbd{propCIs}, function \kbd{riskscoreci}.}

\value{
A list containing:
  \item{conf.int}{a vector containing the lower and upper confidence limit, and the methods name as an attribute}
  \item{estimate}{a single named value}
  \item{quantile}{the quantile used for constructing the interval}
  \item{conf.level}{the confidence level}
}


\references{ 
\itemize{
\item \bold{Agresti A and Caffo B (2000):} Simple and effective confidence intervals for proportions and differences of proportions result from adding two successes and two failures. American Statistician 54 (4), 280-288.
\item \bold{Donner A and Zou GY (2012):} Closed-form confidence intervals for functions of the normal mean and standard deviation.Statistical Methods in Medical Research 21(4):347-359.
\item \bold{Fagerland MW, Newcombe RG (2013):} Confidence intervals for odds ratio and relative risk based on the inverse hyperbolic sine transformation. Statistics in Medicine, DOI: 10.1002/sim.5714
\item \bold{Gart JJ and Nam J (1988)}: Approximate interval estimation of the ratio of binomial parameters: A review and corrections for skewness. Biometrics 44, 323-338.
\item \bold{Koopman, PAR (1984):} Confidence Intervals for the Ratio of Two Binomial Proportions. Biometrics 40(2), 513-517.
\item \bold{Lawson R (2005)}: Small sample confidence intervals for the odds ratio. Communication in Statistics Simulation and Computation, 33, 1095-1113.
\item \bold{Miettinen O and Nurminen M (1985):} Comparative Analysis of Two Rates. Statistics in Medicine 4, 213-226.
\item \bold{Newcombe RG (1998):} Interval Estimation for the Difference Between Independent Proportions: Comparison of Eleven Methods. Statistics in Medicine 17, 873-890. 
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

# Comparison with original publications:

# I. Risk difference:

# Continuity corrected interval:

# 1.Comparison with results presented in Newcombe (1998),
# Table II, page 877, 10. Score, CC
# column 1 (a): 56/70-48/80: [0.0441; 0.3559]

Prop.diff(x=c(56,70-56),y=c(48,80-48), alternative="two.sided",
 conf.level=0.95, CImethod="CC")


# Risk difference, NHS
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


# II. Risk ratio,
# Score interval according to Koopman(1984), Gart and Nam (1988)

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
# Crude Log: [0.883, 4.32]
# Score: [0.844; 4.59]

Prop.ratio(x=c(6,4),y=c(6,14), alternative="two.sided",
 conf.level=0.95, CImethod="GNC")

Prop.ratio(x=c(6,4),y=c(6,14), alternative="two.sided",
 conf.level=0.95, CImethod="Score")


# Koopman (1984), page 517
# x1=36, n1=40, x0=16, n0=80: [2.94, 7.15]

Prop.ratio(x=c(36, 4), y=c(16, 64), CImethod="Score")$conf.int 


# Miettinen, Nurminen (1985) p. 217 (Example 6): 
# x1=10, n1=10, x0=20, n0=20: [0.72, 1.20]

Prop.ratio(x=c(10, 0), y=c(20, 0), CImethod="MNScore")$conf.int


# MOVER-R Wilson in Newcombe and Fagerland, 2013, Table VIII:
#  x1=24, n1=73,x0=53, n0=65: [0.282, 0.563]
Prop.ratio(x=c(24, 49), y=c(53, 12), CImethod="MOVER")$conf.int 

#  x1=29, n1=55, x0=11, n0=11: [0.398, 0.761]
Prop.ratio(x=c(29, 26), y=c(11,0), CImethod="MOVER")$conf.int 

#  x1=7, n1=18, x0=1, n0=18: [(1.27, 40.8)]
Prop.ratio(x=c(7, 11), y=c(1, 17), CImethod="MOVER")$conf.int 


 }

\keyword{ htest }