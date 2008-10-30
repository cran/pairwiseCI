\name{pairwiseCImethodsCount}

\alias{pairwiseCImethodsCount}
\alias{Poisson.ratio}
\alias{Quasipoisson.ratio}
\alias{Negbin.ratio}

\title{ Confidence interval methods implemented for count data in pairwiseCI }

\description{
Confidence interval methods available for pairwiseCI for comparison of two independent samples. Methods for count data.
}

\usage{

Poisson.ratio(x, y, conf.level=0.95, alternative="two.sided")
Quasipoisson.ratio(x, y, conf.level=0.95, alternative="two.sided")
Negbin.ratio(x, y, conf.level=0.95, alternative="two.sided")


}

\arguments{
  \item{x}{ vector of observations in the first sample  }
  \item{y}{ vector of observations in the second sample }
  \item{alternative}{ character string, either "two.sided", "less" or "greater"  }
  \item{conf.level}{ the comparisonwise confidence level of the intervals, where 0.95 is default }
}

\details{
\itemize{

\item \code{Poisson.ratio} calculates a confidence interval for the ratio of
 means assuming the Poisson distribution of the response by fitting a generalized linear model with log-link using \code{glm} in package \pkg{stats},
 constructing a likelihood profile and deriving a equal-tailed confidence interval from this profile. Please not that confidence intervals from this method
  produce severely misleading results, when there is extra-Poisson variation in the data.


\item \code{Quasipoisson.ratio} calculates a confidence interval for the ratio of
 means of the response by fitting a generalized linear model with family \code{quasipoisson} and log-link using \code{glm} in package \pkg{stats},
 constructing a deviance profile and deriving a equal-tailed confidence interval from this profile.


\item \code{Negbin.ratio} calculates a confidence interval for the ratio of
 means assuming the negative binomial distribution of the response by fitting a generalized linear model with log-link using \code{glm.nb} in package \pkg{MASS},
 constructing a likelihood profile and deriving a equal-tailed confidence interval from this profile.

}

Note, that for all the methods, a separate glm is fitted for each two-sample comparison! When a common model can be reasonbly assumed for all the data, there are smarter methods of constructing confidence
intervals for groupwise comparisons, based on a common model, see e.g. the function \code{confint} in package \pkg{stats}, the function \code{confint.glm} in package \pkg{MASS} and the function \code{confint.glht} in package \pkg{multcomp}.

Note, that the code used here is slightly changed from the original code by Venables and Ripley, or Bates and Watts. An limit is imposed on the parameter space in which the profile is constructed. By that limitation, intervals can also be constructed for extreme cases with all observations in one group being zero.

Note, that the \code{Poisson.ratio} can be used when only one count is present in each group. For \code{Quasipoisson.ratio}, \code{Negbin.ratio}, repeated observations are necessary in each group.

}


\value{
A list containing:

  \item{conf.int}{a vector containing the lower and upper confidence limit}
  \item{estimate}{a single named value}
}

\author{Daniel Gerhard, Frank Schaarschmidt}

\references{ 

\bold{Venables WN and Ripley BD (2002).} Modern Applied Statistics using S, Fourth Edition. Springer New York.
\bold{Bates, D.M. and Watts, D.G.(1988).} Nonlinear Regression Analysis and Its Applications. John Wiley and Sons, New York.
}


\examples{

library(mratios)

data(Mutagenicity)

QPCI<-pairwiseCI(MN ~ Treatment, data=Mutagenicity,
 alternative="greater", control="Vehicle", method="Quasipoisson.ratio")

plot(QPCI)

 }

\keyword{ htest }