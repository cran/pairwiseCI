\name{np.re}
\alias{np.re}
\title{
Nonparametric test and confidence interval based on relative effects
}
\description{
Nonparametric test and confidence interval for comparing two samples in presence of heterogeneous variances (Behrens Fisher Problem). The confidence intervals
estimate the relative effect p(x,y), i.e. the probability that a randomly chosen observation from population x is larger than a randomly chosen observation from population y, \eqn{P(x>y)+P(x=y)/2}.
Different approximations are available, the method is designed for continuous or ordered categorical data.
}
\usage{
np.re(x, y, conf.level = 0.95, 
alternative = c("two.sided", "less", "greater"),
 method = c("logit", "probit", "normal", "t.app"))
}
\arguments{
  \item{x}{
a numeric vector, containing the observations of the first sample
}
  \item{y}{
a numeric vector, containing the observations of the second sample
}
  \item{conf.level}{
a single numeric value, the desired confidence level of the interval to be constructed.
}
  \item{alternative}{
a single character string, one of "two.sided" (for two-sided testing and confidence intervals), "less" (for testing that x < y in tendency, or upper confidence limits for p(x,y)), or "greater" (for testing x > y in tendency, or lower confidence limits for p(x,y))

}
  \item{method}{ a single character string, one of "logit","probit","normal","t.app", specifying which of the different available approximative methods should be used. See details below.

}
}
\details{
 The function performs two sample tests for the nonparametric Behrens-Fisher problem. The hypothesis tested is \deqn{H_0: p(x,y)=1/2}{H0: p(x,y)=1/2}, where
 p(x,y) denotes the relative effect of 2 independent samples x and y. Further, confidence intervals for the relative effect p(x,y) are computed. For the computation of p-values as well as confidence limits, 
 a standard normal or Satterthwaite t-approximation can be used directly on the scale of the relative effects (method "normal","t.app"). Based on these methods, the intervals may have bounds outside [0,1] for extreme results.
 Alternatively variance stabilising transformations (Probit and Logit) may be used in combination with normal approximation (methods "logit", and "probit"). 

 If the samples are completely separated, the variance estimator is zero by construction. In this case, estimated relative effects 0 or 1 are replaced with 0.001, 0.999 respectively. The variance estimator is replaced as described in Neubert and Brunner (2006).

}
\value{
An object of class "htest"
}
\references{
\emph{Brunner, E., Munzel, U. (2000).} The Nonparametric Behrens-Fisher Problem: Asymptotic Theory and a Small Sample Approximation. Biometrical Journal 42, 17 -25.
\emph{Neubert, K., Brunner, E., (2006).} A Studentized Permutation Test for the Nonparametric Behrens-Fisher Problem. Computational Statistics and Data Analysis.

}
\author{
 Frank Konietschke, \pkg{nparcomp}, with adaptations by Frank Schaarschmidt
}


\examples{

data(sodium)

iso<-subset(sodium, Treatment=="xisogenic")$Sodiumcontent
tra<-subset(sodium, Treatment=="transgenic")$Sodiumcontent

np.re(x=iso, y=tra, conf.level = 0.95)
np.re(x=iso, y=tra, conf.level = 0.95, alternative="less")
np.re(x=iso, y=tra, conf.level = 0.95, alternative="greater")

}

\keyword{ htest }
\concept{ nonparametric }
