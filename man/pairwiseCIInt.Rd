\name{pairwiseCIInt}
\alias{pairwiseCICont}
\alias{pairwiseCIProp}

\title{ Internal functions for pairwiseCI}
\description{
For internal use. Two different methods for data representable as a two numeric vectors (pairwiseCICont) and
 data representable as matrix with two columns like cbind(successes, failures).
 Functions that split up a data.frame according to one factor, and perform all pairwise comparisons
 and comparisons to control among the levels of the factor by calling methods documented in pairwiseCIMethodCont
 and pairwiseCIMethodsProp.

}
\usage{

pairwiseCICont(formula, data, alternative="two.sided",
 conf.level=0.95, method, control=NULL, ...)

pairwiseCIProp(formula, data, alternative="two.sided",
 conf.level=0.95, control=NULL, method, ...)
}

\arguments{
  \item{formula}{ A formula of the structure \code{response ~ treatment} for numerical variables,
 and of structure \code{cbind(success, failure) ~ treatment} for binomial variables }

  \item{data}{ A data.frame containing the numerical response variable and the treatment
 and by variable as factors. Note, that for binomial data, two columns containing the number
 of successes and failures must be present in the data. }

  \item{alternative}{ Character string, either "two.sided", "less" or "greater"  }

  \item{conf.level}{ The comparisonwise confidence level of the intervals, where 0.95 is default }

  \item{method}{ A character string specifying the confidence interval method
    \item{"Param.diff"}{ Difference of two means, with additional argument \code{var.equal=FALSE}(default) as in \code{t.test(stats)}}
    \item{"Param.ratio"}{ Ratio of two means, with additional argument \code{var.equal=FALSE}(default) as in \code{t.test.ratio(mratios)}}
    \item "Lognorm.diff": Difference of two means, assuming a lognormal distribution,
    \item "Lognorm.ratio": Ratio of two means, assuming a lognormal distribution,
    \item{"HL.diff"}{Exact nonparametric CI for difference of locations based on the Hodges-Lehmann estimator,}
    \item{"HL.ratio"}{Exact nonparametric CI for ratio of locations, based on the Hodges-Lehmann estimator,}
    \item{"HD.diff"}{Nonparametric CI for difference of locations, based on the Harrell-Davis estimator (percentile bootstrap CI),}
    \item{"HD.ratio"}{Nonparametric CI for ratio of locations, based on the Harrell-Davis estimator (percentile bootstrap CI),}
    \item{"Median.diff"}{Nonparametric CI for difference of locations, based on the medians (percentile bootstrap CI),}
    \item{"Median.ratio"}{Nonparametric CI for ratio of locations, based on the medians (percentile bootstrap CI),}
    \item{"Prop.diff"}{Asymptotic CI for difference of proportions \code{prop.test(stats)}}
    \item{"Prop.ratio"}{ Asymptotic CI for ratio of proportions}
    \item{"Prop.or"}{ Asymptotic CI for the odds ratio}
  See \code{?pairwiseCImethods} for details.
  }

 \item{control}{ Character string, specifying one of the levels of the treatment variable as 
   control group in the comparisons; default is NULL, then CI for all pairwise comparisons are calculated. }

 \item{\dots}{further arguments to be passed to the functions specified in \code{methods}}
}
\details{
 These functions are for internal use in pairwiseCI.
}
\value{
  a list containing:
  \item{estimate}{ numeric vector: the point estimates }
  \item{lower}{numeric vector: lower confidence bounds}
  \item{upper}{numeric vector: upper confidence bounds}
  \item{compnames}{character vector with the names of comparisons}

}

\seealso{ \code{\link{pairwiseCI}} for the user level function;
\code{\link{pairwiseCImethodsCont}}, and \code{\link{pairwiseCImethodsProp}} for a more detailed documentation of the implemented methods;
\code{\link{summary.pairwiseCI}} for a summary function.

 \code{t.test(stats)},  \code{wilcox.exact(exactRankTests)}, \code{prop.test(stats)} for the Sources of some of the CI methods,
\pkg{multcomp} for simultaneous intervals for difference for various contrasts,
\pkg{mratios} for simultaneous intervals for the ratio in many-to-one comparisons}


\keyword{ htest }

