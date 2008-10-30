\name{pairwiseCImethodsCont}

\alias{pairwiseCImethodsCont}
\alias{Param.diff}
\alias{Param.ratio}

\alias{Lognorm.diff}
\alias{Lognorm.ratio}

\alias{Median.diff}
\alias{Median.ratio}

\alias{HL.diff}
\alias{HL.ratio}

\alias{HD.diff}
\alias{HD.ratio}

\title{ Confidence interval methods implemented for continuous data in pairwiseCI }

\description{
Confidence interval methods available for pairwiseCI for comparison of two independent samples. Methods for continuous variables.
}

\usage{

Param.diff(x, y, conf.level=0.95, alternative="two.sided", ...)
Param.ratio(x, y, conf.level=0.95, alternative="two.sided", ...)

Lognorm.diff(x, y, conf.level=0.95, alternative="two.sided", sim=10000, ...)
Lognorm.ratio(x, y, conf.level=0.95, alternative="two.sided", sim=10000, ...)

HL.diff(x, y, conf.level=0.95, alternative="two.sided", ...)
HL.ratio(x, y, conf.level=0.95, alternative="two.sided", ...)

Median.diff(x, y, conf.level=0.95, alternative="two.sided", ...)
Median.ratio(x, y, conf.level=0.95, alternative="two.sided", ...)

HD.diff(x, y, conf.level=0.95, alternative="two.sided", ...)
HD.ratio(x, y, conf.level=0.95, alternative="two.sided", ...)
}

\arguments{
  \item{x}{ vector of observations in the first sample  }
  \item{y}{ vector of observations in the second sample }
  \item{alternative}{ character string, either "two.sided", "less" or "greater"  }
  \item{conf.level}{ the comparisonwise confidence level of the intervals, where 0.95 is default }
  \item{sim}{a single integer value, specifying the number of samples to be drawn for calculation of the empirical distribution of the generalized pivotal quantities}
  \item{\dots}{further arguments to be passed to the individual methods, see details}
}

\details{
\itemize{

\item \code{Param.diff} calculates the confidence interval for the difference
 in means of two Gaussian samples by calling \code{t.test} in package \pkg{stats},
 assuming homogeneous variances if \code{var.equal=TRUE},
 and heterogeneous variances if \code{var.equal=FALSE} (default);


\item \code{Param.ratio} calculates the Fiellers (1954) confidence interval for the ratio
 of two Gaussian samples by calling \code{ratio.t.test} in package \pkg{mratios},
 assuming homogeneous variances if \code{var.equal=TRUE}.
 If heterogeneous variances are assumed (setting \code{var.equal=FALSE}, the default), the test by Tamhane and Logan (2004) is inverted by solving a quadratic equation according to Fieller,
 where the estimated ratio is simply plugged in order to get Satterthwaite approximated degrees of freedom. See Hasler and Vonk (2006) for some simulation results.


\item \code{Lognorm.diff} calculates the confidence interval for the difference
 in means of two Lognormal samples, based on general pivotal quantities (Chen and Zhou, 2005); currently, further arguments \code{(\dots)}
 are not used; 


\item \code{Lognorm.ratio} calculates the confidence interval for the ratio
 in means of two Lognormal samples, based on general pivotal quantities (Chen and Zhou, 2005); currently, further arguments \code{(\dots)}
 are not used; 


\item \code{HL.diff} calculates the Hodges-Lehmann confidence interval for the difference of locations
 by calling \code{wilcox.exact} in package \pkg{exactRankTests} ;


\item \code{HL.ratio} calculates the Hodges-Lehmann-like confidence interval for the ratio of locations
 by calling \code{wilcox.exact} in package \pkg{exactRankTests} for the logarithms of observations;
  

\item \code{HD.diff} calculates a percentile bootstrap confidence interval for the difference
 of \dQuote{Harrell-Davis} estimates for location using code copied from \code{hdquantile} in package \pkg{Hmisc}
 and \code{boot.ci} in \pkg{boot}, the number of bootstrap replications can be set
 via \code{R=999} (default) ;


\item \code{HD.ratio} calculates a percentile bootstrap confidence interval for the ratio
 of \dQuote{Harrell-Davis} estimates for location using code copied from \code{hdquantile} in package \pkg{Hmisc}
 and \code{boot.ci} in package \pkg{boot}, the number of bootstrap replications can be set
 via \code{R=999} (default); 


\item \code{Median.diff} calculates a percentile bootstrap confidence interval for the difference
 of Medians using \code{boot.ci} in package \pkg{boot}, the number of bootstrap replications can be set
 via \code{R=999} (default); 


\item \code{Median.ratio} calculates a percentile bootstrap confidence interval for the ratio
 of Medians using \code{boot.ci} in package \pkg{boot}, the number of bootstrap replications can be set
 via \code{R=999} (default); 
}
}


\value{
A list containing:

  \item{conf.int}{a vector containing the lower and upper confidence limit}
  \item{estimate}{a single named value}
}

\references{ 
\itemize{

\item \code{Param.diff} uses \code{t.test} in \pkg{stats}. 

\item \bold{ Fieller EC (1954)}: Some problems in interval estimation. 
Journal of the Royal Statistical Society, Series B, 16, 175-185.

\item \bold{ Tamhane, AC, Logan, BR (2004)}: Finding the maximum safe dose level for heteroscedastic data.
 Journal of Biopharmaceutical Statistics 14, 843-856.

\item \bold{Hasler, M,  Vonk, R, Hothorn, LA}: Assessing non-inferiority of a new treatment in a three arm trial in the presence of heteroscedasticity (submitted). 

 \item  \bold{Chen, Y-H, Zhou, X-H (2006)}: Interval estimates for the ratio and the difference of two lognormal means.
Statistics in Medicine 25, 4099-4113.

\item \bold{Hothorn, T, Munzel, U}: Non-parametric confidence interval for the ratio.
 Report University of Erlangen, Department Medical Statistics 2002; available via:
\url{http://www.imbe.med.uni-erlangen.de/~hothorn/}.


\item \code{HD.diff} xxx


\item \code{HD.ratio} xxx


\item \code{Median.diff} xxx


\item \code{Median.ratio} xxx
}
}


\examples{

##############################################

# Dieldrin example: Two-sample situation:

# The dieldrin example

data(dieldrin)

Ray<-subset(dieldrin, River=="Ray")$dieldrin
Thames<-subset(dieldrin, River=="aThames")$dieldrin

Ray
Thames

## CI for the difference of means, 
# assuming normal errors and homogeneous variances :

thomo<-Param.diff(x=Thames, y=Ray, var.equal=TRUE)

# allowing heterogeneous variances
thetero<-Param.diff(x=Thames, y=Ray, var.equal=FALSE)


## Fieller CIs for the ratio of means,
# also assuming normal errors:

Fielhomo<-Param.ratio(x=Thames, y=Ray, var.equal=TRUE)

# allowing heterogeneous variances

Fielhetero<-Param.ratio(x=Thames, y=Ray, var.equal=FALSE)


## Hodges-Lehmann Intervalls for difference and ratios:

HLD<-HL.diff(x=Thames, y=Ray)

# allowing heterogeneous variances

HLR<-HL.ratio(x=Thames, y=Ray)


## Percentile Bootstrap intervals of Harrell-Davis estimators:

HDD<-HD.diff(x=Thames, y=Ray)

# allowing heterogeneous variances

HDR<-HD.ratio(x=Thames, y=Ray)

##  Percentile Bootstrap intervals of Medians:


MedianD<-Median.diff(x=Thames, y=Ray)

# allowing heterogeneous variances

MedianR<-Median.ratio(x=Thames, y=Ray)

thomo
thetero

Fielhomo
Fielhetero

HLD
HLR

HDD
HDR

MedianD
MedianR

# # #

# Lognormal CIs:

x<-rlnorm(n=10, meanlog=0, sdlog=1)
y<-rlnorm(n=10, meanlog=0, sdlog=1)

Lognorm.diff(x=x, y=y)
Lognorm.ratio(x=x, y=y)

 }

\keyword{ htest }