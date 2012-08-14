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

\title{Confidence intervals for two sample comparisons of continuous data}

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

\item \kbd{Param.diff} calculates the confidence interval for the difference
 in means of two Gaussian samples by calling \kbd{t.test} in package \pkg{stats},
 assuming homogeneous variances if \kbd{var.equal=TRUE},
 and heterogeneous variances if \kbd{var.equal=FALSE} (default);


\item \kbd{Param.ratio} calculates the Fiellers (1954) confidence interval for the ratio
 of two Gaussian samples by calling \kbd{ratio.t.test} in package \pkg{mratios},
 assuming homogeneous variances if \kbd{var.equal=TRUE}.
 If heterogeneous variances are assumed (setting \kbd{var.equal=FALSE}, the default), the test by Tamhane and Logan (2004) is inverted by solving a quadratic equation according to Fieller,
 where the estimated ratio is simply plugged in order to get Satterthwaite approximated degrees of freedom. See Hasler and Vonk (2006) for some simulation results.


\item \kbd{Lognorm.diff} calculates the confidence interval for the difference
 in means of two Lognormal samples, based on general pivotal quantities (Chen and Zhou, 2005); currently, further arguments \kbd{(\dots)}
 are not used; 


\item \kbd{Lognorm.ratio} calculates the confidence interval for the ratio
 in means of two Lognormal samples, based on general pivotal quantities (Chen and Zhou, 2005); currently, further arguments \kbd{(\dots)}
 are not used; 


\item \kbd{HL.diff} calculates the Hodges-Lehmann confidence interval for the difference of locations
 by calling \kbd{wilcox.exact} in package \pkg{exactRankTests} ;


\item \kbd{HL.ratio} calculates the Hodges-Lehmann-like confidence interval for the ratio of locations
 by calling \kbd{wilcox.exact} in package \pkg{exactRankTests} for the logarithms of observations;
  

\item \kbd{Median.diff} calculates a percentile bootstrap confidence interval for the difference
 of Medians using \kbd{boot.ci} in package \pkg{boot}, the number of bootstrap replications can be set
 via \kbd{R=999} (default); 


\item \kbd{Median.ratio} calculates a percentile bootstrap confidence interval for the ratio
 of Medians using \kbd{boot.ci} in package \pkg{boot}, the number of bootstrap replications can be set
 via \kbd{R=999} (default); 
}
}


\value{
A list containing:

  \item{conf.int}{a vector containing the lower and upper confidence limit}
  \item{estimate}{a single named value}
}

\references{ 
\itemize{

\item \kbd{Param.diff} uses \kbd{t.test} in \pkg{stats}. 

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
}
}


\examples{


data(sodium)

iso<-subset(sodium, Treatment=="xisogenic")$Sodiumcontent
trans<-subset(sodium, Treatment=="transgenic")$Sodiumcontent

iso
trans

## CI for the difference of means, 
# assuming normal errors and homogeneous variances :

thomo<-Param.diff(x=iso, y=trans, var.equal=TRUE)

# allowing heterogeneous variances
thetero<-Param.diff(x=iso, y=trans, var.equal=FALSE)


## Fieller CIs for the ratio of means,
# also assuming normal errors:

Fielhomo<-Param.ratio(x=iso, y=trans, var.equal=TRUE)

# allowing heterogeneous variances

Fielhetero<-Param.ratio(x=iso, y=trans, var.equal=FALSE)


## Hodges-Lehmann Intervalls for difference and ratios:

HLD<-HL.diff(x=iso, y=trans,)

# allowing heterogeneous variances

HLR<-HL.ratio(x=iso, y=trans,)



MedianD<-Median.diff(x=iso, y=trans,)

# allowing heterogeneous variances

MedianR<-Median.ratio(x=iso, y=trans,)

thomo
thetero

Fielhomo
Fielhetero

HLD
HLR

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