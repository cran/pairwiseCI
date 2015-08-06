\name{overdispersed.binomial.ratio}
\alias{Quasibin.ratio}
\alias{Betabin.ratio}
\alias{ODbin.ratio}


\title{Confidence intervals for risk ratios of overdispersed binomial data}

\description{Calculate approximate confidence intervals for ratios of proportions (risk ratios) of two samples. Three functions are available for intervals assuming the beta binomial distribution, based on a generalized assumption of overdispersion estimated from the residuals, and based on the quasibinomial assumption using a generalized linear model.}

\usage{
Betabin.ratio(x, y, conf.level=0.95, alternative="two.sided",
 CImethod=c("FBB", "LBB"), iccpool=FALSE, resbin=FALSE) 

ODbin.ratio(x, y, conf.level=0.95, alternative="two.sided",
 CImethod=c("FOD", "LOD"), varmethod=c("res", "san"), resbin=FALSE) 

Quasibin.ratio(x, y, conf.level = 0.95, alternative = "two.sided", grid = NULL)
}

\arguments{
  \item{x}{ a matrix or data.frame of the first sample, with two columns giving the counts of successes and failures in the two columns; each row should correspond to one experimental or observational unit; first column will be treated as 'success'}
  \item{y}{ a matrix or data.frame of the second sample, with two columns giving the counts of successes and failures in the two columns; each row should correspond to one experimental or observational unit; first column will be treated as 'success'}
  \item{conf.level}{ a single numeric value between 0 and 1, the confidence level}
  \item{alternative}{a character string, \code{"two.sided"} for two-sided intervals, \code{"less"} for upper limits, \code{"greater"} for lower limits only}
  \item{CImethod}{a character string, chossing between available methods for interval computation: in \code{betabin.ratio}: assuming the beta binomial distribution \code{"FBB"} invokes the Fieller-Bailey interval and \code{"LBB"} invokes the delta-method on the log scale (Lui et al. 2000);
in \code{ODbin.ratio}: without particular assumptions w.r.t to the distibution, \code{"FOD"} invokes the Fieller-Bailey interval and \code{"LOD"} invokes the delta-method on the log scale as described by Zaihra and Paul (2010)}
  \item{iccpool}{logical, if \code{FALSE}, a separate intra-class-correlation coefficient is estimated for each sample (assuming different levels of overdispersion in the two samples); if \code{TRUE}, a joint intra-class-correlation coefficient (assuming equal ICCs in the two samples)}
  \item{resbin}{logical: if \code{FALSE}, underdispersion is allowed in estimation; if \code{TRUE}, underdispersion not allowed: when sample estimates suggest underdispersion, the variance is fixed at the binommial variance}
  \item{varmethod}{a character string specifying te type of variance estimation in \code{ODbin.ratio}: \code{"res"} is the residual variance, \code{"san"} corresponds to a sandwich estimator, details see (Zaihra and Paul, 2010)}
  \item{grid}{optional, a numeric vector to be supplied to the profiling used internally in \code{quasibin.ratio} to obtain profile deviance intervals for each samples proportion on the logit-scale.}
}

\details{
The methods in \code{betabin.ratio} are described by Lui et al., (2000), where different estimates for ICC (and thus overdispersion) are computed for each sample.
 For small sample size or extreme proportions, one may restrict the variance to that of the binomial distribution and/or use a pooled estimator of ICC for a joint model of overdispersion in the two samples. 

The methods in \code{ODbin.ratio} are described by Zaihra and Paul (2010), where different estimates for overdispersion are computed for each sample. Zaihra and Paul refer to two different methods of estimating the variance (MR, MS), here referred to as "res", "san", respectively. As above one may restrict the variance to that of the binomial distribution.

The method to compute intervals under the quasibinomial assumptions in \code{quasibin.ratio} uses a quasibinomial generalized linear model to obtain profile deviance intervals (e.g. Bates and Watts, 1988; relying on package mcprofile), and then applies the MOVERR method by Donner and Zhou(2012); experimental! 

}
\value{
a list with elements
\item{conf.int }{confidence limits for the ratio of proprotions in x over that in y}
\item{estimate }{the point estimate for the ratio of proprotions in x over that in y}

}
\references{

\emph{Lui K-L, Mayer JA, Eckhardt L (2000):} Confidence intervals for the risk ratio under cluster sampling based on the beta-binomial model. Statistics in Medicine 19, 2933-2942.

\emph{Zaihra, T and Paul, S (2010):} Interval Estimation of Some Epidemiological Measures of Association. The International Journal of Biostatistics. 6 (1), Article 35.

\emph{Bates and Watts(1988):} Nonlinear Regression Analysis and Its Applications, Wiley, Ch.6

\emph{Donner and Zou (2012):} Closed-form confidence intervals for functions of the normal mean and standard deviation. Statistical Methods in Medical Research 21(4):347-359.
}
\author{
Frank Schaarschmidt
}

\note{
The method in \code{quasibin.ratio} is experimental.
}

\examples{

# Toxicologoical data: Number of Pups alive four days after birth (16 litters of rats)
# Original source: Weil, 1970: Selection of valid number[...].
#  Food and Cosmetics, Toxicology, 8, 177-182.
# Cited from Zaihra and Paul(2010): Interval Estimation  of Some Epidemiological
# Measures of Association. Int. J Biostatistics, 6(1), Article 35.


mchem = c(12, 11, 10, 9, 11, 10, 10, 9, 9, 5, 9, 7, 10, 6, 10, 7)
xchem = c(12, 11, 10, 9, 10,  9,  9, 8, 8, 4, 7, 4,  5, 3,  3, 0)
dchem <- cbind("alive"=xchem, "dead"=mchem-xchem)

mcon = c(13, 12, 9, 9, 8, 8, 13, 12, 10, 10, 9, 13, 5, 7, 10, 10)
xcon = c(13, 12, 9, 9, 8, 8, 12, 11,  9,  9, 8, 11, 4, 5,  7,  7)
dcon <- cbind("alive"=xcon, "dead"=mcon-xcon)


# Zaihra and Paul report: MR2: [0.714; 1.034]
ODbin.ratio(x=dchem, y=dcon, CImethod="LOD", resbin=FALSE) 

# Zaihra and Paul report: MR4: [0.710; 1.029]
ODbin.ratio(x=dchem, y=dcon, CImethod="FOD", resbin=FALSE) 



Betabin.ratio(x=dchem, y=dcon, CImethod="FBB", iccpool=TRUE, resbin=TRUE) 

Quasibin.ratio(x=dchem, y=dcon)


# Solar protection data: intervention and control group (Mayer, 1997:)

# Number of children with adequate level of solar protection
# Source: Mayer, Slymen, Eckhardt et al.(1997): Reducing ultraviolat 
# radiation exposure in children. Preventive Medicine 26, 845-861. 
# Cited from: Lui et al. (2000)

mint=c(3,2,2,5,4,3,1,2,2,2,1,3,1,3,2,2,6,2,4,2,2,2,2,1,1,1,1,1,1)
xint=c(1,1,1,0,1,2,1,2,2,1,1,2,1,2,2,0,0,0,0,1,2,1,1,1,1,0,0,0,0)
dint <- cbind("adequate"=xint, "non-adequate"=mint-xint)

mcont=c(2,4,3,2,3,4,4,2,2,3,2,2,4,3,2,3,1,1,2,2,2,3,3,4,1,1,1,1,1)
xcont=c(0,0,2,2,0,4,2,1,1,3,2,1,1,3,2,3,1,0,1,2,1,1,2,4,1,1,1,0,0)
dcont <- cbind("adequate"=xcont, "non-adequate"=mcont-xcont)


# Lui et al.(2000) report for the Fieller-Bailey method
# with pooled ICC: 905% CI = [0.964; 2.281]
Betabin.ratio(x=dcont, y=dint, conf.level=0.95, alternative="two.sided",
 CImethod="FBB", iccpool=TRUE, resbin=FALSE) 

# and for the Log-scale delta method with pooled ICC:
# 95% CI = [0.954; 2.248]
Betabin.ratio(x=dcont, y=dint, conf.level=0.95, alternative="two.sided",
 CImethod="LBB", iccpool=TRUE, resbin=FALSE) 

ODbin.ratio(x=dcont, y=dint, conf.level=0.95, alternative="two.sided",
 CImethod="FOD", resbin=TRUE) 

Quasibin.ratio(x=dcont, y=dint, conf.level = 0.95, alternative = "two.sided")



}

\keyword{ htest }