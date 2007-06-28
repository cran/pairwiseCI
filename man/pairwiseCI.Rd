\name{pairwiseCI}
\alias{pairwiseCI}

\title{ Calculate various confidence intervals for comparison of two independent samples}
\description{

Confidence intervals for difference or ratio of location parameters of two independent samples.
 The CI are NOT adjusted for multiplicity by default. A by statement allows for separate calculation
 of pairwise comparisons according to further factors in the given dataframe. 
 The function applies the intervals available from t.test(stats) for difference of means with
 and without assumptions of homogeneous variances; the exact CI for difference (wilcox.exact(exactRankTests))
 and ratio of location based on the Hodges-Lehmann estimator; bootstrap intervalls for ratio and difference
 of Medians and Harrell-Davis estimators a more robust alternatives to the Hodges-Lehmann estimator (boot, Hmisc); 
 the Score test derived CI for the difference (prop.test(stats)), Woolf-interval for the odds-ratio
 and an add-4-interval for the ratio of proportions.
}
\usage{
pairwiseCI(formula, data, by = NULL,
 alternative = "two.sided", conf.level = 0.95,
 method = "Param.diff", control = NULL, ...)
}

\arguments{
  \item{formula}{ A formula of the structure \code{response ~ treatment} for numerical variables,
 and of structure \code{cbind(success, failure) ~ treatment} for binomial variables }

  \item{data}{ A data.frame containing the numerical response variable and the treatment
 and by variable as factors. Note, that for binomial data, two columns containing the number
 of successes and failures must be present in the data. }
 
 \item{by}{ A character string or character vector giving the name(s) of additional
 factors by which the data set is to be split. }
  \item{alternative}{ Character string, either "two.sided", "less" or "greater"  }
  \item{conf.level}{ The comparisonwise confidence level of the intervals, where 0.95 is default }
  \item{method}{ A character string specifying the confidence interval method
  \itemize{
    \item "Param.diff": Difference of two means, with additional argument \code{var.equal} as in \code{t.test},
    \item "Param.ratio": Ratio of two means, with additional argument \code{var.equal} as in \code{ratio.t.test},
    \item "Lognorm.diff": Difference of two means, assuming a lognormal distribution,
    \item "Lognorm.ratio": Ratio of two means, assuming a lognormal distribution,
    \item "HL.diff": Exact nonparametric CI for difference of locations based on the Hodges-Lehmann estimator,
    \item "HL.ratio": Exact nonparametric CI for ratio of locations, based on the Hodges-Lehmann estimator,
    \item "HD.diff": Nonparametric CI for difference of locations, based on the Harrell-Davis estimator (percentile bootstrap CI),
    \item "HD.ratio": Nonparametric CI for ratio of locations, based on the Harrell-Davis estimator (percentile bootstrap CI),
    \item "Median.diff": Nonparametric CI for difference of locations, based on the medians (percentile bootstrap CI),
    \item "Median.ratio": Nonparametric CI for ratio of locations, based on the medians (percentile bootstrap CI),
    \item "Prop.diff": Asymptotic CI for difference of proportions \code{prop.test},
    \item "Prop.ratio": Asymptotic CI for ratio of proportions,
    \item "Prop.or": Asymptotic CI for the odds ratio,
    }
  See \code{?pairwiseCImethods} for details.
  }

 \item{control}{ Character string, specifying one of the levels of the treatment variable as 
   control group in the comparisons; default is NULL, then CI for all pairwise comparisons are calculated. }
 \item{\dots}{further arguments to be passed to the functions specified in \code{methods}}
}
\details{
 Note that all the computed intervals are without adjustment for multiplicity by default. 

}
\value{
 an object of class "pairwiseCI" structured as:

  a list containing:
  \item{byout}{A list, ordered by the levels of the by-factor, where each element is a list
 containing the vectors estimate, lower, upper and compnames}

for further Info try str(),  

the object is printed by \code{print.pairwiseCI}.

}
\references{ 
\itemize{

\item \code{Param.diff} simply uses: \code{t.test} in \pkg{stats},

\item \code{Param.ratio} for homogeneous variances: \bold{Fieller EC (1954):} Some problems in interval estimation. Journal of the Royal Statistical Society, Series B, 16, 175-185.

\item \code{Param.ratio} for heterogenous variances: the test proposed in: \bold{Tamhane, AC, Logan, BR (2004):} Finding the maximum safe dose level for heteroscedastic data. Journal of Biopharmaceutical Statistics 14, 843-856.
 is inverted by solving a quadratic equation according to Fieller, where the estimated ratio is simply plugged in order to get Satterthwaite approximated degrees of freedom.
 See also: \bold{Hasler, M,  Vonk, R, Hothorn, LA:} Assessing non-inferiority of a new treatment in a three arm trial in the presence of heteroscedasticity (submitted).

\item \code{Lognorm.ratio} and \code{Lognorm.ratio}: \bold{Chen, Y-H, Zhou, X-H (2006)}:Interval estimates for the ratio and the difference of two lognormal means.
Statistics in Medicine 25, 4099-4113.

\item \code{HL.diff}: \code{wilcox.exact} in \pkg{exactRankTests} 

\item \code{HL.ratio}: \bold{Hothorn, T, Munzel, U:} Non-parametric confidence interval for the ratio. Report University of Erlangen, Department Medical Statistics 2002; available via: http://www.imbe.med.uni-erlangen.de/~hothorn/

\item \code{HD.diff}: xxx 

\item \code{HD.ratio}: xxx

\item \code{Median.diff}: xxx 

\item \code{Median.ratio}: xxx

\item \code{Prop.diff}: simply uses \code{prop.test} in \pkg{stats} 

\item \code{Prop.ratio} e.g. in: \bold{Dann, RS and Koch, GG (2005):} Review and evaluation of methods for computing confidence intervals for the ratio of two proportions and considerations for non-inferiority clinical trials. Journal of Biopharmaceutical Statistics, 15, 85-107.

\item \code{Prop.or} e.g. in: \bold{Lawson, R (2005):} Small sample confidence intervals for the odds ratio. Communication in Statistics Simulation and Computation, 33, 1095-1113.
 }
}

\author{ Frank Schaarschmidt, Institute of Biostatistics, Leibniz University of Hannover, Germany }

\seealso{ \code{t.test(stats)},  \code{wilcox.exact(exactRankTests)}, \code{prop.test(stats)} for the CI methods,
\pkg{multcomp} for simultaneous intervals for difference for various contrasts,
\pkg{mratios} for simultaneous intervals for the ratio in many-to-one comparisons}
\examples{

# some examples:
# In many cases the shown examples might not make sense,
# but display how the functions can be used.

data(Oats)

# # all pairwise comparisons,
# separately for each level of nitro:

apc <- pairwiseCI(yield ~ Variety, data=Oats,
 by="nitro", method="Param.diff")

apc

plot(apc)

# # many to one comparisons, with variety Marvellous as control,
# for each level of nitro separately:

m21 <- pairwiseCI(yield ~ Variety, data=Oats,
 by="nitro", method="Param.diff", control="Marvellous")

 plot(m21)

# # the same using confidence intervals for the ratio of means:

m21 <- pairwiseCI(yield ~ Variety, data=Oats,
 by="nitro", method="Param.diff", control="Marvellous")

 plot(m21, CIvert=TRUE, H0line=0.9)

###############################################

# The repellent data set (a trial on repellent 
# effect of sulphur on honey bees): Measured was
# the decrease of sugar solutions (the higher the decrease,
# the higher the feeding, and the less the repellent effect).
# Homogeneity of variances is questionable. Which of the doses
# leads to decrease of the variable decrease compared to the
# control group "H"?

data(repellent)
boxplot(decrease ~ treatment, data=repellent)

# as difference to control (corresponding to Welch tests)
beeCId<-pairwiseCI(decrease ~ treatment, data=repellent,
 method="Param.diff", control="H", alternative="less",
 var.equal=FALSE)
beeCId
plot(beeCId)

# as ratio to control:

beeCIr<-pairwiseCI(decrease ~ treatment, data=repellent,
 method="Param.ratio", control="H", alternative="less",
 var.equal=FALSE)
beeCIr
plot(beeCIr)

# Bonferroni-adjustment can be applied:

beeCIrBonf<-pairwiseCI(decrease ~ treatment, data=repellent,
 method="Param.ratio", control="H", alternative="less",
 var.equal=FALSE, conf.level=1-0.05/7)
beeCIrBonf
plot(beeCIrBonf)


##############################################

# Proportions:

# The rooting example:
# Calculate confidence intervals for the 
# difference of proportions between the 3 doses of IBA,
# separately for 4 combinations of "Age" and "Position".
# Note: we pool over Rep in that way. Whether this makes
# sense or not, is decision of the user.

data(rooting)

# Risk difference

aprootsRD<-pairwiseCI(cbind(root, noroot) ~ IBA,
 data=rooting, by=c("Age", "Position"), method="Prop.diff")

aprootsRD

# Odds ratio

aprootsOR<-pairwiseCI(cbind(root, noroot) ~ IBA,
 data=rooting, by=c("Age", "Position"), method="Prop.ratio")

aprootsOR

# Risk ratio

aprootsRR<-pairwiseCI(cbind(root, noroot) ~ IBA,
 data=rooting, by=c("Age", "Position"), method="Prop.or")

aprootsRR

# CI can be plotted:

plot(aprootsRR)

###############################################

# Other available methods:

data(dieldrin)

boxplot(dieldrin~River, data=dieldrin)

pairwiseCI(dieldrin~River, data=dieldrin, method="HD.diff")
pairwiseCI(dieldrin~River, data=dieldrin, method="HD.ratio")

pairwiseCI(dieldrin~River, data=dieldrin, method="HL.diff")
pairwiseCI(dieldrin~River, data=dieldrin, method="HL.ratio")

pairwiseCI(dieldrin~River, data=dieldrin, method="Median.diff")
pairwiseCI(dieldrin~River, data=dieldrin, method="Median.ratio")


data(sodium)

pairwiseCI(dieldrin~River, data=dieldrin, method="HD.diff")
pairwiseCI(dieldrin~River, data=dieldrin, method="HD.ratio")

# CIs assuming lognormal distribution of the response:

resp<-rlnorm(n=20, meanlog = 0, sdlog = 1)
treat<-as.factor(rep(c("A","B")))
datln<-data.frame(resp=resp, treat=treat)

pairwiseCI(resp~treat, data=datln, method="Lognorm.diff")
pairwiseCI(resp~treat, data=datln,  method="Lognorm.ratio")



}
\keyword{ htest }

