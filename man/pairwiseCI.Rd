\name{pairwiseCI}
\alias{pairwiseCI}

\title{Wrapper function for two-sample confidence intervals}
\description{

Confidence intervals (CI) for difference or ratio of location parameters of two independent samples.
 The CI are NOT adjusted for multiplicity by default. A by statement allows for separate calculation
 of pairwise comparisons according to further factors in the given dataframe. 
 The function applies the intervals available from t.test(stats) for difference of means with
 and without assumptions of homogeneous variances; large sample approximations for the difference and ratio of means of lognormal data; the exact CI for difference (wilcox.exact(exactRankTests))
 and ratio of location based on the Hodges-Lehmann estimator; bootstrap intervalls for ratio and difference
 of Medians and Harrell-Davis estimators a more robust alternatives to the Hodges-Lehmann estimator (boot, Hmisc); 
 the Score test derived CI for the difference (prop.test(stats)), Woolf-interval for the odds-ratio
 and a crude asymptotic as well as an iterative interval for the ratio of proportions.
}
\usage{
pairwiseCI(formula, data, by = NULL,
 alternative = "two.sided", conf.level = 0.95,
 method = "Param.diff", control = NULL, ...)
}

\arguments{
  \item{formula}{ A formula of the structure \kbd{response ~ treatment} for numerical variables,
 and of structure \kbd{cbind(success, failure)} \kbd{~ treatment} for binomial variables }

  \item{data}{ A data.frame containing the numerical response variable and the treatment
 and by variable as factors. Note, that for binomial data, two columns containing the number
 of successes and failures must be present in the data. }
 
 \item{by}{ A character string or character vector giving the name(s) of additional
 factors by which the data set is to be split. }
  \item{alternative}{ Character string, either "two.sided", "less" or "greater"  }
  \item{conf.level}{ The comparisonwise confidence level of the intervals, where 0.95 is default }
  \item{method}{ A character string specifying the confidence interval method, with the following options
    \kbd{"Param.diff"}: Difference of two means, with additional argument \kbd{var.equal=FALSE}(default) as in \kbd{t.test(stats)},
    \kbd{"Param.ratio"}: Ratio of two means, with additional argument \kbd{var.equal=FALSE}(default) as in \kbd{t.test.ratio(mratios)},
    \kbd{"Lognorm.diff"}: Difference of two means, assuming a lognormal distribution, based on generalized pivotal quantities,
    \kbd{"Lognorm.ratio"}: Ratio of two means, assuming a lognormal distribution, based on generalized pivotal quantities,
    \kbd{"np.re"}: nonparametric confidence interval for relative effects suitable for the Behrens Fisher problem,
    \kbd{"HL.diff"}: Exact conditional nonparametric CI for difference of locations based on the Hodges-Lehmann estimator,
    \kbd{"HL.ratio"}: Exact conditional nonparametric CI for ratio of locations, based on the Hodges-Lehmann estimator,
    \kbd{"Median.diff"}: Nonparametric CI for difference of locations, based on the medians (percentile bootstrap, stratified by the grouping variable),
    \kbd{"Median.ratio"}: Nonparametric CI for ratio of locations, based on the medians (percentile bootstrap, stratified by the grouping variable),
    \kbd{"Negbin.ratio"}: Profile-likelihood CI for ratio of expected values assuming the negative binomial distribution, adapting code from the profile function of MASS (Venables and Ripley,2002),
    \kbd{"Quasipoisson.ratio"}: Profile-deviance CI for the ratio of expected values with a quasipoisson assumption, using the adapted code of the profile function in MASS 
    \kbd{"Poisson.ratio"}: Profile-likelihood CI for the ratio of expected values with a Poisson assumption, again using slightly changed code from MASS,
    \kbd{"Prop.diff"}: Asymptotic CI for the difference of proportions with the following options: \kbd{CImethod="NHS"} Newcombes Hybrid Score interval (Newcombe, 1998), \kbd{CImethod="CC"} continuity corrected interval (Newcombe, 1998) as implemented in \kbd{prop.test(stats)},  \kbd{CImethod="AC"} Agresti-Caffo interval (Agresti and Caffo, 2000)
    \kbd{"Prop.ratio"}: Asymptotic CI (normal approximation on the log: \kbd{CImethod="GNC"}) or iterative CI (\kbd{CImethod="Score"}) for ratio of proportions,
    \kbd{"Prop.or"}: Asymptotic CI for the odds ratio (normal approximation on the log: \kbd{CImethod="Woolf"}), or the exact CI corresponding to Fishers exact test (\kbd{CImethod="Exact"}, taken from \kbd{fisher.test}, package \kbd{stats})
  See \kbd{?pairwiseCImethods} for details.
  }                                                 

 \item{control}{ Character string, specifying one of the levels of the treatment variable as 
   control group in the comparisons; default is NULL, then CI for all pairwise comparisons are calculated. }
 \item{\dots}{further arguments to be passed to the functions specified in \code{\link{pairwiseCImethodsCont}}, \code{\link{pairwiseCImethodsCount}} and \code{\link{pairwiseCImethodsProp}}. 
  In particular, different methods to calculate confidence intervals can be specified via the argument \kbd{CImethod} for the comparison of binomial proportions, see details in \code{\link{pairwiseCImethodsProp}}. }
}
\details{
 Note that all the computed intervals are without adjustment for multiplicity by default. 
 When based on crude normal approximations or crude non-parametric bootstrap methods, the derived confidence intervals can be unreliable for small sample sizes.
 The method implemented here split the data set into small subsets (according to the grouping variable in \kbd{formula} and the variable specified in \kbd{by})
 and compute confidence intervals only based each particular subset. Please note that, when a (generalized) linear model can be reasonable assumed to describe the 
 complete data set, it is smarter to compute confidence intervals based on the model estimators. Methods to do this are, e.g., implemented in the packages \pkg{stats}, \pkg{multcomp}, \pkg{mratios}.
}
\value{
 an object of class "pairwiseCI" structured as:

  a list containing:

  \item{byout}{A named list, ordered by the levels of the by-factor, where each element is a list
 containing the numeric vectors \kbd{estimate}, \kbd{lower}, \kbd{upper} and the character vector \kbd{compnames}}

  \item{bynames}{the level names of the by-factor}
 and further elements as input, try str() for details.

the object is printed by \code{\link{print.pairwiseCI}}.

}
\references{ 
\itemize{

\item \kbd{Param.diff} simply uses: \kbd{t.test} in \pkg{stats}, note that the default setting assums possibly heterogeneous variances (\kbd{var.equal=FALSE}).

\item \kbd{Param.ratio} for homogeneous variances (\kbd{var.equal=TRUE}): \bold{Fieller EC (1954):} Some problems in interval estimation. Journal of the Royal Statistical Society, Series B, 16, 175-185.

\item \kbd{Param.ratio} for heterogenous variances (\kbd{var.equal=FALSE}): : the test proposed in: \bold{Tamhane, AC, Logan, BR (2004):} Finding the maximum safe dose level for heteroscedastic data. Journal of Biopharmaceutical Statistics 14, 843-856.
 is inverted by solving a quadratic equation according to Fieller, where the estimated ratio is simply plugged in order to get Satterthwaite approximated degrees of freedom.
 See also: \bold{Hasler, M,  Vonk, R, Hothorn, LA:} Assessing non-inferiority of a new treatment in a three arm trial in the presence of heteroscedasticity. Statistics in Medicine 2007 (in press).

\item \kbd{Lognorm.ratio} and \kbd{Lognorm.ratio}: \bold{Chen, Y-H, Zhou, X-H (2006)}: Interval estimates for the ratio and the difference of two lognormal means.
Statistics in Medicine 25, 4099-4113.

\item \kbd{Np.re}: \bold{Brunner, E., Munzel, U. (2000).} The Nonparametric Behrens-Fisher Problem: Asymptotic Theory and a Small Sample Approximation. Biometrical Journal 42, 17 -25.
\bold{Neubert, K., Brunner, E., (2006).} A Studentized Permutation Test for the Nonparametric Behrens-Fisher Problem. Computational Statistics and Data Analysis.

\item \kbd{HL.diff}: \kbd{wilcox.exact} in \pkg{exactRankTests} 

\item \kbd{HL.ratio}: \bold{Hothorn, T, Munzel, U:} Non-parametric confidence interval for the ratio. Report University of Erlangen, Department Medical Statistics 2002;
 available via: http://www.imbe.med.uni-erlangen.de/~hothorn/
For the Hodges-Lehmann estimator see: \bold{Hodges, J.L., Lehmann E.L.(1963):} Estimates of location based on rank tests. Ann. Math. Statist. 34, 598-611.

\item \kbd{Median.diff}: The interval is calculated from a bootstrap sample (stratified according to the group variable) of the difference of sample Medians, using package boot.

\item \kbd{Median.ratio}: The interval is calculated from a bootstrap sample (stratified according to the group variable) of the ratio of sample Medians, using package boot.

Note, that the 4 bootstrap versions will hardly make sense for small samples, because of the discreteness of the resulting bootstrap sample.

\item \kbd{Poisson.ratio}: Profile-likelihood CI, based on the assumption of a Poisson distribution, basic method described in \bold{Venables, W.N., Ripley, B.D. (2002):} Modern Applied Statistics with S. Springer Verlag, New York.

\item \kbd{Quasipoisson.ratio}: Profile-deviance CI, based on the quasipoisson assumption, basic method  described in Venables, \bold{W.N., Ripley, B.D. (2002):} Modern Applied Statistics with S. Springer Verlag, New York.

\item \kbd{Negbin.ratio}: Profile-likelihood CI, based on the assumption of the negative binomial distribution, basic method  described in \bold{Venables, W.N., Ripley, B.D. (2002):} Modern Applied Statistics with S. Springer Verlag, New York.

\item \kbd{Prop.diff}: provides three approximate methods to calculate confidence intervals for the difference of proportions: Default is \kbd{CImethod="NHS"}: Newcombes Hybrid Score interval (Newcombe, 1998), other options are \kbd{CImethod="CC"} continuity corrected interval (Newcombe, 1998) as implemented in \kbd{prop.test(stats)} and \kbd{CImethod="AC"} Agresti-Caffo interval (Agresti and Caffo, 2000) 
\bold{Newcombe R.G. (1998):} Interval Estimation for the Difference Between Independent Proportions: Comparison of Eleven Methods. Statistics in Medicine 17, 873-890. 

\item \kbd{Prop.ratio} calculates the crude asymptotic interval (normal approximation on the log-scale, \kbd{CImethod="GNC"}) or the Score interval (\kbd{CImethod="Score"}), both described in:
\bold{Gart, JJ and Nam, J (1988)}: Approximate interval estimation of the ratio of binomial parameters: A review and corrections for skewness.
 Biometrics 44, 323-338. 

\item \kbd{Prop.or} Adjusted Woolf interval (\kbd{CImethod="Woolf"}), e.g. in: \bold{Lawson, R (2005):} Small sample confidence intervals for the odds ratio. Communication in Statistics Simulation and Computation, 33, 1095-1113.
 or the exact interval corresponding to Fishers exact test (\kbd{CImethod="Exact"}, taken from \kbd{fisher.test(stats)}, see references there)
 }
}

\seealso{
\code{\link{as.data.frame.pairwiseCI}} to create a data.frame from the output,
\code{\link{summary.pairwiseCI}} to create a more easily accessable list from the output,
\code{\link{plot.pairwiseCI}} to plot the intervals.
Further, see 
\pkg{multcomp} for simultaneous intervals for difference for various contrasts,
\pkg{mratios} for simultaneous intervals for the ratio of means,
\pkg{stats} \kbd{p.adjust}, \kbd{pairwise.t.test}, \kbd{pairwise.prop.test}, \kbd{pairwise.wilcox.test},
\kbd{TukeyHSD} for methods of multiple comparisons.}
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
 data=rooting, by=c("Age", "Position"), method="Prop.or")

aprootsOR

# Risk ratio

aprootsRR<-pairwiseCI(cbind(root, noroot) ~ IBA,
 data=rooting, by=c("Age", "Position"), method="Prop.ratio")

aprootsRR

# CI can be plotted:

plot(aprootsRR)

###############################################

# CIs assuming lognormal distribution of the response:

resp<-rlnorm(n=20, meanlog = 0, sdlog = 1)
treat<-as.factor(rep(c("A","B")))
datln<-data.frame(resp=resp, treat=treat)

pairwiseCI(resp~treat, data=datln, method="Lognorm.diff")
pairwiseCI(resp~treat, data=datln,  method="Lognorm.ratio")

}
\keyword{ htest }
\concept{confidence}

