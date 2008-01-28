\name{pairwiseCI-package}
\alias{pairwiseCI-package}

\docType{package}

\title{
Wrapper function for two-sample confidence intervals and tests.
}

\description{
A collection of wrapper functions for simple evaluation of factorial trials.
The function pairwiseCI allows to calculate 2-sample confidence intervals for all-pairs and many-to-one comparisons
between levels of a factor. Intervals are NOT adjusted for multiple hypothesis testing per default.
The function pairwiseTest allows to calculate p-values of two-sample tests for all-pairs and many-to-one comparisons
between levels of a factor. P-values are NOT adjusted for multiple hypothesis testing per default.
Both function allow splitting of the data according to additional factors.
Intervals can be plotted, summary.pairwiseTest allows to use the p-value adjustments as implemented in p.adjust(stats).
Different test and interval methods (parametric, nonparametric, bootstrap for robust estimators of location, binomial proportions)
are implemented in a unified user level function.
}


\author{
Frank Schaarschmidt
Maintainer: Frank Schaarschmidt <schaarschmidt@biostat.uni-hannover.de>
}

\keyword{ package }

\seealso{
Multiple comparisons for the differences of means:
\code{\link[<multcomp>:<multcomp>-package]{<multcomp>}}

\code{pairwise.t.test(stats)}
\code{pairwise.prop.test(stats)}
\code{p.adjust(stats)}
}



\examples{

# some examples:
# In many cases the shown examples might not make sense,
# but display how the functions can be used.

data(Oats)
Oats

# # all pairwise comparisons,
# separately for each level of nitro:

apc <- pairwiseCI(yield ~ Variety, data=Oats,
 by="nitro", method="Param.diff")

apc

# Intervals can be plotted:

plot(apc)

# See ?pairwiseCI or ?pairwiseCImethodsCont
# for further options for intervals of 2 samples
# of continuous data.


# Or a test

apcTest <- pairwiseTest(yield ~ Variety, data=Oats,
 by="nitro", method="t.test")

# with holm-adjusted p-values:
summary(apcTest, p.adjust.method="holm")



# # If only comparisons to one control would be of interest:
# many to one comparisons, with variety Marvellous as control,
# for each level of nitro separately:

m21 <- pairwiseCI(yield ~ Variety, data=Oats,
 by="nitro", method="Param.diff", control="Marvellous")


##############################################
# # Proportions: another structure of the data is needed.
# Currently the data.frame data must contain two columns,
# specifying the number of successes and failures on each 
# unit.

# The rooting example:
# Calculate confidence intervals for the 
# difference of proportions between the 3 doses of IBA,
# separately for 4 combinations of "Age" and "Position".
# Note: we pool over Rep in that way. Whether this makes
# sense or not, is decision of the user.

data(rooting)
rooting

# Confidence intervals for the risk difference

aprootsRD<-pairwiseCI(cbind(root, noroot) ~ IBA,
 data=rooting, by=c("Age", "Position"), method="Prop.diff")

# See ?pairwiseCIProp for further options to compare proportions

# Or a test:

aprootsTest<-pairwiseTest(cbind(root, noroot) ~ IBA,
 data=rooting, by=c("Age", "Position"), method="Prop.test")
aprootsTest

summary(aprootsTest, p.adjust.method="holm")

}
\keyword{ htest }

