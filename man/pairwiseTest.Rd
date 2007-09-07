\name{pairwiseTest}
\alias{pairwiseTest}

\title{ Calculate unadjusted p-values for pairwise comparisons }
\description{ Calculation of raw p-values for pairwise comparisons of several groups.
 The data can be split by additional factors. Any test function can be used,
 that takes two samples x,y as input and returns a list containing the p.value in an element named \code{$p.value}.
 The output of this function might be further processed using \code{\link{p.adjust}} in order to adjust for multiple comparisons.
  }
\usage{
pairwiseTest(formula, data, by = NULL,
 alternative = "two.sided", method = "t.test",
 control = NULL, ...)

}
\arguments{
  \item{formula}{ a formula specifiying the response and the factor variable: \code{response ~ factor}  }
  \item{data}{ a data frame, containing the variables specified in \code{formula} }
  \item{by}{ optional vector of character strings, defining factors by which to split the data set.
 Then, pairwise comparisons are performed separately for each level of the specified factors.}
  \item{alternative}{ character string, defining whether the direction of the alternative hypothesis,
 passed to the function defined in \code{method} }
  \item{method}{ character string, giving the name of the function, which shall be used to calculate local p-values. Any function,
 taking two vectors x, and y as first arguments and returning a list with the p.value in a list element named \code{p.value} can be specified.}
  \item{control}{ optional character string, defining the name of a control group.
 Must be one of the levels of the factor variable defined in \code{formula}.
 By default control=NULL, then all pairwise comparisons between the levels of the factor variable are computed.  }
  \item{\dots}{ Arguments to be passed the function defined in \code{method}  }
}
\details{
This function splits the response variable according to the factor(s) specified in \code{by}, and within each subset
according to the grouping variable specified in \code{formula}. The function specified in \code{method} is called
to calculate a p.value for all pairwise comparisons of between the subsets, within each level of \code{by}.
The p-values are NOT adjusted for multiple hypothesis testing.

For binomial proportions, only \code{"Prop.test"} can be specified in the argument \code{method};
For continous variables, any function can be specified, which takes \code{x} and \code{y} as first arguments,
and returns a list containing a list containing the appropriate p-value in the element named \code{p.value}
 (as do the functions of class \code{"htest"}). See the examples for details. 
}
\value{
  A named list with elements
  \item{byout}{a list, containing the output of pairwiseTestint for each level of by,
 i.e. a data.frame containing with columns \code{p.value},\code{compnames} \code{groupx}, \code{groupy}}
  \item{bynames}{a character vector containing the names of the levels of the factors specified in \code{by} }
  \item{alternative}{a character string}
  \item{method}{a character string, name of the function used}
  \item{control}{a character string}
  \item{by}{vector of character strings, same as  argument \code{by}}
  \item{\dots}{further arguments that were passed to \code{FUN}}
}

\author{ Frank Schaarschmidt }

\seealso{ 
You can use \code{\link{summary.pairwiseTest}} to calculate multiplicity adjusted p-values from the output of pairwiseTest.

The following methods provide multiplicity adjusted p-values for various situations:
  \code{\link{pairwise.t.test}}, \code{\link{pairwise.prop.test}}, \code{\link{p.adjust}},
\code{summary.glht(multcomp)}, \code{simtest.ratio(mratios)}   }

\examples{

#######################################################
# The rooting example:
# Calculate confidence intervals for the 
# difference of proportions between the 3 doses of IBA,
# separately for 4 combinations of "Age" and "Position".
# Note: we pool over Rep in that way. Whether this makes
# sense or not, is decision of the user.

data(rooting)

# Pairwise Chi-square tests:

aproots<-pairwiseTest(cbind(root, noroot) ~ IBA,
 data=rooting, by=c("Age", "Position"), method="Prop.test")

aproots

# With Holm adjustment for multiple hypotheses testing:

summary(aproots, p.adjust.method="holm")


#########################################################

data(Oats)


apc <- pairwiseTest(yield ~ nitro, data=Oats,
 by="Variety", method="wilcox.test")

apc

summary(apc)
summary(apc, p.adjust.method="holm")


# # many to one comparisons, with variety Marvellous as control,
# for each level of nitro separately:

m21 <- pairwiseTest(yield ~ Variety, data=Oats,
 by="nitro", method="perm.test", control="Marvellous")
m21


############################################################

set.seed(1234)

resp<-rnorm(n=100,
 mean=rep(c(2,5,5,5,5,10,18,5,5,5),each=10),
 sd=rep(c(1,2,2,2,3,5,5,2,2,2), each=10)
)

noise<-rbinom(n=100, size=1, prob=0.05)

resp<-resp + noise*rnorm(n=100,mean=10, sd=10)

fact<-as.factor(rep(LETTERS[1:10], each=10))
data<-data.frame(resp=resp, fact=fact)

boxplot(resp~fact)

# All pairwise comparisons using Welch-tests

aptestW<-pairwiseTest(resp~fact, data=data, method="t.test",
 var.equal=FALSE)
aptestW
summary(aptestW, p.adjust.method="holm", letters=TRUE)

# All pairwise comparisons using Wilcoxon tests

aptestWi<-pairwiseTest(resp~fact, data=data, method="wilcox.test")
aptestWi
summary(aptestWi, p.adjust.method="holm", letters=TRUE)

# All pairwise comparisons using Permutation tests 
# from library exactRankTests

aptestP<-pairwiseTest(resp~fact, data=data, method="perm.test")
aptestP
summary(aptestP, p.adjust.method="holm", letters=TRUE)


#####################################
# Petersens bean example:

data(bean)
boxplot(yield ~ P*T, data=bean)
bean$trt<-paste(bean$P, bean$T, sep="")
bean
# There might be interaction AND heterogeneity of variances.
# There are definitely smarter ways to analyze these data.
# But a simple-minded way is:

pairwiseTest(yield~trt,data=bean, method="var.test")

# or
# Might there be different variances between the
# comparisons of interest?

pairwiseTest(yield~P,data=bean, by="T", control="P0",
 method="var.test")

# Which Phosphor doses lead to increase of yield
# compared to P0, separate for the two types?

compP<-pairwiseTest(yield~P,data=bean, by="T",
 control="P0", method="t.test", var.equal=FALSE,
 alternative="greater")

# P-values adjusted according to Holm:

summary(compP, p.adjust.method="holm")

# confidence intervals for the same problem,
# using Bonferroni-adjustment:

CIcompP<-pairwiseCI(yield~P, data=bean, by="T", 
 control="P0", method="Param.diff", var.equal=FALSE,
 alternative="greater", conf.level=1-0.05/4)

plot(CIcompP)

  }
}
\keyword{ htest }

