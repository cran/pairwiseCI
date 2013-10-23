\name{pairwiseTest}
\alias{pairwiseTest}

\title{Wrapper to calculate unadjusted p-values for pairwise comparisons}
\description{Calculation of raw p-values for pairwise comparisons of several groups.
 The data can be split by additional factors. Any test function can be used,
 that takes two samples x,y as input and returns a list containing the p.value in an element named \kbd{p.value}.
 The output of this function might be further processed using \kbd{p.adjust} in order to adjust for multiple comparisons.
  }
\usage{
pairwiseTest(formula, data, by = NULL,
 method = "t.test", control = NULL, ...)

}
\arguments{
  \item{formula}{ a formula specifiying the response and the factor variable: \kbd{response ~ factor}  }
  \item{data}{ a data frame, containing the variables specified in \kbd{formula} }
  \item{by}{ optional vector of character strings, defining factors by which to split the data set.
 Then, pairwise comparisons are performed separately for each level of the specified factors.}
  \item{method}{ character string, giving the name of the function, which shall be used to calculate local p-values. Any function,
 taking two vectors x, and y as first arguments and returning a list with the p.value in a list element named \kbd{p.value} can be specified.}
  \item{control}{ optional character string, defining the name of a control group.
 Must be one of the levels of the factor variable defined in \kbd{formula}.
 By default control=NULL, then all pairwise comparisons between the levels of the factor variable are computed.  }
  \item{\dots}{ Arguments to be passed the function defined in \kbd{method}  }
}
\details{
This function splits the response variable according to the factor(s) specified in \kbd{by}, and within each subset
according to the grouping variable specified in \kbd{formula}. The function specified in \kbd{method} is called
to calculate a p.value for all pairwise comparisons of between the subsets, within each level of \kbd{by}.
The p-values are NOT adjusted for multiple hypothesis testing.

For binomial proportions, only \kbd{"Prop.test"} can be specified in the argument \kbd{method};
For continous variables, any function can be specified, which takes \kbd{x} and \kbd{y} as first arguments,
and returns a list containing a list containing the appropriate p-value in the element named \kbd{p.value}
 (as do the functions of class \kbd{"htest"}). See the examples for details. 
}
\value{
  A named list with elements
  \item{byout}{a list, containing the output of pairwiseTestint for each level of by,
 i.e. a data.frame containing with columns \kbd{p.value},\kbd{compnames} \kbd{groupx}, \kbd{groupy}}
  \item{bynames}{a character vector containing the names of the levels of the factors specified in \kbd{by} }
  \item{method}{a character string, name of the function used}
  \item{control}{a character string}
  \item{by}{vector of character strings, same as  argument \kbd{by}}
  \item{\dots}{further arguments that were passed to \kbd{FUN}}
}

\author{ Frank Schaarschmidt }

\seealso{ 
You can use \code{\link{summary.pairwiseTest}} to calculate multiplicity adjusted p-values from the output of pairwiseTest.

The following methods provide multiplicity adjusted p-values for various situations:
  \code{\link{pairwise.t.test}}, \code{\link{pairwise.prop.test}}, \kbd{\link{p.adjust}},
\kbd{summary.glht(multcomp)}, \kbd{simtest.ratio(mratios)}   }

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

  }

\keyword{ htest }

