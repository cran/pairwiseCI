\name{summary.pairwiseTest}

\alias{summary.pairwiseTest}

\title{ Summary function for pairwiseTest objects }
\description{ Creates a data.frame from the output of pairwiseTest
  }
\usage{
\method{summary}{pairwiseTest}(object, digits = 4, p.adjust.method = "none", letters = FALSE, ...)
}

\arguments{
  \item{object}{ An object of class "pairwiseTest", created using the function \code{\link{pairwiseTest}} }
  \item{digits}{ number of digits for rounding of results }
  \item{p.adjust.method}{ Method to adjust p-values for multiple hypothesis testing, see options in \code{p.adjust.method} in \code{stats}.
  The default in this function in "none", resulting in raw p.values.}
  \item{letters}{logical value, indicating, whether grouping letters for non significant differences shall be calculated. Default is FALSE.
 This argument is ignored, if a by-level was specified in \code{pairwiseTest}, or if a control group was specified.  }
  \item{\dots}{ Currently not used. }
}
\details{
Testversion.  
}
\value{
A dataframe, with columns
  \item{p.value }{p.values}
  \item{groupx }{levels of group x}
  \item{groupy }{levesl of group y}
  and possibly further columns containing levels of by.
}

\author{ Frank Schaarschmidt }
\note{ 
Testversion.
}

\examples{

data(Oats)
apOats<-pairwiseTest(yield~nitro, data=Oats, by="Variety", method="t.test", var.equal=FALSE)
apOats

# summary just creates a data.frame from the output
summary(apOats)

# an allows application of p.adjust
# on the p.values:

summary(apOats, p.adjust.method="holm")

# See ?p.adjust.methods for the methods available.

###############################################

#  For one-way layouts also
#  letters can be calculated

set.seed(1234)

resp<-rnorm(n=100,
 mean=rep(c(2,5,5,5,5,10,18,5,5,5), each=10),
 sd=rep(c(1,2,2,2,3,5,5,2,2,2), each=10)
)

noise<-rbinom(n=100, size=1, prob=0.05)

resp<-resp + noise*rnorm(n=100,mean=10, sd=10)

fact<-as.factor(rep(LETTERS[1:10], each=10))
data<-data.frame(resp=resp, fact=fact)

boxplot(resp~fact)

# All pairwise comparisons using Welch-tests

aptestW<-pairwiseTest(resp~fact, data=data, method="t.test", var.equal=FALSE)
aptestW
summary(aptestW, p.adjust.method="holm", letters=TRUE)

summary(aptestW, p.adjust.method="holm", letters=TRUE, alpha=0.1)

summary(aptestW, p.adjust.method="bonf")

###############################################

}
\keyword{ htest }
