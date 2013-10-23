\name{summary.pairwiseTest}

\alias{summary.pairwiseTest}

\title{ Summary function for "pairwiseTest" }
\description{ Creates a data.frame from the output of pairwiseTest,
 allows to adjust raw p-values by methods implemented in p.adjust.
  }
\usage{
\method{summary}{pairwiseTest}(object, digits = 4,
 p.adjust.method = "none", ...)
}

\arguments{
  \item{object}{ An object of class "pairwiseTest", created using the function \code{\link{pairwiseTest}} }
  \item{digits}{ number of digits for rounding of results }
  \item{p.adjust.method}{ Method to adjust p-values for multiple hypothesis testing, see options in \kbd{p.adjust.method} in \pkg{stats}.
  The default in this function in "none", resulting in unadjusted p-values}
  \item{\dots}{ Currently not used. }
}
\details{  
Coerces the raw p-values and the corresponding group levels to a data.frame and applies p.adjust to it. 
}
\value{
A dataframe, with columns
  \item{p.val.raw }{raw p-values}
  \item{p.val.adj }{adjusted p-values, according to the method specified in \kbd{p.adjust.method} }
  \item{comparison}{the calculated differences or ratios of parameters}
  \item{groupx }{levels of group x}
  \item{groupy }{levels of group y}
  and possibly further columns containing levels of by.
}

\examples{

data(Oats)
apOats<-pairwiseTest(yield~nitro, data=Oats,
 by="Variety", method="t.test", var.equal=FALSE)
apOats

# summary just creates a data.frame from the output
summary(apOats)

# an allows application of p.adjust
# on the p.values:

summary(apOats, p.adjust.method="holm")

# See ?p.adjust.methods for the methods available.

}
\keyword{htest}
\keyword{print}
\concept{multiple testing}
