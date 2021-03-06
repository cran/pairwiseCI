\name{pairwiseMEP}
\alias{pairwiseMEP}
\alias{pairwiseMEP.data.frame}

\title{Wrapper to compute confidence intervals for multiple endpoints}
\description{
 This is a test version!
 Computes confidence intervals for pair wise comparisons of groups (assuming independent observations)
 for multiple endpoints. The methods available in pairwiseCI for continuous and count data can be called. Methods for
 binary data are currently not available. NOTE: Although multiple endpoints and multiple group wise comparisons are considered, there
 is no adjustment for multiplicity implemented in this function! 
}
\usage{

pairwiseMEP(x, ...)

\method{pairwiseMEP}{data.frame}(x, ep, f,
 control = NULL, conf.level = 0.95, 
 alternative = c("two.sided", "less", "greater"), 
 method = "Param.diff", ...)
}

\arguments{
  \item{x}{ a data.frame }
  \item{ep}{ a vector of character strings, naming the variables in \kbd{x} which are the response variables (endpoints) of interest }
  \item{f}{ a single character string, naming a factor variable in data which splits the dataset into treatment groups }
  \item{control}{ optionally, a single character string, naming a factor level in variable \kbd{f}, which shall be considered as control group; if omitted (default) all pairwise comparisons are computed }
  \item{conf.level}{ a single numeric between 0.5 and 1, specifying the local confidence level of the single confidence intervals}
  \item{alternative}{ a single character string, one of \kbd{'two.sided'}, \kbd{'less'}, \kbd{'greater'} }
  \item{method}{ a vector of character strings, specifying the method for computation of the confidence intervals, see \code{\link{pairwiseCImethodsCont}} and \code{\link{pairwiseCImethodsCount}} for possible options;
 must have length 1 or the same length as \kbd{ep}! }
  \item{\dots}{  further arguments to be passed to \code{\link{pairwiseCI}}, options are listed in \code{\link{pairwiseCImethodsCont}} and \code{\link{pairwiseCImethodsCount}}   }
}
\details{
 Calls \code{pairwiseCI}.
}
\value{

  \item{conf.int}{ a list with one element for each element in \kbd{ep}, containing the estimates, lower and upper limits and the comparison names and by levels in the format of a data.frame }
  \item{data}{ as input \kbd{x} }
  \item{ep}{ as input }
  \item{f}{ as input }
  \item{control}{ as input }
  \item{conf.level}{ as input }
  \item{alternative}{ as input }
  \item{method}{ as input }
}

\seealso{ The result can be plotted: \code{\link{plotCI.pairwiseMEP}},
 and coerced to a data.frame: \code{\link{as.data.frame.pairwiseMEP}} }

\examples{

x1<-rnorm(80,100,8)
x2<-rnbinom(80,mu=10, size=10)
A<-rep(c("a1","a2"), c(40,40))
B<-rep(rep(c("b1","b2"), c(20,20)), times=2)
dat<-data.frame(x1=x1,x2=x2,A=A, B=B)

test<-pairwiseMEP(x=dat, ep=c("x1","x2"), control="a1",
 f="A", by="B", method=c("Param.ratio","Negbin.ratio"))
test


plotCI(test, whichep=c("x1","x2"))

as.data.frame(test, whichep=c(1,2))

as.data.frame(test, whichep=c("x1","x2"))


}

\keyword{ htest }
\concept{ confidence interval }
