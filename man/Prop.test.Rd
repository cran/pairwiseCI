\name{Prop.test}
\alias{Prop.test}

\title{ Wrapper to prop.test(stats)}
\description{
  Only for internal use in pairwiseTest.
}
\usage{
Prop.test(x, y, alternative = "two.sided", ...)
}
\arguments{
  \item{x}{ a vector of success and failure in sample x,
 or a data.frame with a column of successes and a column of failures, then colSums are used. }
  \item{y}{ a vector of success and failure in sample y,
 or a data.frame with a column of successes and a column of failures, then colSums are used. }
  \item{alternative}{ character string defining the alternative hypothesis }
  \item{\dots}{ arguments to be passed to \code{prop.test(stats)} }
}
\details{
Just a wrapper function to call \code{prop.test(stats)}. 
If x, y are data.frames containing two columns taken to be counts of successes and counts of failures, columnwise sums of x,y are calculated.
The total number of successes and the total number of trials is then passed to prop.test.
}
\value{
   An object of class "htest", as defined by \code{prop.test(stats)}
}

\seealso{\code{\prop.test}, and \code{pairwise.prop.test} in \code{stats}}
\examples{


# If input is a data.frame:

set.seed(1234)

trials=rep(20,8)
success <- rbinom(n=8, size=trials,
 prob=c(0.2,0.2,0.2,0.2, 0.3,0.3,0.3,0.3))
failure <- trials-success

f<-as.factor(rep(c("group1", "group2"), each=4))

data<-data.frame(success=success, failure=failure, f=f)

g1<-subset(data, f=="group1")[,c("success","failure")]
g2<-subset(data, f=="group2")[,c("success","failure")]

g1
g2

# Prop.test calculates the columnwise sums and calls prop.test stats:

Prop.test(x=g1, y=g2)

# should be the same as:

CS1<-colSums(g1)
CS2<-colSums(g2)

CS1
CS2

prop.test(x=c(CS1[1], CS2[1]), n=c(sum(CS1), sum(CS2)))

}
\keyword{ htest }