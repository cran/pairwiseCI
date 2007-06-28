\name{nonsigletters}
\alias{nonsigletters}

\title{ Function to assign letters to groups which are not significantly different}
\description{
  Function to assign letters to groups which are not significantly different in all pairwise comparisons.

}
\usage{
nonsigletters(factor1, factor2, pvalue, alpha = 0.05)
}
\arguments{
  \item{factor1}{ A factor, specifying the group names (of the first group) for each comparisons }
  \item{factor2}{ A factor, specifying the group names (of the second group) for each comparisons }
  \item{pvalue}{ a vector of p-values of the comparisons defined by the \code{factor1} and \code{factor2} }
  \item{alpha}{ a single numeric value, defining the alpha-level, for which a difference is considered "non significant" }
}
\details{
  A testversion.
 Works only for all pairwise comparisons in a one-way layout.
 This function builds groups which are not significantly different from each other.
 For this grouping, the size of teststatistics etc. as can be used in t-Tests, is NOT used.
 Only the information of nonsignificance contained in a full matrix of p-values of all pairwise comparisons is used.
 Therefore, grouping letters can be assigned after any valid two-sample test.
 The letters are NOT ordered by magitude of means of the groups. Instead, the first letter is assigned to that group with most members.
}
\value{
  If it is a LIST, use
  \item{group }{the level name}
  \item{letter }{letters defining groups consisting of levels which are not significantly different }
}

\author{ Daniel Gerhard, Frank Schaarschmidt }
\note{ Testversion.
 }
\seealso{ package \pkg{multcompView} }
\examples{

######################################

# An artificial example

set.seed(1234)

n<-rep(100,10)
x<-rbinom(n=10, size=n,
 prob=c(0.1, 0.1, 0.2, 0.2, 0.2, 0.2, 0.3, 0.1, 0.1, 0.1))
f<-as.factor(LETTERS[1:10])

data<-data.frame(success=x, failure=n-x, f=f)
data

# calculate p.values:

test<-pairwiseTest(cbind(success, failure)~f,
 data=data, method="Prop.test")

stest<- summary(test, p.adjust.method="holm")

stest

# assign equal letters to groups 
# which are not significantly different
# according to the holm-adjusted p.values:

nonsigletters(pvalue=stest$p.val.adj,
 factor1=stest$groupx,
 factor2=stest$groupy)



###################################


a <- c("A","A","A", "B","B", "C")
b <- c("B","C","D", "C","D", "D")
pv <- c(1,1,0, 1,0, 0)

nonsigletters(a,b,pv)

# This still needs to be fixed:

a <- c("A","A","A", "B","B", "C")
b <- c("B","C","D", "C","D", "D")
pv <- c(1,1,1, 1,1, 1)

nonsigletters(a,b,pv)



factor1<-c("A","A","A","A","A", "B","B","B","B",
 "C","C","C", "D","D", "E")
factor2<-c("B","C","D","E","F", "C","D","E","F",
 "D","E","F", "E","F", "F")
pv <- c(0,1,1,1,0, 1,0,1,0, 1,1,0, 0,1, 0)



nonsigletters(factor1, factor2, pv)

# Currently, this nonsense just results in a warning:

# factor1<-c("A","A","A","A","A", "B","B","B","B", "C","C","C")
# factor2<-c("B","C","D","E","F", "C","D","E","F", "D","E","F")
# pv <- c(0,1,1,1,0, 1,0,1,0, 1,1,0)

# nonsigletters(factor1, factor2, pv)

}

\keyword{ htest }
