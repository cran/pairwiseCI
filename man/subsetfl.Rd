\name{subsetfl}
\alias{subsetfl}

\title{ Subset a data.frame and reorder the levels of factors in it}
\description{
 This is a test version!
 A wrapper to change the order of levels of one or several factors in a data.frame.
 When factor levels are omitted, only the relevant subset of the data.frame is returned.
}
\usage{
subsetfl(data, order)
}

\arguments{
  \item{data}{ a \code{data.frame} }
  \item{order}{ a named list, with elements containing either integer vectors or character verctors.
The names of the list should correspond to factor variables in \code{data}; if the elements of the list are character vectors, then they should name all or a subset of the 
levels of the corresponding factor variable in \code{data} }
}
\details{

This is a test version. Please check the result.

Reorders the levels of factor variables as specified in \code{order}. If some factor levels are omitted which are present in \code{data}, only the relevant subset of \code{data} is returned.
The order of values in the data set remains the same, but the order in plots (e.g. \code{boxplot}), contrasts (\code{lm}) is changed.

}
\value{
A data.frame, being (a subset of) the input data.frame with order of the levels changed according to the specification in \code{order}.

}

\seealso{ The function \code{\link{factor}} is used internally. }
\examples{

x1<-round(rnorm(120,20,2))
x2<-round(rnorm(120,100,8))
A<-rep(c("a1","a2","a3"), c(40,40,40))
B<-rep(rep(c("b1","b2","b3","b4"), c(10,10,10,10)), times=3)
C<-rep(rep(c("D0","D5","D10","D50","D100"), times=24))

dat<-data.frame(x1=x1,x2=x2, Dose=C, A=A, B=B)



str(dat)
levels(dat$Dosis)
levels(dat$A)
levels(dat$B)

boxplot(x1~Dose, data=dat)

pairwiseCI(x1~Dose,data=dat, control="D0")

datC<-subsetfl(data=dat,
 order=list("Dose"=c("D0","D5","D10","D50","D100")))

datC

str(datC)
levels(datC$Dosis)
levels(datC$A)
levels(datC$B)

boxplot(x1~Dose, data=datC)


pairwiseCI(x1~Dose,data=datC, control="D0")

}

\keyword{ misc }
\concept{ factor }

