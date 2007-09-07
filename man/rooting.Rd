\name{rooting}
\alias{rooting}
\docType{data}
\title{Rooting of plants}
\description{
 Part of an experiment on propagation of plant genera Acer and Pyrus.
 Cuttings were taken from motherplants of age 5 and age 20, from top or base,
 and were treated with 0, 0.5 and 2 percent IBA to induce rooting.
 Treatments were arranged in a completely randomized design,
 among other variables, the number of cuttings with and without roots was recorded.
}
\usage{data(rooting)}
\format{
  A data frame with 48 observations on the following 6 variables.
  \describe{
    \item{\code{Age}}{a numeric vector: age of mother plants}
    \item{\code{Position}}{a factor with levels \code{B} and \code{T}, for "base" and "top" cuttings}
    \item{\code{IBA}}{a numeric vector, specifying the concentration of IBA}
    \item{\code{Rep}}{a numeric vector, number of replication}
    \item{\code{root}}{a numeric vector, number of cutting with successfull rooting, out of 12 trials}
    \item{\code{noroot}}{a numeric vector, number of cutting with successfull rooting, out of 12 trials}
  }
}
\details{
}
\source{
  Data taken from Msc thesis by Dawit Mamushet Yifru,
  Institute of Floriculture,
  Tree Nursery Science and Plant Breeding,
  University of Hannover, 2005. 
}
\examples{
data(rooting)

rooting$IBAf<-as.factor(rooting$IBA)
rooting$Rep<-as.factor(rooting$Rep)

fitB<-glm(cbind(root,noroot)~Rep+(Age + Position + IBA)^2,
 data=rooting, family=binomial)

fitQB<-glm(cbind(root,noroot)~Rep+(Age + Position + IBA)^2,
 data=rooting, family=quasibinomial)

summary(fitB)
summary(fitQB)

anova(fitB, test="Chisq")
anova(fitQB, test="F")

}
\keyword{datasets}
