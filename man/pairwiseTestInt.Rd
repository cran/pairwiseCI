\name{pairwiseTestInt}

\alias{pairwiseTestInt}
\alias{pairwiseTestProp}
\alias{pairwiseTestCont}

\title{ Internal functions for pairwiseTest }
\description{ Only for internal use by pairwiseTest.
Two different functions for data representable as a two numeric vectors (pairwiseTestCont) and
 data representable as matrix with two columns (pairwiseTestProp) as created can be done with a formula
 like \code{cbind(successes, failures) ~ group}.
 Functions that split up a data.frame according to one factor, and perform all pairwise comparisons
 and comparisons to control among the levels of the factor by calling functions that can deal with two vectors x and y
 or the one documented in ?Prop.test.
  }
\usage{

pairwiseTestCont(formula, data, alternative="two.sided", control=NULL, method, ...)
pairwiseTestProp(formula, data, alternative="two.sided", control=NULL, method, ...)

}
\arguments{
  \item{formula}{ a formula specifiying the response and the factor variable: \code{response ~ factor}  }
 
 \item{data}{ a data frame, containing the variables specified in \code{formula} }

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
For internal use in pairwiseTest.
}
\value{
a data.frame containing the columns
\item{p.value}{numeric vector: the p.values}
\item{compnames}{character vector: the names of the comparisons performed}
\item{groupx}{character vector: the names of the first group}
\item{groupy}{character vector: the names of the second group}
}

\seealso{ \code{\link{pairwiseTest}} for a user level function, 
and 
 \code{\link{pairwise.t.test}}, \code{\link{pairwise.prop.test}}, \code{\link{p.adjust}} for further functions to calculate multiplicity adjusted p-values.  }

\keyword{ htest }

