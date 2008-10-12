\name{behenic}
\alias{behenic}
\docType{data}
\title{Measurements of behenic acid in two samples}
\description{
  Observations below a detection limit: in field trials with transgenic and isogenic corn
 varieties the behenic acid content was measured. Objective is to prove equivalence.
}
\usage{data(behenic)}
\format{
  A data frame with 12 observations on the following 2 variables.
  \describe{
    \item{\code{Treatment}}{a factor with 2 levels: \code{transgenic} \code{xisogenic}}
    \item{\code{Behenic}}{a numeric vector giving concentration of behenic acid, where 0.002 is the detection limit}
  }
}

\source{
  \emph{Oberdoerfer, R.B.} Example dataset from composition analyses of genetically modified oilseed rape seeds. 2003; BCS GmbH.
}

\examples{
data(behenic)
boxplot(Behenic~Treatment, data= behenic)
}
\keyword{datasets}


