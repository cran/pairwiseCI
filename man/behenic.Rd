\name{behenic}
\alias{behenic}
\docType{data}
\title{ Measurements of behenic acid}
\description{
  Observations below a detection limit: in field trials with transgeneic and isogenic corn varieties the behenic acid content
}
\usage{data(behenic)}
\format{
  A data frame with 12 observations on the following 2 variables.
  \describe{
    \item{\code{Treatment}}{a factor with 2 levels: \code{transgenic} \code{xisogenic}}
    \item{\code{Behenic}}{a numeric vector giving concentration of behenic acid, where 0.002 is the detection limit}
  }
}
\details{

}
\source{
  Oberdoerfer, R.B. Example dataset from composition analyses of genetically modified oilseed rape seeds. 2003; BCS GmbH.
}

\examples{
data(behenic)
boxplot(Behenic~Treatment, data= behenic)
}
\keyword{datasets}


