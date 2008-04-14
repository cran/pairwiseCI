\name{profileDG}
\alias{profileDG}
\title{ Construct a (quasi-) likelihood-profile }
\description{
  Construct a (quasi-) likelihood-profile based on a glm-fit. For internal use for functions constructing profile-likelihood confidence intervals.
}
\usage{
profileDG(fit, steps = 100, wh = 1:p)
}
\arguments{
  \item{fit}{ an object of class "glm" or "negbin"}
  \item{steps}{ a single integer value, the number of steps for the profile }
  \item{wh}{ wh }
}
\details{
  An adaptation of the code in \code{profile.glm}. Will work also for the case that only 0-values occur in one group.
  For internal use
}
\value{
  An object of class "profile.glm", "profile".
}

\author{ Daniel Gerhard }

\keyword{ htest}
