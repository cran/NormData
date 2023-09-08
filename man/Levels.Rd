\name{Levels}
\alias{Levels}
\title{Explore data}
\description{
Gives the levels of a variable.
}
\usage{
Levels(x)
}
\arguments{
  \item{x}{A variable for which the different levels should be printed.}
}
\value{No return value, called for side effects.}
\details{
For details, see Van der Elst (2023).
}
\references{
Van der Elst, W. (2023). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} New York: Springer. 
}
\author{
Wim Van der Elst
}
\examples{
data(Substitution)
Levels(Substitution$Gender)
}
\keyword{Levels}