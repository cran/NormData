\name{Coding}
\alias{Coding}
\title{
Check the coding of a variable
}
\description{
This function checks the coding of a variable, e.g., the dummy-coding scheme that will be used for binary or qualitative variables. 
}
\usage{
Coding(x, verbose=TRUE)
}
\arguments{
  \item{x}{The variable to be evaluated.}
  \item{verbose}{A logical value indicating whether verbose output should be generated.}
}
\value{No return value, called for side effects.}
\references{
Van der Elst, W. (2023). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} New York: Springer. 
}
\author{
Wim Van der Elst
}
\examples{
data(Substitution)
Coding(Substitution$LE)
}
