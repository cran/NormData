\name{Check.Assum}
\alias{Check.Assum}
\title{Check assumptions for a fitted Stage 1 model}
\description{
Helper function to check the validity of the homoscedasticity and normality assumptions for a fitted Stage 1 model
}
\usage{
Check.Assum(Stage.1.Model)
}
\arguments{
  \item{Stage.1.Model}{The fitted \code{Stage.1} model.}
}
\details{
For details, see Van der Elst (2023).
}
\value{
An object of class \code{Check.Assum} with component,
\item{Assume.Homo.S2}{Is the homoscedasticity assumption valid?} 
\item{Assume.Normality.S2}{Is the normality assumption valid?} 
}
\references{
Van der Elst, W. (2023). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} New York: Springer. 
}
\author{
Wim Van der Elst
}
\seealso{\code{\link{Stage.1}}}
\examples{
data("Substitution")
# Fit a model with a linear mean prediction function
Fit <- Stage.1(Dataset = Substitution, Model = LDST~Age)
Check.Assum(Fit)
   # Output shows that the homoscedasticity and normality
   # assumptions are both violated
}
\keyword{Model fit}
