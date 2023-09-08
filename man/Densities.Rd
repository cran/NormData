\name{Densities}
\alias{Densities}
\title{Plot densities}
\description{
Plot densities for an outcome for different subgroups.
}
\usage{
Densities(Dataset, Test.Score, IV, Color=TRUE, 
Size.Legend=1, xlab="Test score", main, \dots)
}
\arguments{
  \item{Dataset}{The name of the dataset.}
  \item{Test.Score}{The name of the outcome variable (e.g., a raw test score).}
  \item{IV}{The name of the stratification variable, that defines for which subgroups density plots should be provided. If \code{IV} is not specified, a single density is shown (no subgroups).}
  \item{Color}{Logical. Should densities for different subgroups be depicted in color? Default \code{Color=TRUE}.}
  \item{Size.Legend}{The size of the legend in the plot. Default \code{Size.Legend=1}.}
\item{xlab}{The label on the X-axis. Default \code{xlab="Test score"}.}
\item{main}{The title of the plot.}
\item{\dots}{Other arguments to be passed to the \code{plot(function)}, e.g. \code{xlim=c(0, 100)}.}
}
\value{No return value, called for side effects.}
\references{
Van der Elst, W. (2023). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} New York: Springer. 
}
\author{
Wim Van der Elst
}
\examples{
# Plot Gender-specific densities of the raw science exam 
# scores in the GCSE dataset
data(GCSE)
Densities(Dataset = GCSE, Test.Score = Science.Exam, IV=Gender)

# Plot LE-specific densities of the residuals of a model
# where the Openness scale score is regressed on LE
data(Personality)
Fit <- Stage.1(Dataset = Personality, Model = Openness~LE)
summary(Fit)
Data.With.Residuals <- data.frame(Personality, 
  Fit$HomoNorm$Residuals)
Densities(Dataset = Data.With.Residuals, 
  Test.Score = Fit.HomoNorm.Residuals, IV = LE)

}
\keyword{Plot}
\keyword{Exploratory}