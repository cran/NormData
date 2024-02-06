\name{Plot.Scatterplot.Matrix}
\alias{Plot.Scatterplot.Matrix}
\title{Explore data}
\description{
The function \code{Plot.Scatterplot.Matrix()} makes a scatterplot matrix of the specified variables.
}
\usage{
Plot.Scatterplot.Matrix(Dataset, Variables, 
Add.Jitter=0.1, Seed=123, ...)
}
\arguments{
\item{Dataset}{The name of the dataset.}
\item{Variables}{The names of the variables that should be shown in the scatterplot matrix.}
\item{Add.Jitter}{The amount of jitter (random noise) that should be added to the variables in the scatterplot matrix. Adding a bit of jitter is useful to show the inidividual data points more clearly, especially if several qualitative variables are added in the plot. The specified value \code{Add.Jitter=} in the function call determines the amount of jitter (range of values) that is added. For example, when \code{Add.Jitter=0.1}, a random value between -0.1 and 0.1 (sampled from a uniform distribution) is added to the datapoints. Default \code{Add.Jitter=0.1}.}
\item{Seed}{The seed that is used when adding jitter. Default \code{Seed=123}.}
\item{\dots}{Extra graphical parameters to be passed to \code{plot()}.}
}
\value{No return value, called for side effects.}
\details{
For details, see Van der Elst (2023).
}
\references{
Van der Elst, W. (2024). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} Springer Nature. 
}
\author{
Wim Van der Elst
}
\examples{
data(Substitution)

# Make a scatterplot matrix with the variables LDST, 
# Age, Gender and LE in the Substitution dataset
Plot.Scatterplot.Matrix(Dataset = Substitution, 
Variables = c("LDST", "Age", "Gender", "LE"))
}
\keyword{Explore data}