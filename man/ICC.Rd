\name{ICC}
\alias{ICC}
\title{Intra class correlation}
\description{
The function ICC computes the intra class correlation. The ICC corresponds to the proportion of the total variance in the residuals that is accounted for by the clustering variable at hand (Kutner \emph{et al.}, 2005).
}
\usage{
ICC(Cluster, Test.Score, Dataset, CI = 0.95)
}
\arguments{
\item{Cluster}{The name of the clustering variable in the dataset.}
\item{Test.Score}{The name of the outcome variable in the dataset (e.g., a test score).}
\item{Dataset}{A dataset.}
\item{CI}{The required confidence limits around the ICC. Default \code{CI=.95}, i.e. the 95 CI.}
}
\details{
This function is a modification of the \code{ICCest} function from the \code{ICC} package (v2.3.0), with minimal changes. For details of the original function, see https://cran.r-project.org/web/packages/ICC/ICC.pdf. The author of the original function is Matthew Wolak.}

\value{
An object of class \code{ICC} with component,
\item{ICC}{The intra class correlation coefficient.} 
\item{LowerCI}{The lower bound of the CI around the ICC.}
\item{UpperCI}{The upper bound of the CI around the ICC.}
\item{Num.Clusters}{The number of clusters in the dataset.}
\item{Mean.Cluster.Size}{The mean number of observations per cluster.}
\item{Data}{The dataset used in the analysis (observations with missing values are excluded).}
\item{N.Dataset}{The sample size of the full dataset.}
\item{N.Removed}{The number of observations that are removed due to missingness.}
\item{alpha}{The specified \eqn{\alpha}-level for the CI, i.e., \eqn{\alpha} = 1 - CI.}
\item{Labels.Cluster}{The labels of the clustering variable.}
}
\references{
https://cran.r-project.org/web/packages/ICC/ICC.pdf

Kutner, M. H., Nachtsheim, C. J., Neter, J., and Li, W. (2005). \emph{Applied linear statistical models} (5th edition). New York: McGraw Hill.

Van der Elst, W. (2023). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} New York: Springer. 
}
\author{
Original function: Matthew Wolak (with some small modifications by Wim Van der Elst)
}
\seealso{\link{plot.ICC}}
\examples{
# Compute ICC in Substitution dataset, using Test.Administrator as 
# clustering unit
data(Substitution)

# Add administrator to the dataset (just randomly allocate labels 
# as Test.Administrator, so ICC should be approx. 0)
Substitution$Test.Adminstrator <- NA
Substitution$Test.Adminstrator <- sample(LETTERS[1:10], 
  replace = TRUE, size = length(Substitution$Test.Adminstrator))
Substitution$Test.Adminstrator <- 
  as.factor(Substitution$Test.Adminstrator)

ICC_LDST <- ICC(Cluster = Test.Adminstrator, Test.Score = LDST, Data = Substitution)

# Explore results
summary(ICC_LDST)
plot(ICC_LDST)
}
\keyword{Plot ICC}
\keyword{Clustering}
\keyword{Intra class correlation}
