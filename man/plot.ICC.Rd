\name{plot ICC}
\alias{plot ICC}
\alias{plot.ICC}
\title{Graphical depiction of the ICC.}
\description{
The ICC corresponds to the proportion of the total variance in the residuals that is accounted for by the clustering variable at hand (Kutner \emph{et al.}, 2005). This function visualizes the extent ot which there is clustering in the dataset. 
}
\usage{
\method{plot}{ICC}(x, X.Lab="Cluster", Y.Lab="Test score", 
Main="", Add.Jitter=0.2, Size.Points=1, Size.Labels=1, 
Add.Mean.Per.Cluster=TRUE, Col.Mean.Symbol="red", Seed=123, 
...)
}
\arguments{
  \item{x}{A fitted object of class \code{ICC}.}
  \item{X.Lab}{The label that should be added to the X-axis. \code{X.Lab="Cluster"}.}
  \item{Y.Lab}{The label that should be added to the Y-axis. \code{Y.Lab="Test score"}.}
  \item{Main}{The title of the plot. Default \code{Main=" "}, i.e., no title.}
  \item{Add.Jitter}{The amount of jitter (random noise) that should be added in the horizontal direction (predicted scores, X-axis) of the plot. Adding a bit of jitter is useful to show the inidividual data points more clearly. The specified value \code{Add.Jitter=} in the function call determines the amount of jitter (range of values) that is added. For example, when \code{Add.Jitter=0.2}, a random value between -0.2 and 0.2 (sampled from a uniform) is added to the X-axis. Default \code{Add.Jitter=0.2}.}
  \item{Size.Points}{The size of the points in the plot. Default \code{Size.Points=1}.}
  \item{Size.Labels}{The size of the Labels of the X-axis in the plot. Default \code{Size.Labels=1}.}
\item{Add.Mean.Per.Cluster}{Logical. Should the means per cluster be shown? \cr Default \code{Add.Mean.Per.Cluster=TRUE}.}
\item{Col.Mean.Symbol}{The color of the symbol that is used to indicate the mean (for each of the clusters). Default \code{Col.Mean.Symbol="red"}.}
\item{Seed}{The random seed that is used to add jitter. Default \code{Seed=123}.}
\item{...}{Other arguments to be passed to the plot function.}
}
\value{No return value, called for side effects.}
\references{
Kutner, M. H., Nachtsheim, C. J., Neter, J., and Li, W. (2005). \emph{Applied linear statistical models} (5th edition). New York: McGraw Hill.

Van der Elst, W. (2024). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} Springer Nature. 
}
\author{
Wim Van der Elst
}
\seealso{\link{ICC}
}
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

# Make points in the plot a bit larger and reduce 
# the size of labels on the X-axis (initials test administrators)
plot(ICC_LDST, Size.Labels = .5, Size.Points=.5)
}
\keyword{Plot ICC}
\keyword{Clustering}
\keyword{Intra class correlation}