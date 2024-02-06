\name{Sandwich}
\alias{Sandwich}
\title{
Sandwich estimators for standard errors
}
\description{
The \code{Sandwich()} function can be used to obtain heteroscedasticity-consistent standard errors of the regression parameters of a fitted Stage 1 model. These are used to account for heteroscedasticity.
}
\usage{
Sandwich(Stage.1.Model, Type="HC0")
}
\arguments{
  \item{Stage.1.Model}{The fitted stage 1 model for which heteroscedasticity-consistent standard errors (sandwich estimators) for the standard errors of the regression parameters has to be provided.}
    \item{Type}{The type of the heteroscedasticity-consistent estimator that is used. By default, White's (White, 1980) estimator is used (i.e., \code{Type="HC0"}) but other estimators are available. For details, see the \code{vcovHC} function of the \code{sandwich} package. }
}
\value{
\item{Sandwich}{The fitted Stage 1 model with sandwich estimators.}
\item{Alpha}{The significance level that is used for inference. Default \code{Alpha=0.05}.}
}
\references{
Van der Elst, W. (2024). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} Springer Nature. 

White, H. (1980). A heteroscedasticity-consistent covariance matrix and a direct test for heteroscedasticity. Econometrica, 48, 817-838.
}
\author{
Wim Van der Elst
}
\seealso{\link{Stage.1}
}
\examples{
data(GCSE)
Model.1.GCSE <- Stage.1(Dataset = GCSE, Model = Science.Exam~Gender)
Sandwich(Stage.1.Model = Model.1.GCSE)
}
\keyword{Sandwich estimator}
\keyword{Heteroscedasticity}
