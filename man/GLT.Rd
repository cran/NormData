\name{GLT}
\alias{GLT}
\title{Conduct the General Linear Test (GLT) procedure}
\description{
The function \code{GLT} fits two nested linear regression models (that are referred to as the unrestricted and the restricted models), and evaluates whether or not the fit of both models differs significantly. 
}
\usage{
GLT(Dataset, Unrestricted.Model, Restricted.Model, Alpha=0.05, 
Alpha.Homosc=0.05, Assume.Homoscedasticity=NULL)
}
\arguments{
  \item{Dataset}{A \code{data.frame} that should consist of one line per test participant. Each line should contain (at least) one test score and one independent variable.}
  \item{Unrestricted.Model}{The unrestricted regression model to be fitted. A formula should be provided using the syntaxis of the \code{lm} function (for help, see \code{?lm}). For example, \code{Test.Score~ Gender} will fit a linear regression model in which \code{Gender} is regressed on \code{Test.Score}. \code{Test.Score~Gender+Age+Gender:Age} will regress \code{Test.Score} on \code{Gender}, \code{Age}, and their interaction.}
  \item{Restricted.Model}{The restricted regression model to be fitted.} 
  \item{Alpha}{The significance level that should be used in the GLT procedure. Default \code{Alpha= 0.05}.}
    \item{Alpha.Homosc}{The significance level to conduct the homoscedasticity test. If the unrestricted model only contains qualitative independent variables, the Levene test is used. If the model contains at least one quantitative independent variables, the Breusch-Pagan test is used. If the homoscedasticity assumption is violated, a heteroscedasticity-robust \code{F*} test is provided. Default \code{Alpha.Homosc=0.05}.}
    \item{Assume.Homoscedasticity}{Logical. The \code{NormData} package `decides' whether the homoscedasticity assumption is valid based on the Levene (or Breusch-Pagan) test. \cr The \code{Assume.Homoscedasticity= TRUE/FALSE} argument can be used to overrule this decision process and `force' the \code{NormData} package to assume or not assume homoscedasticity.}
}
\details{
For details, see Van der Elst (2023).
}

\value{An object of class \code{GLT} with components,
  \item{F.Test.Stat.Results}{The result of the GLT procedure, i.e., the SSEs and DFs the fitted unrestricted and restricted models, and the \eqn{F^*} test-statistic.}
  \item{Fit.Unrestricted.Model}{The fitted unrestricted model.}
  \item{Fit.Restricted.Model}{The fitted restricted model.}
  \item{Alpha}{The significance level that was used.}
    \item{p.val.homoscedasticity}{The p-value that was used in the homoscedasticity test for the unrestricted model.}
\item{F.Test.Hetero.Robust}{The result of the heteroscedasticity-robust \code{F*} test. For details, see the \code{waldtest} function of the \code{lmtest package} (see \code{?waldtest}).}
\item{Alpha.Homoscedasticity}{The significance level that was used to conduct the homoscedasticity test. Default \code{Alpha.Homoscedasticity=0.05}.}
}
\references{
Van der Elst, W. (2024). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} Springer Nature. 
}
\author{
Wim Van der Elst
}
\examples{
# Replicate the GLT results that were obtained in 
# Case study 1 of Chapter 5 in Van der Elst (2023)
# ------------------------------------------------
data(Personality)

GLT.Openness <- GLT(Dataset=Personality,
     Unrestricted.Model=Openness~LE, Restricted.Model=Openness~1)
summary(GLT.Openness)

# Replicate the GLT results that were obtained in 
# Case study 2 of Chapter 5 in Van der Elst (2023)
# ------------------------------------------------
data(Fluency)

GLT.Fruits <- GLT(Dataset=Fluency,
     Unrestricted.Model=Fruits~LE, Restricted.Model=Fruits~1)
summary(GLT.Fruits)
}
\keyword{Fitted model}
\keyword{GLT}
\keyword{General Linear Test}
\keyword{F test-statistic}