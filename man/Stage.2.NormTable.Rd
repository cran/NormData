\name{Stage.2.NormTable}
\alias{Stage.2.NormTable}
\title{Derive a normative table}
\description{
This function allows for deriving a normative table that shows percentile ranks \eqn{\hat{\pi}_0} that correspond to a wide range of raw test scores \eqn{Y_0} (stratified by the relevant independent variables). 
}
\usage{
Stage.2.NormTable(Stage.1.Model, Assume.Homoscedasticity, 
Assume.Normality, Grid.Norm.Table, Test.Scores, Digits=6, 
Rounded=TRUE)
}
\arguments{
  \item{Stage.1.Model}{A fitted object of class \code{Stage.1} that should be used to derive the normative table.}
  \item{Assume.Homoscedasticity}{Logical. Should homoscedasticity be assumed when deriving the normative table? By default, homoscedasticity is assumed when the \eqn{p}-value of the Levene or Breusch-Pagan test for the fitted Stage 1 model is above the specified \eqn{\alpha}-level in the \code{Stage.1()} function call. When homoscedasticity is assumed, an overall residual standard error is used in the derivation of the normative table. When homoscedasticity is not assumed, prediction-specific residual standard errors used. The default decision procedure can be overruled by means of the arguments argument \code{Assume.Homoscedasticity=TRUE} or \code{Assume.Homoscedasticity=FALSE}.}
  \item{Assume.Normality}{Logical. Should normality of the standardized errors be assumed when deriving the normative table? By default, normality is assumed when the \eqn{p}-value of the Shapiro-Wilk test for the fitted Stage 1 model is above the specified \eqn{\alpha}-level in the \code{Stage.1()} function call. When normality is assumed, the \eqn{Y_0} to  \eqn{\hat{\pi}_0} conversions in the normative table are based on the CDF of the standard normal distribution. When normality is not assumed, these conversions are based on the CDF of the standardized residuals in the normative sample. The default decision procedure can be overruled by means of the arguments argument \code{Assume.Normality=TRUE} or \code{Assume.Normality=FALSE}.}
  \item{Grid.Norm.Table}{A \code{data.frame} that specifies the name of the independent variable(s) (e.g., \code{Gender}) and the levels (e.g., \code{"F"} and \code{"M"}) or values (e.g., \code{Age.C=seq(from=20, to=80, by=1)-50)}) for which the estimated percentile ranks should be tabulated. Both the name of the independent variable and the coding scheme that is specified in the \code{Grid.Norm.Table=...} argument should exactly match the name of the independent variable and the original coding scheme that was used in the \code{Stage.1()} function call. For example, if the variable name \code{Gender} with original coding scheme \code{F} and \code{M} was used in the \code{Stage.1()} function call, the same should be done in the \code{Stage.2.NormTable()} function call. So \code{Grid.Norm.Table= data.frame(Gender=c("F", "M")}) should be used, and not e.g., \code{Grid.Norm.Table= data.frame(GenderM=c(0,1))}. Observe that quotes are used to refer to a female student (i.e., "F"). This is done because the string \code{F} (without quotes) is shorthand notation for the logical indicator \code{FALSE} in R. If no quotes are used, an error will be generated that a logical indicator was provided where a factor level was expected.
  
When multiple independent variables are considered, the \code{data.frame} can be constructed using the \code{expand.grid()} function. For example, \code{Grid.Norm.Table= expand.grid(Age.C=seq(from=-30, to=30, by=1), LE=c("Low", "Average", "High"))} specifies that the normative table should be stratified for both Age centered (with score range -30 to 30) and LE.}
\item{Test.Scores}{A vector that specifies the raw test scores that should be shown in the normative table.}
\item{Rounded}{Logical. Should the percentile ranks that are shown in the normative table be rounded to a whole number? Default \code{Rounded=TRUE}.}
\item{Digits}{The number of digits that need to be shown in the normative table for the predicted means and residual standard errors. Default \code{Digits=6}.}
}
\details{
For details, see Van der Elst (2023).
}

\value{
An object of class \code{Stage.2.NormTable} with components,
\item{Norm.Table}{The normative table.}
\item{Group.Specific.SD.Resid}{Logical. Where prediction-specific SDs of the residuals used?}  
\item{Empirical.Dist.Delta}{Logical. Was the CDF of the standardized residuals used to convert the raw test scores into percentile ranks?}
\item{N.Analysis}{The sample size of the analyzed dataset.}
\item{Test.Scores}{A vector of raw test scores for which percentile ranks were requested.}
\item{Assume.Homoscedasticity}{Is homoscedasticity assumed in the computation of the normative data?}
\item{Assume.Normality}{Is normality assumed in the computation of the normative data?}
\item{Stage.1.Model}{The \code{Stage.1.Model} model that was used to do the computations.}
\item{Grid.Norm.Table}{The specified \code{Grid.Norm.Table} in the function call.}
\item{Digits.Percentile}{The number of digits after the decimal point that were requested for the percentile ranks.}
}
\references{
Van der Elst, W. (2024). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} Springer Nature. 
}
\author{
Wim Van der Elst
}
\seealso{\code{\link{Stage.2.NormScore}}, \code{\link{Stage.2.AutoScore}}, \code{\link{Bootstrap.Stage.2.NormScore}}}
\examples{
# Replicate the normative table that was obtained in 
# Case study 1 of Chapter 3 in Van der Elst (2023)
# -----------------------------------------------------
library(NormData) # load the NormData package
data(GCSE)        # load the GCSE dataset

# Fit the Stage 1 model
Model.1.GCSE <- Stage.1(Dataset=GCSE, 
    Model=Science.Exam~Gender)

# Make a normative table for raw Science Exam scores = 10, 
# 11, ... 85, stratified by Gender
NormTable.GCSE <- Stage.2.NormTable(Stage.1.Model=Model.1.GCSE,
  Test.Scores=c(10:85), 
  Grid.Norm.Table=data.frame(Gender=c("F", "M")))

summary(NormTable.GCSE)


# Replicate the normative table that was obtained in 
# Case study 1 of Chapter 7 in Van der Elst (2023)
# ------------------------------------------------
library(NormData)   # load the NormData package
data(Substitution)  # load the Substitution dataset

# Make the new variable Age.C (= Age centered) that is 
# needed to fit the final Stage 1 model, 
# and add it to the Substitution dataset
Substitution$Age.C <- Substitution$Age - 50

# Fit the final Stage 1 model
Substitution.Model.9 <- Stage.1(Dataset=Substitution, 
    Alpha=0.005, Model=LDST~Age.C+LE, Order.Poly.Var=1) 

# Make a normative table for LDST scores = 10, 12, ... 56,
# stratified by Age and LE
NormTable.LDST <- Stage.2.NormTable(
  Stage.1.Model=Substitution.Model.9,
  Test.Scores=seq(from=10, to=56, by=2),
  Grid.Norm.Table=expand.grid(Age.C=seq(from=-30, to=30, by=1),
  LE=c("Low", "Average", "High")))
}
\keyword{Regression-based normative data}
\keyword{Stage 2}
\keyword{Normative table}
\keyword{Percentile ranks}