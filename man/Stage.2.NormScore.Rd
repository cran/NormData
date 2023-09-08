\name{Stage.2.NormScore}
\alias{Stage.2.NormScore}
\title{Convert a raw score to a percentile rank}
\description{
The function \code{Stage.2.NormScore()} can be used to convert the raw test score of a tested person \eqn{Y_0} into a percentile rank \eqn{\hat{\pi}_0} (taking into account specified values of the independent variables). 
}
\usage{
Stage.2.NormScore(Stage.1.Model, Assume.Homoscedasticity, 
Assume.Normality, Score, Rounded=TRUE)
}
\arguments{
  \item{Stage.1.Model}{A fitted object of class \code{Stage.1} that should be used to conduct the normative conversions.}
  \item{Assume.Homoscedasticity}{Logical. Should homoscedasticity be assumed in conducting the normative conversion? By default, homoscedasticity is assumed when the \eqn{p}-value of the Levene or Breusch-Pagan test for the fitted Stage 1 model is above the specified \eqn{\alpha}-level in the \code{Stage.1()} function call. When homoscedasticity is assumed, an overall residual standard error is used in the normative conversions. When homoscedasticity is not assumed, prediction-specific residual standard errors used. The default decision procedure can be overruled by means of the arguments argument \code{Assume.Homoscedasticity=TRUE} or \code{Assume.Homoscedasticity=FALSE}.}
  \item{Assume.Normality}{Logical. Should normality of the standardized errors be assumed in conducting the normative conversion? By default, normality is assumed when the \eqn{p}-value of the Shapiro-Wilk test for the fitted Stage 1 model is above the specified \eqn{\alpha}-level in the \code{Stage.1()} function call. When normality is assumed, the \eqn{Y_0} to  \eqn{\hat{\pi}_0} conversion is based on the CDF of the standard normal distribution. When normality is not assumed, this conversion is based on the CDF of the standardized residuals in the normative sample. The default decision procedure can be overruled by means of the arguments argument \code{Assume.Normality=TRUE} or \code{Assume.Normality=FALSE}.}
  \item{Score}{A \code{list} that contains the test score \eqn{Y_0} to be converted into a percentile rank and the values for the relevant independent variable(s). For example, the argument \code{Score=list(Science.Exam=30, Gender="F")} specifies that a female student obtained a raw Science Exam score \eqn{Y_0}. Observe that quotes are used to refer to a female student (i.e., "F"). This is done because the string \code{F} (without quotes) is shorthand notation for the logical indicator \code{FALSE} in R. If no quotes are used, an error will be generated that a logical indicator was provided where a factor level was expected. To avoid such issues, it is recommended to always use quotes to refer to the levels of a factor. In the \code{Score=...} argument, the test score should always be specified first followed by the independent variable. Notice that both the name of the independent variable and the coding scheme that is specified in the \code{Score=...} argument should correspond to the name of the independent variable and the original coding scheme that was used in the \code{Stage.1()} function call. For example, if the variable name \code{Gender} original coding scheme \code{F} and \code{M} was used in the \code{Stage.1()} function call, the same should be done in the \code{Stage.2.NormScore()} call. Thus \code{Score=list(Science.Exam=30, Gender="F")} should be used, and not e.g., \code{Score=list(Science.Exam=30, GenderM=0)}.}
\item{Rounded}{Logical. Should the percentile rank be rounded to a whole number? Default \code{Rounded=TRUE}.}
%\item{}{. Default \code{}.}
}
\details{
For details, see Van der Elst (2023). 
}
\value{
An object of class \code{Stage.2.NormScore} with components,
\item{Fitted.Model}{A fitted object of class \code{Stage.1()} that was used to convert the raw test score \eqn{Y_0} into a percentile rank \eqn{\hat{\pi}_0}.}
\item{Results}{A data frame that contains the observed test score, residuals, percentile rank, ...} 
\item{Assume.Homoscedasticity}{The homoscedasticity assumption that was made in the normative conversion.}
\item{Assume.Normality}{The normality assumption that was made in the normative conversion.}
\item{Score}{The test score and the value(s) of the independent variable(s) that were used in the computations.}
\item{Stage.1.Model}{The \code{Stage.1.Model} model used in the analysis.}
}
\references{
Van der Elst, W. (2023). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} New York: Springer. 
}
\author{
Wim Van der Elst
}
\seealso{\code{\link{Stage.2.NormTable}}, \code{\link{Stage.2.AutoScore}}, \code{\link{Bootstrap.Stage.2.NormScore}}
}
\examples{
# Replicate the normative conversion that was obtained in 
# Case study 1 of Chapter 3 in Van der Elst (2023)
# (science exam score = 30 obtained by a female)
# -------------------------------------------------------
library(NormData) # load the NormData package
data(GCSE)        # load the GCSE dataset

# Fit the Stage 1 model
Model.1.GCSE <- Stage.1(Dataset=GCSE, 
  Model=Science.Exam~Gender)

# Stage 2: Convert a science exam score = 30 obtained by a 
# female into a percentile rank (point estimate)
Normed_Score <- Stage.2.NormScore(Stage.1.Model=Model.1.GCSE,
  Score=list(Science.Exam=30, Gender="F"))

summary(Normed_Score)
plot(Normed_Score)


# Replicate the normative conversion that was obtained in 
# Case study 1 of Chapter 7 in Van der Elst (2023)
# (LDST score = 40 obtained by a 20-year-old 
# test participant with LE=Low)
# -------------------------------------------------------
library(NormData)   # load the NormData package
data(Substitution)  # load the Substitution dataset

# Make the new variable Age.C (= Age centered) that is 
# needed to fit the final Stage 1 model, 
# and add it to the Substitution dataset
Substitution$Age.C <- Substitution$Age - 50

# Fit the final Stage 1 model
Substitution.Model.9 <- Stage.1(Dataset=Substitution, 
  Alpha=0.005, Model=LDST~Age.C+LE, Order.Poly.Var=1) 
summary(Substitution.Model.9)

# Convert an LDST score = 40 obtained by a 
# 20-year-old test participant with LE=Low 
# into a percentile rank (point estimate)
Normed_Score <- Stage.2.NormScore(
  Stage.1.Model=Substitution.Model.9, 
  Score=list(LDST=40, Age.C=20-50, LE = "Low"))

summary(Normed_Score)
plot(Normed_Score)
}
\keyword{Regression-based normative data}
\keyword{Normative conversion}
\keyword{Stage 2}
\keyword{Percentile rankss}