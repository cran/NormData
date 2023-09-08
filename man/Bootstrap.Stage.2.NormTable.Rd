\name{Bootstrap.Stage.2.NormTable}
\alias{Bootstrap.Stage.2.NormTable}
\title{Bootstraps confidence intervals for a normative table}
\description{
The function \code{Stage.2.NormTable()} is used to derive a normative table that shows the percentile ranks \eqn{\hat{\pi}_0} that correspond to a wide range of raw test scores \eqn{Y_0}  (stratified by the relevant independent variables). The function \code{Bootstrap.Stage.2.NormTable()} can be used to obtain confidence intervals (CIs) around the point estimates of the percentile ranks \eqn{\hat{\pi}_0} in the normative table. A non-parametric bootstrap is used to compute these CIs (for details, see Chapter 8 in Van der Elst, 2023). 
}
\usage{
Bootstrap.Stage.2.NormTable(Stage.2.NormTable,
CI=.99, Number.Bootstraps=2000, Seed=123, 
Rounded=FALSE, Show.Fitted.Boot=FALSE, verbose=TRUE)
}
\arguments{
\item{Stage.2.NormTable}{A fitted object of class \code{Stage.2.NormTable}.}
\item{CI}{The desired CI around the percentile ranks. Default \code{CI=.99}.}
 \item{Number.Bootstraps}{The number of bootstrap samples that are taken. Default \code{Number.Bootstraps=2000}.}
\item{Seed}{The seed to be used in the bootstrap (for repoducibility). Default \code{Seed = 123}.}
\item{Rounded}{Logical. Should the percentile ranks that are shown in the normative table be rounded to a whole number? Default \code{Rounded=FALSE}.}
\item{Show.Fitted.Boot}{Logical. Should the fitted Stage 1 models for the bootstrap samples be printed? Default \code{Show.Fitted.Boot=FALSE}.}
\item{verbose}{A logical value indicating whether verbose output should be generated.}
}
\details{
For details, see Chapter 8 in Van der Elst (2023).
}

\value{
An object of class \code{Stage.2.NormTable} with components,
\item{NormTable.With.CI}{The normative table with the bootstrapped CI.}
\item{CI}{The CI used.}
\item{Assume.Homoscedasticity}{Logical. Was homoscedasticity assumed in the normative conversion? For details, see \code{\link{Stage.2.NormTable}}.}
\item{Assume.Normality}{Logical. Was normality assumed in the  in the normative conversion? For details, see \code{\link{Stage.2.NormTable}}.}
\item{NormTable.With.CI.Min}{A table with the lower bounds of the CIs.}
\item{NormTable.With.CI.Max}{A table with the upper bounds of the CIs.}
}
\references{
Van der Elst, W. (2023). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} New York: Springer. 
}
\author{
Wim Van der Elst
}
\seealso{
\code{\link{Stage.2.NormTable}}
}
\examples{
\dontrun{ # Time-intensive part
# Replicate the bootstrap results that were obtained in 
# Case study 1 of Chapter 8 in Van der Elst (2023)
# -----------------------------------------------------
library(NormData) # load the NormData package
data(GCSE)        # load the GCSE dataset

# Fit the Stage 1 model
Model.1.GCSE <- Stage.1(Dataset=GCSE, 
  Model=Science.Exam~Gender)

# Normative table with CIs
NormTable.GCSE <- Stage.2.NormTable(
  Stage.1.Model=Model.1.GCSE, 
  Test.Scores=seq(from=10, to=85, by=5),
  Grid.Norm.Table=data.frame(Gender=c("F", "M")), 
  Rounded = FALSE)
summary(NormTable.GCSE)

# Bootstrap the CIs
Bootstrap_NormTable.GCSE <- Bootstrap.Stage.2.NormTable(
  Stage.2.NormTable = NormTable.GCSE)
summary(Bootstrap_NormTable.GCSE)


# Replicate the bootstrap results that were obtained in 
# Case study 2 of Chapter 8 in Van der Elst (2023)
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

summary(Substitution.Model.9)

# Make the normative table
NormTable.LDST <- Stage.2.NormTable(
Stage.1.Model=Substitution.Model.9, 
  Test.Scores=seq(from=25, to=40, by=5),
  Grid.Norm.Table=expand.grid(
  Age.C=seq(from=-30, to=30, by = 1), 
  LE=c("Low", "Average", "High")), Rounded = FALSE)

# Bootstrap the CIs
Bootstrap_NormTable.LDST <- Bootstrap.Stage.2.NormTable(
  Stage.2.NormTable = NormTable.LDST)

summary(Bootstrap_NormTable.LDST)
}
}
\keyword{Regression-based normative data}
\keyword{Normative table}
\keyword{Percentile ranks}
\keyword{Bootstrap}
\keyword{Confidence interval}