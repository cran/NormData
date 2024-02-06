\name{Bootstrap.Stage.2.NormScore}
\alias{Bootstrap.Stage.2.NormScore}
\title{Bootstraps a confidence interval for a percentile rank}
\description{
The function \code{Stage.2.NormScore()} can be used to convert a raw test score of a tested person \eqn{Y_0} into a percentile rank \eqn{\hat{\pi}_0} (taking into account specified values of the independent variables). The function \code{Bootstrap.Stage.2.NormScore()} can be used to obtain a confidence interval (CI) around the point estimate of the percentile rank \eqn{\hat{\pi}_0}. A non-parametric bootstrap is used to compute a confidence interval (CI) around the estimated percentile rank (for details, see Chapter 8 in Van der Elst, 2023). 
}
\usage{
Bootstrap.Stage.2.NormScore(Stage.2.NormScore, 
CI=.99, Number.Bootstraps=2000, Seed=123, 
Rounded=FALSE, Show.Fitted.Boot=FALSE, verbose=TRUE)
}
\arguments{
\item{Stage.2.NormScore}{A fitted object of class \code{Stage.2.NormScore}.}
\item{CI}{The desired CI around the percentile rank for the raw test score at hand. Default \code{CI=.99}.}
 \item{Number.Bootstraps}{The number of bootstrap samples that are taken. Default \code{Number.Bootstraps=2000}.}
\item{Seed}{The seed to be used in the bootstrap (for repoducibility). Default \code{Seed = 123}.}
\item{Rounded}{Logical. Should the percentile rank be rounded to a whole number? Default \code{Rounded=FALSE}.}
\item{Show.Fitted.Boot}{Logical. Should the fitted Stage 1 models for the bootstrap samples be printed? Default \code{Show.Fitted.Boot=FALSE}.}
\item{verbose}{A logical value indicating whether verbose output should be generated.}
}
\details{
For details, see Chapter 8 in Van der Elst (2023).
}
\value{
An object of class \code{Stage.2.NormScore} with components,
\item{CI.Percentile}{The bootstrapped CI around the estimated percentile rank.}
\item{CI}{The CI used.}
\item{All.Percentiles}{All bootstrapped percentile ranks for the raw test score at hand.}
\item{Assume.Homoscedasticity}{Logical. Was homoscedasticity assumed in the normative conversion? For details, see \code{\link{Stage.2.NormScore}}.}
\item{Assume.Normality}{Logical. Was normality assumed in the normative conversion? For details, see \code{\link{Stage.2.NormScore}}.}
\item{Stage.2.NormScore}{The fitted \code{Stage.2.NormScore} object used in the function call.}
\item{Percentile.Point.Estimate}{The point estimate for the percentile rank (based on the original dataset).}
}
\references{
Van der Elst, W. (2024). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} Springer Nature. 
}
\author{
Wim Van der Elst
}
\seealso{
\code{\link{Stage.2.NormScore}}
}
\examples{
\donttest{ # Time-intensive part
# Replicate the bootstrap results that were obtained in 
# Case study 1 of Chapter 8 in Van der Elst (2023)
# -----------------------------------------------------
library(NormData) # load the NormData package
data(GCSE)        # load the GCSE dataset

# Fit the Stage 1 model
Model.1.GCSE <- Stage.1(Dataset=GCSE, 
  Model=Science.Exam~Gender)

# Stage 2: Convert a science exam score = 30 obtained by a 
# female into a percentile rank (point estimate)
Normed_Score <- Stage.2.NormScore(Stage.1.Model=Model.1.GCSE,
  Score=list(Science.Exam=30, Gender="F"), Rounded = FALSE)
summary(Normed_Score)

# Derive the 99pc CI around the point estimate 
# using a bootstrap procedure
Bootstrap_Normed_Score <- Bootstrap.Stage.2.NormScore(
  Stage.2.NormScore=Normed_Score)

summary(Bootstrap_Normed_Score)

plot(Bootstrap_Normed_Score)


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

# Convert an LDST score = 40 obtained by a 
# 20-year-old test participant with LE=Low 
# into a percentile rank (point estimate)
Normed_Score <- Stage.2.NormScore(
   Stage.1.Model=Substitution.Model.9, 
   Score=list(LDST=40, Age.C=20-50, LE = "Low"), 
   Rounded = FALSE)

# Derive the 99pc CI around the point estimate 
# using a bootstrap
Bootstrap_Normed_Score <- Bootstrap.Stage.2.NormScore(
   Stage.2.NormScore = Normed_Score)
summary(Bootstrap_Normed_Score)
plot(Bootstrap_Normed_Score)
}
}
\keyword{Regression-based normative data}
\keyword{Normative table}
\keyword{Percentile rank}
\keyword{Bootstrap}
\keyword{Confidence interval}