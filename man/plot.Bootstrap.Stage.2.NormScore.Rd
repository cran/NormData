\name{plot Bootstrap.Stage.2.NormScore}
\alias{plot Bootstrap.Stage.2.NormScore}
\alias{plot.Bootstrap.Stage.2.NormScore}
\title{Plot the bootstrap distribution and the percentile bootstrap CI}
\description{
This function plots the bootstrap distribution and the percentile bootstrap CI for a test score based on a \code{Bootstrap.Stage.2.NormScore} object. A non-parametric bootstrap is used to compute a confidence interval (CI) around the estimated percentile rank (for details, see Chapter 8 in Van der Elst, 2023). 
}
\usage{
\method{plot}{Bootstrap.Stage.2.NormScore}(x, 
cex.axis=1, cex.main=1, cex.lab=1, \dots)
}
\arguments{
\item{x}{A fitted object of class \code{Bootstrap.Stage.2.NormScore}.}
\item{cex.axis}{The magnification to be used for axis annotation.}
\item{cex.main}{The magnification to be used for the main label.}
\item{cex.lab}{The magnification to be used for X and Y labels.}
\item{\dots}{Other arguments to be passed to the \code{plot()} function.}
}
\value{No return value, called for side effects.}
\references{
Van der Elst, W. (2024). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} Springer Nature. 
}
\author{
Wim Van der Elst
}
\seealso{\link{Bootstrap.Stage.2.NormScore}
}
\examples{
\donttest{  # Time-intensive part
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
\keyword{Plot NormScore}
\keyword{Regression-based normative data}