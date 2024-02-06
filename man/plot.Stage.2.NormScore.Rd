\name{plot Stage.2.NormScore}
\alias{plot Stage.2.NormScore}
\alias{plot.Stage.2.NormScore}
\title{Plot the results for a \code{Stage.2.NormScore} object.}
\description{
The function \code{Stage.2.NormScore()} is used to convert the raw test score of a tested person \eqn{Y_0} into a percentile rank \eqn{\hat{\pi}_0} (taking into account specified values of the independent variables). This function plots the results graphically. In particular, the density of the standard normal distribution is shown (when the normality assumption is valid for the fitted Stage 1 model), or the density of the standardized residuals in the normative sample (when the noormality assumption is not shown). The AUC between \eqn{- \infty} and the tested person's standarized test score \eqn{\widehat{\delta}_i} is shaded in grey, which visualizes the percentile rank that corresponds to the raw test score.
}
\usage{
\method{plot}{Stage.2.NormScore}(x, Main=" ", Both.CDFs=FALSE, xlim, 
cex.axis=1, cex.main=1, cex.lab=1, \dots)
}
\arguments{
\item{x}{A fitted object of class \code{Stage.2.NormScore}.}
\item{Main}{The title of the plot. Default \code{Main=" "}.}
\item{Both.CDFs}{Should both the densities of the standard normal distribution and of the standardized residuals \eqn{\widehat{\delta}_i} in the normative sample be shown in one plot? Default \code{Both.CDFs=FALSE}.}
\item{xlim}{The limits for the X-axis. Default \code{xlim=c(-4,4)}.}
\item{cex.axis}{The magnification to be used for axis annotation.}
\item{cex.main}{The magnification to be used for the main label.}
\item{cex.lab}{The magnification to be used for X and Y labels.}
\item{\dots}{Extra graphical parameters to be passed to \code{plot()}.}
}
\value{No return value, called for side effects.}
\references{
Van der Elst, W. (2024). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} Springer Nature. 
}
\author{
Wim Van der Elst
}
\seealso{\link{Stage.2.NormScore}
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
\keyword{Plot NormScore}
\keyword{Regression-based normative data}