\name{CheckFit}
\alias{CheckFit}
\title{Check the fit of the mean structure of a regression model}
\description{
The function \code{CheckFit()} allows for evaluating the fit of the mean structure of a regression model by comparing sample means and model-predicted means. If the model fits the data well, there should be a good agreement between the sample means and the predicted mean test scores in the relevant subgroups. When the model only contains (binary and/or non-binary) qualitative independent variables, the subgroups correspond to all possible combinations of the different levels of the qualitative variables. When there are quantitative independent variables in the model, these have to be discretized first.
}
\usage{
CheckFit(Stage.1.Model, Means, CI=.99, Digits=6)
}
\arguments{
  \item{Stage.1.Model}{The fitted \code{Stage.1} model.}
  \item{Means}{A formula in the form of \code{Test.Score~Independent.Var1+Independent.Var2+...}. The mean, SD, and N will be provided for all combinations of the independent variable values levels. Note that all indpendent variables should be factors (i.e., non -quantitative).}
\item{CI}{The required confidence limits. Default \code{CI=.99}, i.e. the 99 percent CI.}
\item{Digits}{The number of digits used when showing the results. Default \code{Digits=6}.}
}
\details{
For details, see Van der Elst (2023).
}
\value{
An object of class \code{CheckFit} with component,
\item{Results.Observed}{A table with the means, SDs, and N for the observed test score, for each combination of independent variable levels.} 
\item{Results.Predicted}{A table with the mean predicted test scores, for each combination of independent variable levels.} 
\item{Miss}{The number of missing observations in the dataset.}
\item{Dataset}{The dataset used in the analysis.}
\item{Model}{The specified model for the mean.}
\item{CI}{The requested CI around the mean.}
\item{N}{The sample size of the specified dataset.}
\item{Stage.1.Model}{The fitted \code{Stage.1.Model} used in the analysis.}
\item{Saturated}{Is the fitted \code{Stage.1.Model} a saturated model?}
}
\references{
Van der Elst, W. (2023). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} New York: Springer. 
}
\author{
Wim Van der Elst
}
\seealso{\code{\link{Stage.1}}, \code{\link{plot.CheckFit}}}
\examples{
# Replicate the fit plot that was obtained in 
# Case study 1 of Chapter 7 in Van der Elst (2023)
# ------------------------------------------------
library(NormData)   # load the NormData package
data(Substitution)  # load the Substitution dataset
head(Substitution)  # have a look at the first datalines in
# the Substitution dataset

# Final Stage 1 model 
Substitution$Age.C <- Substitution$Age - 50
  # Add Age_Group (that discretizes the quantitative variable Age 
  # into 6 groups with a span of 10 years in the dataset for use 
  # by the CheckFit() function later on)
Substitution$Age_Group <- cut(Substitution$Age, 
   breaks=seq(from=20, to=80, by=10)) 
Substitution.Model.9 <- Stage.1(Dataset=Substitution, 
   Alpha=0.005, Model=LDST~Age.C+LE, Order.Poly.Var=1) 

# Examine fit 
Fit.LDST <- CheckFit(Stage.1.Model=Substitution.Model.9, 
  Means=LDST~Age_Group+LE)
summary(Fit.LDST) 
plot(Fit.LDST)


# Replicate the fit plot that was obtained in 
# Case study 2 of Chapter 7 in Van der Elst (2023)
# ------------------------------------------------
library(NormData)   # load the NormData package
data(VLT)           # load the VLT dataset
head(VLT)           # have a look at the first datalines in 
                    # the VLT dataset

# Fit the final Stage 1 model
VLT$Age.C <- VLT$Age - 50
VLT$Age.C2 <- (VLT$Age - 50)**2
  # Add Age_Group (that discretizes the quantitative variable Age 
  # into 6 groups with a span of 10 years in the dataset for use 
  # by the CheckFit() function later on)
VLT$Age_Group <- cut(VLT$Age, breaks=seq(from=20, to=80, by=10)) 

VLT.Model.4 <- Stage.1(Dataset = VLT, Alpha = .005, 
  Model = Total.Recall ~ Age.C+Age.C2+Gender+LE+Age.C:Gender)

# Examine fit using fit plots for the Age Group by 
# LE by Gender subgroups
Fit.Means.Total.Recall <- CheckFit(Stage.1.Model=VLT.Model.4, 
  Means=Total.Recall~Age_Group+LE+Gender)

summary(Fit.Means.Total.Recall)
plot(Fit.Means.Total.Recall)
}
\keyword{Model fit}
