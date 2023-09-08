\name{plot CheckFit}
\alias{plot CheckFit}
\alias{plot.CheckFit}
\title{Evaluate the fit of the mean structure of a fitted Stage 1 model.}
\description{
The function \code{CheckFit()} allows for evaluating the fit of the mean structure of a regression model by comparing sample means and model-predicted means. This function plots the sample means (with CIs) and the means of the model-predicted values. If the model fits the data well, there should be a good agreement between the sample means and the predicted mean test scores in the relevant subgroups. When the model only contains (binary and/or non-binary) qualitative independent variables, the subgroups correspond to all possible combinations of the different levels of the qualitative variables. When there are quantitative independent variables in the model, these have to be discretized first.
}
\usage{
\method{plot}{CheckFit}(x, Color, pch, lty, 
Width.CI.Lines=.125, Size.symbol = 1, 
No.Overlap.X.Axis=TRUE, xlab, ylab="Test score", 
main = " ", Legend.text.size=1, Connect.Means, 
cex.axis=1, cex.main=1.5, cex.lab=1.5, \dots)
}
\arguments{
  \item{x}{A fitted object of class \code{CheckFit}.}
  \item{Color}{The colors to be used for the means. If not specified, the default colors are used.}
 \item{pch}{The symbols to be used for the means. If not specified, dots are used.}
 \item{lty}{The line types to be used for the means. If not specified, solid lines are used.}
  \item{Width.CI.Lines}{The width of the horizontal lines that are used to depict the CI around the mean. Default \code{Width.CI.Lines=0.125}.}
  \item{Size.symbol}{The size of the symbol used to depict the mean test score. Default \code{Size.symbol=1}.}
  \item{No.Overlap.X.Axis}{Logical. When a plot is constructed using two IVs (i.e., 2 or more lines of the mean and CIs in the plot), it is possible that the plot is unclear because the different means and CIs can no longer be distinguished. To avoid this, the levels of IV1 (plotted on the X-axis) can be assigned slightly different values for each level of IV2. For example, the mean for the subcategory males in age range [20; 40] will be shown at value X=0.9 (rather than 1) and the mean for the subcategory females in age range [20; 40] will be shown at value X=1.1 (rather than 1). In this way, the different means and CIs can be more clearly distinguished. Default \code{No.Overlap.X.Axis=TRUE}.}
  \item{xlab}{The label that should be added to the X-axis.}
  \item{ylab}{The label that should be added to the Y-axis. Default \code{ylab="Test score"}.}
  \item{main}{The title of the plot. Default \code{main=" "}.}
  \item{Legend.text.size}{The size of the text of the label for IV2. Default \code{Legend.text.size=1}.}
  \item{Connect.Means}{Logical. Should the symbols depicting the mean test scores be connected? If not specified, \code{Connect.Means = TRUE} is used if the model contains numeric independent variables and \code{Connect.Means = FALSE} otherwise.}
  \item{cex.axis}{The size of the labels on the X- and Y-axis. Default \code{cex.axis=1}.}  
  \item{cex.main}{The magnification to be used for the main label.}
\item{cex.lab}{The magnification to be used for X and Y labels.}
\item{\dots}{Extra graphical parameters to be passed to \code{plot()}.}
}
\value{No return value, called for side effects.}
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