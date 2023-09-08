\name{plot ExploreData}
\alias{plot ExploreData}
\alias{plot.ExploreData}
\title{Plot means and CIs for test scores.}
\description{
Plot the means (and CIs) for the test scores, stratified by the independent variable(s) of interest. The independent variables should be factors (i.e., binary or non-binary qualitiative variables).
}
\usage{
\method{plot}{ExploreData}(x, Width.CI.Lines=.125, Size.symbol = 1,
No.Overlap.X.Axis=TRUE, xlab, ylab="Test score", main, 
Color, pch, lty, Black.white=FALSE, Legend.text.size=1, 
Connect.Means = TRUE, Error.Bars = "CI", 
cex.axis=1, cex.main=1, cex.lab=1, \dots)
}
\arguments{
  \item{x}{A fitted object of class \code{ExploreData}.}
  \item{Width.CI.Lines}{The width of the horizontal lines that are used to depict the CI around the mean. Default \code{Width.CI.Lines=0.125}.}
  \item{Size.symbol}{The size of the symbol used to depict the mean test score. Default \code{Size.symbol=1}.}
  \item{No.Overlap.X.Axis}{Logical. When a plot is constructed using multiple IVs (specified in the \code{Model=} argument of the \code{ExploreData()} function), it is possible that the plot becomes unclear because the different means (and CIs) largely overlap. To avoid this, the levels of IV1 (plotted on the X-axis) can be slightly shifted for each level of IV2. For example, if IV1=Age group and IX2=Gender, the mean for the subcategory males in age range [20; 40] will be shown at value 0.9 on the X-axis (rather than 1) and the mean for the subcategory females in age range [20; 40] will be shown at value 1.1 (rather than 1), and similarly for all levels of IV1. In this way, the different means and CIs can be more clearly distinguished. Default \code{No.Overlap.X.Axis=TRUE}.}
  \item{xlab}{The label that should be added to the X-axis.}
  \item{ylab}{The label that should be added to the Y-axis. Default \code{ylab="Test score"}.}
  \item{main}{The title of the plot.}
  \item{Color}{The colors that should be used for the means. If not specified, the default colors are used.}
 \item{pch}{The symbols to be used for the means. If not specified, dots are used.}
 \item{lty}{The line types to be used for the means. If not specified, solid lines are used (i.e., \code{lty=1}.}
  \item{Black.white}{Logical. Should the plot be in black and white (rather than in color)? Default \code{Black.white=FALSE}.}
  \item{Legend.text.size}{The size of the text of the label for IV2. Default \code{Legend.text.size=1}.}
  \item{Connect.Means}{Logical. Should the symbols depicting the mean test scores be connected? Default \code{Connect.Means = TRUE}.}
\item{Error.Bars}{The type of error bars around the means that should be added in the plot: confidence intervals (\code{Error.Bars = "CI"}), standard errors (\code{Error.Bars = "SE"}), standard deviations (\code{Error.Bars = "SD"}) or no error bars  (\code{Error.Bars = "None"}). Default \code{Error.Bars = "CI"}.}
\item{cex.axis}{The magnification to be used for axis annotation.}
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
\seealso{\link{ExploreData}
}
\examples{
# Replicate the exploratory analyses that were conducted  
# in Case study 1 of Chapter 5 in Van der Elst (2023)
# ------------------------------------------------------
library(NormData) # load the NormData package

data(Personality) # load the Personality dataset
Explore_Openness <- ExploreData(Dataset=Personality, 
  Model=Openness~LE)
summary(Explore_Openness)
plot(Explore_Openness, 
  main="Mean Openness scale scores and 99pc CIs")


# Replicate the exploratory analyses that were conducted  
# in Case study 1 of Chapter 7 in Van der Elst (2023)
# ------------------------------------------------------
library(NormData)   # load the NormData package
data(Substitution)  # load the Substitution dataset
head(Substitution)  # have a look at the first datalines in
                    # the Substitution dataset

# First make a new variable Age_Group, that discretizes the
# quantitative variable Age into 6 groups with a span of 10 years
Substitution$Age_Group <- cut(Substitution$Age, 
   breaks=seq(from=20, to=80, by=10)) 

# Compute descriptives of the LDST score for different Age Group
# by LE combinations
Explore.LDST.Age.LE <- ExploreData(Dataset=Substitution,
   Model=LDST~Age_Group+LE) 
summary(Explore.LDST.Age.LE)

# Make a plot of the results. 
plot(Explore.LDST.Age.LE, 
   main="Mean (99pc CI) LDST scores by Age group and LE")

# Compute descriptives of the LDST score for different
# Age Group by Gender combinations
Explore.LDST.Age.Gender <- ExploreData(Dataset=Substitution, 
  Model=LDST~Age_Group+Gender)

# Plot the results
plot(Explore.LDST.Age.Gender, 
  main="Mean (99pc CI) LDST scores by Age group and Gender")

# Compute descriptives of the LDST score for different
# LE by Gender combinations
Explore.LDST.LE.Gender <-
  ExploreData(Dataset=Substitution, Model=LDST~LE+Gender)

# Plot the results
plot(Explore.LDST.LE.Gender,
  main="Mean (99pc CI) LDST scores by LE and Gender")

# Compute summary statistics of the LDST score in the
# Age Group by LE by Gender combinations
Explore.LDST <- ExploreData(Dataset=Substitution,
   Model=LDST~Age_Group+LE+Gender)

# Plot the results
plot(Explore.LDST)
}
\keyword{Plot ExploreData}