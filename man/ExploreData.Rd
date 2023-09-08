\name{ExploreData}
\alias{ExploreData}
\title{Explore data}
\description{
This function provides summary statistics of a test score (i.e., the mean, SD, N, standard error of the mean, and CI of the mean), stratified by the independent variable(s) of interest. The independent variables should be factors (i.e., binary or non-binary qualitiative variables).
}
\usage{
ExploreData(Dataset, Model, CI=.99, Digits=6)
}
\arguments{
  \item{Dataset}{A dataset.}
  \item{Model}{A formula in the form of \code{Test.Score~IV.1+IV.2+...}. Summary statistics (i.e., the mean, SD, N, standard error of the mean, and CI of the mean) are provided for all combinations of the levels of the IVs (independent variables). Note that all IVs should be factors (i.e., binary or non-binary qualitative variables).}
\item{CI}{The CI for the mean. Default \code{CI=.99}, i.e. the 99 CI.}
\item{Digits}{The number of digits used when showing the results. Default \code{Digits=6}.}
}
\details{
For details, see Van der Elst (2023).
}

\value{
An object of class \code{ExploreData} with component,
\item{Results}{A table with the summary statistics.} 
\item{Miss}{The number of missing observations in the dataset.}
\item{Dataset}{The dataset used in the analysis.}
\item{Model}{The specified model.}
\item{CI}{The requested CI around the mean.}
\item{N}{The sample size of the specified dataset.}
}
\references{
Van der Elst, W. (2023). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} New York: Springer. 
}
\author{
Wim Van der Elst
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
\keyword{Explore data}
\keyword{Summary statistics}
