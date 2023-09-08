\name{plot Tukey.HSD}
\alias{plot Tukey.HSD}
\alias{plot.Tukey.HSD}
\title{Plot the results of Tukey's Honest Significance Difference test.}
\description{
This function plots the results of Tukey's Honest Significance Difference (HSD; Tukey, 1949) test that allows for making post hoc comparisons of the group means. Tukey's HSD can only be conducted when the mean structure of the Stage 1 model only contains qualitative independent variables (i.e., when the fitted regression model is essentially an ANOVA).}
\usage{
\method{plot}{Tukey.HSD}(x, \dots)
}
\arguments{
  \item{x}{A fitted object of class \code{Tukey.HSD}.}
  \item{\dots}{Extra graphical parameters to be passed to \code{plot()}.}
}
\value{No return value, called for side effects.}
\references{
Tukey, J. (1949). Comparing individual means in the Analysis of Variance. \emph{Biometrics, 5,} 99-114.

Van der Elst, W. (2023). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} New York: Springer. 
}
\author{
Wim Van der Elst
}
\seealso{\code{\link{Tukey.HSD}}}
\examples{
data(Personality)
Model.Openness <- Stage.1(Dataset = Personality, Model = Openness ~ LE) 
# conduct post hoc comparisons for the levels of education
Tukey.Openness <- Tukey.HSD(Model.Openness)
summary(Tukey.Openness)
plot(Tukey.Openness)

# conduct post hoc comparisons for the levels of education by education combinations
data(Substitution)
Model.Substitution <- Stage.1(Dataset = Substitution, Model = LDST ~ LE*Gender)
Tukey.Substitution <- Tukey.HSD(Model.Substitution)
summary(Tukey.Substitution)
plot(Tukey.Substitution)
}
\keyword{Tukey HSD}