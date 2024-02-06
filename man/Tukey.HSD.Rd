\name{Tukey.HSD}
\alias{Tukey.HSD}
\title{
Conducts Tukey's Honest Significance Difference test
}
\description{
This function conducts Tukey's Honest Significance Difference (HSD; Tukey, 1949) test that allows for making post hoc comparisons of the group means. Tukey's HSD can only be conducted when the mean structure of the Stage 1 model only contains qualitative independent variables (i.e., when the fitted regression model is essentially an ANOVA).
}
\usage{
Tukey.HSD(Stage.1.Model, ...)
}
\arguments{
  \item{Stage.1.Model}{A fitted stage one model that only contains qualitative variables.}
  \item{...}{Arguments to be passed to the plot function of the Tukey HSD procedure.}
}
\value{No return value, called for side effects.}
\references{
Tukey, J. (1949). Comparing individual means in the Analysis of Variance. \emph{Biometrics, 5,} 99-114.

Van der Elst, W. (2024). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} Springer Nature. 
}
\author{
Wim Van der Elst
}
\seealso{\code{\link{plot.Tukey.HSD}}}
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