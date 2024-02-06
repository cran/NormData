\name{PlotFittedPoly}
\alias{PlotFittedPoly}
\title{Explore data}
\description{
The function PlotFittedPoly fits polynomials of a specified order to the data.
}
\usage{
PlotFittedPoly(Dataset, Test.Score, IV, Center.Value.IV=0,
Order.Polynomial=3, Confidence.Band.Poly=FALSE, Alpha=.01,
EMF = TRUE, Confidence.Band.EMF=TRUE,
xlab, ylab, Color = "red", Black.white=FALSE,
Legend.Location="topright", Legend.text.size=1,
Add.Jitter=0, Seed=123, cex.axis=1, cex.main=1, 
cex.lab=1, Loess.Span=0.75, ...)
}
\arguments{
  \item{Dataset}{The name of the dataset.}
\item{Test.Score}{The name of the test score.}
\item{IV}{The name of the independent variable.}
\item{Center.Value.IV}{The constant that is subtracted from the independent variable. \cr Default \code{Center.Value.IV=0}.}
\item{Order.Polynomial}{The order of the polynomials to be fitted. By default, \code{Order.Polynomial=3} and thus a cubic polynomial is fitted. If no polynomial has to be plotted, the argument  \code{Order.Polynomial="None"} can be used.}
\item{Confidence.Band.Poly}{Logical. Should a confidence band around the prediction function of the polynomial model be added to the plot? Default \code{Confidence.Band.Poly=FALSE}.}
\item{Alpha}{The Alpha-level of the confidence band(s) for the polynomial and/or loess models. Default \code{Alpha=0.01} and thus a \eqn{99\%} confidence band is fitted.}
\item{EMF}{Logical. Should the EMF be added to the plot? Default \code{EMF=TRUE}.}
\item{Confidence.Band.EMF}{Logical. Should a confidence band around the prediction function of the loess model be added to the plot? Default \code{Confidence.Band.EMF=TRUE}.}
  \item{xlab}{The label that should be added to the X-axis. Default \code{xlab="IV"}}
  \item{ylab}{The label that should be added to the Y-axis. Default \code{ylab="Test score"}.}
  \item{Color}{The color to be used for the fitted EMF. Default \code{Color = "red"}.}
  \item{Black.white}{Logical. Should the plot be in black and white (rather than in color)? Default \code{Black.white=FALSE}.}
  \item{Legend.Location}{The location of the legend. Default \code{Legend.Location="topright"}. If no legend is needed, the argument \code{Legend.Location="None"} can be used.}
  \item{Legend.text.size}{The size of the text of the label for IV2. Default \code{Legend.text.size=1}.}
  \item{Add.Jitter}{The amount of jitter (random noise) that should be added to the test score. Adding a bit of jitter is useful to show the data more clearly, i.e., to avoid overlapping data points. The specified value \code{Add.Jitter=} in the function call determines the amount of jitter (range of values) that is added. For example, when \code{Add.Jitter=0.1}, a random value between -0.1 and 0.1 (sampled from a uniform) is added to the test  scores. Default \code{Add.Jitter=0}, i.e., no jitter added to the predicted values in the homoscedasticity plot.}
\item{Seed}{The seed that is used when adding jitter. Default \code{Seed=123}.}
\item{cex.axis}{The magnification to be used for axis annotation.}
\item{cex.main}{The magnification to be used for the main label.}
\item{cex.lab}{The magnification to be used for X and Y labels.}
\item{Loess.Span}{The parameter \eqn{\alpha} that determines the degree of smoothing of the Empirical Variance Function. Default \code{Loess.Span=0.75}.}
  \item{\dots}{Extra graphical parameters to be passed to \code{plot()}.}
}
\value{No return value, called for side effects.}
\details{
For details, see Van der Elst (2023).
}
\references{
Van der Elst, W. (2024). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} Springer Nature. 
}
\author{
Wim Van der Elst
}
\examples{
data(Substitution)

# plot of linear, quadratic and cubic polynomials relating age
# to the LDST test score 
PlotFittedPoly(Dataset = Substitution, Test.Score = LDST, IV = Age, 
Order.Polynomial = 1, Center.Value.IV = 50)

PlotFittedPoly(Dataset = Substitution, Test.Score = LDST, IV = Age, 
Order.Polynomial = 2, Center.Value.IV = 50)

PlotFittedPoly(Dataset = Substitution, Test.Score = LDST, IV = Age, 
Order.Polynomial = 3, Center.Value.IV = 50)
}
\keyword{Explore data}