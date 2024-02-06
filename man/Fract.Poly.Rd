\name{Fract.Poly}
\alias{Fract.Poly}
\title{Fit fractional polynomials}
\description{
Fit a fractional polynomial model with \eqn{m} terms of the form \eqn{X^{p}}, where the exponents \eqn{p} are selected from a small predefined set \eqn{S} of both integer and non-integer values. This function can be useful to model the mean or variance prediction function in a more flexible way than by using linear, quadratic or cubic polynomials. 
}
\usage{
Fract.Poly(IV, Outcome, 
S=c(-3, -2.5, -2.0, -1.5, -1, -0.5, 0.5, 1, 1.5, 2, 2.5, 3), 
Max.M=3)
}
\arguments{
  \item{IV}{The Independent Variable to be considered in the model.}
  \item{Outcome}{The outcome to be considered in the model.}
  \item{S}{The set \eqn{S} from which each power \eqn{p^{m}} is selected. Default \code{S={-3, -2.5, -2.0, -1.5, -1, -0.5, 0.5, 1, 1.5, 2, 2.5, 3}}.}
  \item{Max.M}{The maximum order \eqn{M} to be considered for the fractional polynomial. This value can be \eqn{5} at most. When \eqn{M=5}, then fractional polynomials of order \eqn{1} to \eqn{5} are considered. Default \code{Max.M=3}.}
}
\value{
\item{All.Results}{The results (powers and AIC values) of the fractional polynomials.}
\item{Lowest.AIC}{Table with the fractional polynomial model that has the lowest AIC.}
\item{Best.Model}{The best fitted model (\code{lm} object).}
\item{IV}{The IV tha was considered in the model.}
\item{Outcome}{The outcome that was considered in the model.}
}
\references{
Van der Elst, W. (2024). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} Springer Nature. 
}
\author{
Wim Van der Elst
}
\examples{
data(VLT)
# Fit fractional polynomials of orders 1 to 2
FP <- Fract.Poly(IV = VLT$Age, Outcome = VLT$Total.Recall, 
  Max.M=2)
FP$Lowest.AIC
FP$Best.Model  
# Model with lowest AIC: 127.689 + (-190.731 * (Age**(-0.5))) +
#  (-7.586 * (Age**(0.5)))

# Make plot
plot(x=VLT$Age, y=VLT$Total.Recall, col="grey")
  # add best fitted fractional polynomial
Age.Vals.Plot <- 20:80
Pred.Vals <- 127.689 + (-190.731 * (Age.Vals.Plot**(-0.5))) +
   (-7.586 * (Age.Vals.Plot**(0.5)))
lines(x=Age.Vals.Plot, y=Pred.Vals, lwd=2, col="red", lty=2)
legend("topright", lwd=2, col="red", lty=2, 
  legend="Mean Prediction Function, Fractional Polynomial")
}
\keyword{Fractional polynomials}
