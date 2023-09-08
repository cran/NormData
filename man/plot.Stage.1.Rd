\name{plot Stage.1}
\alias{plot Stage.1}
\alias{plot.Stage.1}
\title{Check the model assumptions for a fitted Stage 1 model graphically.}
\description{
This function provides several plots that are useful to evaluate model assumptions. When the \code{plot()} function is applied to a fitted \code{Stage.1} object, three panels are generated. These panels show plots that can be used (i) to evaluate the homoscedasticity assumption, (ii) to evaluate the normality assumption, and (iii) to evaluate the presence of outliers. 
}
\usage{
\method{plot}{Stage.1}(x, Homoscedasticity=TRUE, Normality=TRUE, 
Outliers=TRUE, Assume.Homoscedasticity, Add.Jitter=0, Seed=123, 
Confidence.QQ.Normality=.99, Plots.Together=TRUE, 
Y.Lim.ResVarFunction, Group.Spec.Densities.Delta=FALSE, Main.Homosced.1,
Main.Homosced.2, Main.Norm.1, Main.Norm.2, Main.Norm.3, Main.Outliers, 
cex.axis.homo=1, cex.main.homo=1, cex.lab.homo=1,  
cex.axis.norm=1.6, cex.main.norm=1.5, cex.lab.norm=1.5,  
cex.axis.outl=1, cex.main.outl=1, cex.lab.outl=1,  
Color="red", Loess.Span=0.75, verbose=TRUE, ...)}
\arguments{
  \item{x}{A fitted object of class \code{Stage.1}.}
  \item{Homoscedasticity}{Logical. Should plots to evaluate homoscedasticity be shown? \cr  Default \code{Homoscedasticity=TRUE}.}
  \item{Normality}{Logical. Should plots to evaluate the normality assumption be shown? The normality plots are based on the standardized residuals in the normative dataset, which are computed as explained in the \code{Assume.Homoscedasticity=} argument documentation below. Default \code{Normality=TRUE}.}
  \item{Outliers}{Logical. Should plots to evaluate outliers be shown? The outlier plot is based on the standardized residuals in the normative dataset, which are computed as explained in the \code{Assume.Homoscedasticity=} argument documentation below. Default \code{Outliers=TRUE}.}
\item{Assume.Homoscedasticity}{
By default, the standardized residuals \eqn{\widehat{\delta}_i}  that are shown in the normality and outlier plots are computed based on the overall residual standard error when the homoscedasticity assumption is valid (i.e., as \eqn{\widehat{\delta}_i = \frac{\widehat{\varepsilon}_i}{\widehat{\sigma}^2_{\varepsilon}}}, with  \eqn{\widehat{\sigma}^2_{\varepsilon}} corresponding to the overall residual standard error), or based on prediction-specific residual standard errors when the homoscedasticity assumption is invalid (i.e., as \eqn{\widehat{\delta}_i = \frac{\widehat{\varepsilon}_i}{\widehat{\sigma}^2_{\varepsilon_i}}}, with  \eqn{\widehat{\sigma}^2_{\varepsilon_i}} corresponding to e.g., a cubic polynomial variance prediction function \eqn{\widehat{\sigma}^2_{\varepsilon_i} = \widehat{\gamma}_0 + \widehat{\gamma}_1 \: \widehat{Y} + \widehat{\gamma}_2 \: \widehat{Y}^2 + {\gamma}_3 \: \widehat{Y}^3} when the mean structure of the model contains quantitiative independent variables). The default behaviour of the \code{plot()} function can be overruled using the \code{Assume.Homoscedasticity} argument. For example, when adding the argument \code{Assume.Homoscedasticity=TRUE} to the function call, the standardized residuals that are plotted will be computed based on the overall residual standard error (irrespective of the result of the Levene or Breusch-Pagan test).}
\item{Add.Jitter}{The amount of jitter (random noise) that should be added to the X-axis of the  homoscedasticity plots (which show the model-predicted mean values). Adding a bit of jitter is useful to show the data more clearly (especially when there are only a few unique predicted values, e.g., when a binary or non-binary qualitative independent variable is considered in the mean structure of the model), i.e., to avoid overlapping data points. The specified value \code{Add.Jitter=} in the function call determines the amount of jitter (range of values) that is added. For example, when \code{Add.Jitter=0.1}, a random value between -0.1 and 0.1 (sampled from a uniform) is added to the predicted values in the homoscedasticity plots (shown on the X-axis). Default \code{Add.Jitter=0}, i.e., no jitter added to the predicted values in the homoscedasticity plots.}
\item{Seed}{The seed that is used when adding jitter. Default \code{Seed=123}.}
  \item{Confidence.QQ.Normality}{Specifies the desired confidence-level for the confidence band arond the line of perfect agreement/normality in the QQ-plot that is used to evaluate normality. Default \code{Confidence.QQ.Normality=0.95}. Use \code{Confidence.QQ.Normality= FALSE} if no confidence band is needed.}
\item{Plots.Together}{The different homoscedasticity and normality plots are grouped together in a panel by default. For example, the three normality plots are shown together in one panel. If it is preferred to have the different plots in separate panels (rather than grouped to- gether), the argument \code{Plots.Together=FALSE} can be used. Default \code{Plots.Together=TRUE}.}
\item{Y.Lim.ResVarFunction}{The min, max limits of the Y-axis that should be used for the variance function plot. By default, the limit of the Y-axis is set between \eqn{0} and the maximum value of estimated variances multiplied by \eqn{2}. This can be changed using the \code{Y.Lim.ResVarFunction} argument. For example, adding the argument \code{Y.Lim.ResVarFunction=c(0, 500)} sets the range of the Y-axis of the variance function plot from 0 to 500.}
\item{Group.Spec.Densities.Delta}{Logical. Should a plot with the group-specific densities of the standardized residuals be shown? Default \code{Group.Spec.Densities.Delta=FALSE}.}
\item{Main.Homosced.1}{The title of the first panel of the homoscedasticity plot (i.e., the scatterplot of the residuals against the predicted scores).}
\item{Main.Homosced.2}{The title of second panel of the homoscedasticity plot (i.e., the variance function plot).}
\item{Main.Norm.1}{The title of the first panel of the normality plot (i.e., the histogram of the standardized residuals).}
\item{Main.Norm.2}{The title of the second panel of the normality plot (i.e., the density of the standardized residuals and standard normal distribution).}
\item{Main.Norm.3}{The title of the third panel of the normality plot (i.e., the QQ-plot).}
\item{Main.Outliers}{The title of the outlier plot.}
\item{cex.axis.homo}{The magnification to be used for axis annotation of the homoscedasticity plots.}
\item{cex.main.homo}{The magnification to be used for the main label of the homoscedasticity plots.}
\item{cex.lab.homo}{The magnification to be used for the X- and Y-axis labels of the homoscedasticity plots.}
\item{cex.axis.norm}{The magnification to be used for axis annotation of the normality plots.}
\item{cex.main.norm}{The magnification to be used for the main label of the normality plots.}
\item{cex.lab.norm}{The magnification to be used for X and Y labels of the normality plots.}
\item{cex.axis.outl}{The magnification to be used for axis annotation of the outlier plot.}
\item{cex.main.outl}{The magnification to be used for the main label of the outlier plot.}
\item{cex.lab.outl}{The magnification to be used for X- and Y-axis labels of the outlier plot.}
\item{Color}{The color to be used for the Empirical Variance Function (EVF) and the standard normal distribution in the variance function plot and the normality plot that show the densities of the standardized residuals and the normal distribution, respectively. Default \code{Color="red"}.}
\item{Loess.Span}{The parameter \eqn{\alpha} that determines the degree of smoothing of the EVF that is shown in the variance function plot. Default \code{Loess.Span=0.75}.}
\item{verbose}{A logical value indicating whether verbose output should be generated.}
\item{...}{Other arguments to be passed.}
}
\value{No return value, called for side effects.}
\references{
Van der Elst, W. (2023). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} New York: Springer. 
}
\author{
Wim Van der Elst
}
\examples{
# Replicate the Stage 1 results that were obtained in 
# Case study 1 of Chapter 4 in Van der Elst (2023)
# ---------------------------------------------------
library(NormData)   # load the NormData package
data(GCSE)          # load the GCSE dataset

# Conduct the Stage 1 analysis
Model.1.GCSE <- Stage.1(Dataset=GCSE, 
  Model=Science.Exam~Gender)

summary(Model.1.GCSE)
plot(Model.1.GCSE, Add.Jitter = .2)

# Use blue color for EVF and density normal distribution
plot(Model.1.GCSE, Add.Jitter = .2, Color="blue")

# Change the title of the variance function plot into
# "Variance function plot, residuals Science exam"
plot(Model.1.GCSE, Add.Jitter = .2, 
  Main.Homosced.2 = "Variance function plot, residuals Science exam")

# Use a 95 percent CI around the line of perfect agreement in the
# QQ plot of normality
plot(Model.1.GCSE, Add.Jitter = .2, 
     Confidence.QQ.Normality = .9)


# Replicate the Stage 1 results that were obtained in 
# Case study 1 of Chapter 7 in Van der Elst (2023)
# ---------------------------------------------------
library(NormData)   # load the NormData package
data(Substitution)  # load the Substitution dataset

# Add the variable Age.C (= Age centered) to the Substitution dataset
Substitution$Age.C <- Substitution$Age - 50

# Fit the final Stage 1 model
Substitution.Model.9 <- Stage.1(Dataset=Substitution, 
   Alpha=0.005, Model=LDST~Age.C+LE,
   Order.Poly.Var=1) # Order.Poly.Var=1 specifies a linear polynomial
                     # for the variance prediction function

# Final Stage 1 model
summary(Substitution.Model.9)
plot(Substitution.Model.9) 

# Request a variance function plot that assumes that 
# the homoscedasticity assumption is valid
plot(Substitution.Model.9, Assume.Homoscedasticity = TRUE) 
}
\keyword{Diagnostics}
\keyword{Residual plots}
\keyword{Regression-based normative data}
\keyword{Assumption checks}
\keyword{Normality test}
\keyword{Outliers}
\keyword{Homoscedasticity test}
\keyword{Stage 1}