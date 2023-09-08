\name{Stage.1}
\alias{Stage.1}
\title{Stage 1 of the regression-based normative analysis}
\description{
The function \code{Stage.1} fits a regression model with the specified mean and residual variance components, and conducts several model checks (homoscedasticity, normality, absence of outliers, and multicollinearity) that are useful in a setting where regression-based normative data have to be established.}
\usage{
Stage.1(Dataset, Model, Order.Poly.Var=3, 
Alpha=0.05, Alpha.Homosc=0.05, Alpha.Norm = .05, 
Assume.Homoscedasticity=NULL,
Test.Assumptions=TRUE, Outlier.Cut.Off=4, 
Show.VIF=TRUE, GVIF.Threshold=10, Sandwich.Type="HC0", 
Alpha.CI.Group.Spec.SD.Resid=0.01)
}
\arguments{
  \item{Dataset}{A \code{data.frame} that should consist of one line per test participant (the so-called `wide' data-format). Each line should contain (at least) one test score and one independent variable.}
  \item{Model}{The regression model to be fitted (mean structure). A formula should be provided using the syntaxis of the  \code{lm} function (for help, see \code{?lm}). For example, \code{Test.Score~Gender} will fit a linear regression model in which \code{Gender} (the independent variable) is regressed on \code{Test.Score}. \code{Test.Score~Gender+Age+ Gender:Age} will regress \code{Test.Score} on \code{Gender}, \code{Age}, and the interaction term. \code{Test.Score~1} will fit an intercept-only model.}
\item{Order.Poly.Var}{If the homoscedasticity assumption is violated and the mean structure of the fitted model contains at least one quantitative variable, a polynomial variance prediction function is fitted. The argument \code{Order.Poly.Var=} determines the order of the polynomial, e.g., \code{Order.Poly.Var=1}, \code{Order.Poly.Var=2}, \code{Order.Poly.Var=3} for linear, quadratic and cubic polynomials, respectively. By default, \code{Order.Poly.Var = 3}.}
\item{Alpha}{The significance level to be used when conducting inference for the mean structure of the model. Default \code{Alpha=0.05}.}
\item{Alpha.Homosc}{The significance level to be used to evaluate the homoscedasticity assumption based on the Levene test (when all independent variables in the model are qualitative) or the Breusch-Pagan test (when at least one of the independent variables is quantitative). Default \code{Alpha.Homosc=0.05}.}
\item{Alpha.Norm}{The significance level to be used to test the normality assumption for the standardized errors using the Shapiro-Wilk test. The normality assumption is evaluated based on the standardized residuals in the normative dataset, which are computed as explained in the \code{Assume.Homoscedasticity=} argument documentation below. Default \code{Alpha.Shapiro=0.05}.}
\item{Assume.Homoscedasticity}{Logical. The \code{NormData} package `decides' whether the homoscedasticity assumption is valid based on the Levene or Breusch-Pagan tests (for models that only include qualitative independent variables versus models that include at least one quantitative independent variable, respectively). The \code{Assume.Homoscedasticity= TRUE/FALSE} argument can be used to overrule this decision process and `force' the \code{NormData} package to assume or not assume homoscedasticity. When the argument \cr  \code{Assume.Homoscedasticity=TRUE} is used, the argument \code{Alpha.Homosc=0} is automatically used in the \code{Stage.1()} function call and thus the homoscedasticity assumption will never be rejected (because the \eqn{p}-value of the Levene or Breusch-Pagan test-statistics will always be larger than the specified \eqn{\alpha=0}). When \code{Assume.Homoscedasticity=FALSE} is used, the argument \code{Alpha.Homosc=1} is automatically used thus the homoscedasticity assumption will always be rejected (because the \eqn{p}-value of the Levene or Breusch-Pagan test-statistics will always be smaller than the specified \eqn{\alpha=1}).

By default, the standardized residuals \eqn{\widehat{\delta}_i} that are shown in the normality and outlier output sections of the results (and the plots, see \code{\link{plot Stage.1}}) are computed based on the overall residual standard error when the homoscedasticity assumption is valid (i.e., as \eqn{\widehat{\delta}_i = \frac{\widehat{\varepsilon}_i}{\widehat{\sigma}^2_{\varepsilon}}}, with  \eqn{\widehat{\sigma}^2_{\varepsilon}} corresponding to the overall residual standard error), or based on prediction-specific residual standard errors when the homoscedasticity assumption is invalid (i.e., as \eqn{\widehat{\delta}_i = \frac{\widehat{\varepsilon}_i}{\widehat{\sigma}^2_{\varepsilon_i}}}, with  \eqn{\widehat{\sigma}^2_{\varepsilon_i}} corresponding to e.g., a cubic polynomial variance prediction function \eqn{\widehat{\sigma}^2_{\varepsilon_i} = \widehat{\gamma}_0 + \widehat{\gamma}_1 \: \widehat{Y} + \widehat{\gamma}_2 \: \widehat{Y}^2 + {\gamma}_3 \: \widehat{Y}^3} when the mean structure of the model contains quantitiative independent variables).
}
\item{Test.Assumptions}{Logical. Should the model assumptions be evaluated for the specified  model? Default \code{Test.Assumptions=TRUE}.}
\item{Outlier.Cut.Off}{Outliers are evaluated based on the standardized residuals, which are computed as explained in the \code{Assume.Homoscedasticity=} argument documentation. The \code{Outlier.Cut.Off=} argument specifies the absolute value that is used as a threshold to detect outliers. Default \code{Outlier.Cut.Off=4}, so test scores with standardized residuals \eqn{< -4} or \eqn{> 4} are flagged as outliers.}
\item{Show.VIF}{Logical. Should the generalized VIF (Fox and Monette, 1992) be shown when the function \code{summary()} is applied to the fitted object? Default \code{Show.VIF=TRUE}. If all  names of the independent variables in the fitted Stage 1 model contain the string `Age' (e.g., Age, Age.2 and Age.3), a higher-order polynomial model for the mean structure is being fitted. For such models, multicollinearity diagnostics are essentially irrelevant (see Van der Elst, 2023) and in such cases the generalized VIF is not printed in the summary output. The generalized VIF is also not shown whenn there is only one independent variable in the model (because multicollinearity relates to the linear association of two or more independent variables).}
\item{GVIF.Threshold}{The threshold value to be used to detect multicollinearity based on the generalized VIF. Default \code{GVIF.Threshold=10}.}
\item{Sandwich.Type}{When the homoscedasticity assumption is violated, so-called sandwich estimators (or heteroscedasticity-consistent estimators) for the standard errors of the regression parameters are used. For example, the sandwich estimator for the standard error of \eqn{\widehat{\beta}_1} in a simple linear regression model corresponds to \eqn{\widehat{\sigma}_{{\beta}_1}=\sqrt{\frac{
\sum\limits_{i=1}^{N}\left(\left(X_i - \widehat{\mu}_{X_{i}}\right)^2 \: \widehat{\varepsilon}_i^2 \right)}{\left(\sum\limits_{i=1}^{N}(X_i - \widehat{\mu}_{X_{i}})^2\right)^2}}}. For multiple linear regression models, the sandwich estimators for the different independent variables  \eqn{\widehat{\sigma}_{{\beta}_0}}, \eqn{\widehat{\sigma}_{{\beta}_1}}, ...correspond to the square roots of the diagonal elements of \eqn{\boldsymbol{\widehat{\Sigma}}_{\beta} = \\
\left(\boldsymbol{X}^{'}\boldsymbol{X}\right)^{-1}
\left(\boldsymbol{X}^{'} \left[\begin{array}{cccc}
\widehat{\varepsilon}^2_1 & 0 & \ldots & 0\\
0 & \widehat{\varepsilon}^2_2 & \ldots & 0\\
\vdots & \vdots & \ddots & 0\\
0 & 0 & 0 & \widehat{\varepsilon}^2_N
\end{array}\right] \boldsymbol{X}\right)
\left(\boldsymbol{X}^{'}\boldsymbol{X}\right)^{-1}.}
The sandwich-estimators that are shown in the above expressions are referred to as the Heteroscedasticity-Consistent 0 estimator (or HC0 estimator), which is the first sandwich-estimator that was proposed in the literature. The HC0 sandwich-estimator is justified based on asymptotic theory, and its application thus requires large sample sizes. For smaller sample sizes of \eqn{N < 250}, the use of the HC3 estimator is recommended because the HC0 sandwich-estimator tends to be negatively biased (Long and Erwin, 2000). By default, the HC0 estimator is used. The argument \code{Sandwich.Type=} can be used to request another type of the heteroscedasticity-consistent estimator. For details on these estimators, see the \code{vcovHC} function of the \code{sandwich} package. If \eqn{N < 250} and the homoscedasticity assumption is violated, a note will be given that the use of the HC3-estimator is recommended. To this end, the argument \code{Sandwich.Type="HC3"} can be added in the \code{Stage.1()} function call.}
\item{Alpha.CI.Group.Spec.SD.Resid}{The \eqn{\alpha}-level to be used for the CIs around the prediction-specific residual standard errors (when the homoscedasticity assumption is invalid and the model only contains qualitative independent variable). These CIs are used in the variance function plot. Default \code{Alpha.CI.Group.Spec.SD.Resid=0.01}.}
}
\details{
For details, see Van der Elst (2023).
}

\value{An object of class \code{Stage.1} with components,
\item{HomoNorm}{The fitted regression model assuming homoscedasticity and normality.}
\item{NoHomoNorm}{The fitted regression model assuming no homoscedasticity and normality.}
\item{HomoNoNorm}{The fitted regression model assuming homoscedasticity and no normality. }
\item{NoHomoNoNorm}{The fitted regression model assuming no homoscedasticity and no normality. }
\item{Predicted}{The predicted test scores based on the fitted model.}
\item{Sandwich.Type}{The requested sandwich estimator.}
\item{Order.Poly.Var}{The order of the polynomial variance prediction function.}
}
\references{
Fox, J. and Monette, G. (1992). Generalized collinearity diagnostics. \emph{JASA, 87,} 178-183.

Long, J. S. and Ervin, L. H. (2000). Using Heteroscedasticity Consistent Standard Errors in the Linear Regression Model. \emph{The American Statistician, 54,} 217-224.

Van der Elst, W. (2023). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} New York: Springer. 
}
\author{
Wim Van der Elst
}
\seealso{\code{\link{plot Stage.1}}, \code{\link{Stage.2.AutoScore}}, \code{\link{Stage.2.NormScore}}, \code{\link{Stage.2.NormTable}}}
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
plot(Model.1.GCSE)


# Replicate the Stage 1 results that were obtained in 
# Case study 1 of Chapter 7 in Van der Elst (2023)
# ---------------------------------------------------
library(NormData)   # load the NormData package
data(Substitution)  # load the Substitution dataset

# Add the variable Age.C (= Age centered) and its 
# quadratic and cubic terms to the Substitution dataset
Substitution$Age.C <- Substitution$Age - 50
Substitution$Age.C2 <- (Substitution$Age - 50)**2
Substitution$Age.C3 <- (Substitution$Age - 50)**3

# Fit the full Stage 1 model
Substitution.Model.1 <- Stage.1(Dataset=Substitution,
   Model=LDST~Age.C+Age.C2+Age.C3+Gender+LE+Age.C:LE+
   Gender:LE+Age.C:Gender, Alpha=0.005)
summary(Substitution.Model.1)

# Fit the model in which the non-significant Age.C:Gender
# interaction term is removed
Substitution.Model.2 <- Stage.1(Dataset=Substitution, 
    Alpha=0.005,
    Model=LDST~Age.C+Age.C2+Age.C3+Gender+LE+
    Age.C:LE+Gender:LE)
summary(Substitution.Model.2)

# Evaluate the significance of the Gender:LE interaction term
# GLT is used because the interaction involves multiple regression
# parameters
GLT.1 <- GLT(Dataset=Substitution, Alpha=0.005, 
   Unrestricted.Model=LDST~Age.C+Age.C2+Age.C3+
      Gender+LE+Age.C:LE+Gender:LE, 
   Restricted.Model=LDST~Age.C+Age.C2+Age.C3+
      Gender+LE+Age.C:LE)
summary(GLT.1)

# Fit the model in which the non-significant Gender:LE
# interaction term is removed
Substitution.Model.3 <- Stage.1(Dataset=Substitution, 
    Alpha=0.005,
    Model=LDST~Age.C+Age.C2+Age.C3+Gender+LE+Age.C:LE)
summary(Substitution.Model.3)

# Evaluate the significance of the Age:LE interaction
# using the General Linear Test framework
GLT.2 <- GLT(Dataset=Substitution,
    Unrestricted.Model=LDST~Age.C+Age.C2+Age.C3+Gender+LE+Age.C:LE,
    Restricted.Model=LDST~Age.C+Age.C2+Age.C3+Gender+LE, Alpha=0.005)
summary(GLT.2)

# Fit the model in which the non-significant Age_c:LE
# interaction term is removed
Substitution.Model.4 <- Stage.1(Dataset=Substitution,
   Alpha=0.005, Model=LDST~Age.C+Age.C2+Age.C3+Gender+LE)
summary(Substitution.Model.4)

# Fit the model in which the non-significant Age.C3 term is removed
Substitution.Model.5 <- Stage.1(Dataset=Substitution,
   Alpha=0.005, Model=LDST~Age.C+Age.C2+Gender+LE)
summary(Substitution.Model.5)

# Fit the model in which the non-significant Age.C2 term is removed
Substitution.Model.6 <- Stage.1(Dataset=Substitution,
   Alpha=0.005, Model=LDST~Age.C+Gender+LE)
summary(Substitution.Model.6)

# Fit the model in which the non-significant main effect of Gender 
# is removed
Substitution.Model.7 <- Stage.1(Dataset=Substitution, 
  Alpha=0.005, Model=LDST~Age.C+LE)
summary(Substitution.Model.7)
plot(Substitution.Model.7, Normality = FALSE, Outliers = FALSE)

# Check the significance of LE using the GLT framework
GLT.3 <- GLT(Dataset=Substitution, Alpha=0.005,
    Unrestricted.Model=LDST~Age.C+LE, 
    Restricted.Model=LDST~Age.C)
summary(GLT.3)

# Residual variance function. Substitution.Model.7 uses
# a cubic polynomial variance prediction function. 
# Remove cubic Pred.Y term from Substitution.Model.7, so
# fit quadratic variance prediction function
Substitution.Model.8 <- Stage.1(Dataset=Substitution, 
    Alpha=0.005, Model=LDST~Age.C+LE,
    Order.Poly.Var=2)  # Order.Poly.Var=2 specifies a quadratic polynomial
                       # for the variiance prediction function
summary(Substitution.Model.8)
plot(Substitution.Model.8, Normality = FALSE, Outliers = FALSE)

# Remove quadratic Pred.Y term, so fit linear variance 
# prediction function
Substitution.Model.9 <- Stage.1(Dataset=Substitution, 
    Alpha=0.005, Model=LDST~Age.C+LE,
    Order.Poly.Var=1) # Order.Poly.Var=1 specifies a linear polynomial
                      # for the variiance prediction function

# Final Stage 1 model
summary(Substitution.Model.9)
plot(Substitution.Model.9) 
}
\keyword{Regression-based normative data}
\keyword{Stage 1}
\keyword{Fitted model}
\keyword{Assumption checks}
\keyword{Levene Test}
\keyword{Breusch-Pagann Test}
\keyword{Shapiro-Wilk test}
\keyword{Outliers}
\keyword{Normality test}
\keyword{Homoscedasticity test}
\keyword{Multicollinearity}
\keyword{VIF}
\keyword{Sandwich estimator}
\keyword{Polynomial function}
\keyword{Mean prediction function}
\keyword{Variance prediction function}