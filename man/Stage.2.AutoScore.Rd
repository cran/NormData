\name{Stage.2.AutoScore}
\alias{Stage.2.AutoScore}
\title{Make an automatic scoring sheet}
\description{
This function is useful to construct an automatic scoring sheet that implements the Stage 2 normative conversion approach in a spreadsheet. In particular, a spreadsheet will be created with three tabs that should be copy-pasted to the different sections of the \code{Model details} tab of the template file. For details, see Van der Elst (2023). 
}
\usage{
Stage.2.AutoScore(Stage.1.Model, Assume.Homoscedasticity, 
  Assume.Normality, Folder, NameFile="NormSheet.xlsx", 
  verbose=TRUE)
}
\arguments{
  \item{Stage.1.Model}{A fitted object of class \code{Stage.1} that should be written to the Excel sheet (i.e., the final Stage 1 model).}
  \item{Assume.Homoscedasticity}{Logical. Should homoscedasticity be assumed? By default,  homoscedasticity is assumed when the \eqn{p}-value of the Levene or Breusch-Pagan test for the fitted Stage 1 model is above the specified \eqn{\alpha}-level in the \code{Stage.1()} function call. When homoscedasticity is assumed, an overall residual standard error is written to the spreadsheet. When homoscedasticity is not assumed, prediction-specific residual standard errors are written to the spreadsheet. The default decision procedure can be overruled by means of the arguments \cr  \code{Assume.Homoscedasticity=TRUE} or \code{Assume.Homoscedasticity=FALSE}.}
  \item{Assume.Normality}{Logical. Should normality of the standardized errors be assumed? By default, normality is assumed when the \eqn{p}-value of the Shapiro-Wilk test for the fitted Stage 1 model is above the specified \eqn{\alpha}-level in the \code{Stage.1()} function call. When normality is assumed, the CDF of the standard normal distribution is written to the spreadsheet. When normality is not assumed, the CDF of the standardized residuals in the normative sample is written to the speeadsheet. The default decision procedure can be overruled by means of the arguments argument \code{Assume.Normality=TRUE} or \code{Assume.Normality=FALSE}.}
  \item{Folder}{The folder where the spreadsheet file should be saved.}
  \item{NameFile}{The name of the file in which the normative tables should be saved. Default  \code{NameFile="NormTable.xlsx"}}  
\item{verbose}{A logical value indicating whether verbose output should be generated.}
}
\details{
For details, see Van der Elst (2023).
}

\value{
An object of class \code{Stage.2.AutoScore} with components,
\item{Mean.Structure}{The mean prediction function.}
\item{Residual.Structure}{The variance prediction function.}  
\item{Percentiles.Delta}{A table of the standardized residuals and their corresponding estimated percentile ranks (based on the CDF of the standard normal distribution or the CDF of the standardized residuals in the normative sample, see above).} 
}
\references{
Van der Elst, W. (2023). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} New York: Springer. 
}
\author{
Wim Van der Elst
}
\seealso{
\code{\link{Stage.1}}, \code{\link{Stage.2.NormTable}}, \code{\link{Stage.2.AutoScore}}
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

# Write the results to a spreadsheet file
Stage.2.AutoScore(Stage.1.Model=Model.1.GCSE, 
  Folder=tempdir(),   # Replace tempdir() by the desired folder  
  NameFile="GCSE.Output.xlsx")

# Copy-paste the information in GCSE.Output.xlsx to the
# template file, as detailed in Van der Elst (2023)


# Replicate the Stage 1 results that were obtained in 
# Case study 1 of Chapter 7 in Van der Elst (2023)
# ---------------------------------------------------
library(NormData)   # load the NormData package
data(Substitution)  # load the Substitution dataset

# Add the variable Age.C (= Age centered) to the Substitution dataset
Substitution$Age.C <- Substitution$Age - 50

# Fit the final Stage 1 model
Substitution.Model.9 <- Stage.1(Dataset=Substitution, 
   Alpha=0.005, Model=LDST~Age.C+LE, Order.Poly.Var=1) 

# Final Stage 1 model
summary(Substitution.Model.9)
plot(Substitution.Model.9) 

# Write the results to a spreadsheet file
Stage.2.AutoScore(Stage.1.Model=Substitution.Model.9,
   Folder=tempdir(),  # Replace tempdir() by the desired folder
   NameFile="LDST.Output.xlsx")

# Copy-paste the information in LDST.Output.xlsx to the
# template file, as detailed in Van der Elst (2023)
}
\keyword{Regression-based normative data}
\keyword{Stage 2}
\keyword{Auutomatic scoring sheet}
\keyword{Percentile ranks}
\keyword{Spreadsheet}