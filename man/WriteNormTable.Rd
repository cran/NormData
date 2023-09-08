\name{WriteNormTable}
\alias{WriteNormTable}
\title{
Write a normative table from R to a .txt/.csv/.xlsx file
}
\description{
The function \code{Stage.2.NormTable()} allows for deriving a normative table that shows percentile ranks \eqn{\hat{\pi}_0} that correspond to a wide range of raw test scores \eqn{Y_0} (stratified by the relevant independent variables). The raw R output format that is provided by the \code{Stage.2.NormTable()} function is not always convenient, especially when a large number of test scores are tabulated and the table is spread out over several lines. The function \code{WriteNormTable()} can be used to export the normative table to a \code{.txt}, \code{.csv} or \code{.xlsx} file. Such a file can then be opened in a spreadsheet (such as Google Sheets or LibreOffice), where the normative table can be put in a more user-friendly format.
}
\usage{
WriteNormTable(NormTable, Folder, NameFile="NormTable.xlsx", 
verbose=TRUE)
}
\arguments{
  \item{NormTable}{An object of class \code{Stage.2.NormTable} that contains the normative table that has to be exported.}
  \item{Folder}{The folder where the file with the normative table should be saved.}
  \item{NameFile}{The name of the file to which the normative table should be written. Only the extensions \code{.txt}, \code{.csv} or \code{.xlsx} can be used. If unspecified, the argument \code{NameFile="NormTable.xlsx"} is used by default. The \code{.txt} and \code{.csv} files use a space as the delimiter.}
\item{verbose}{A logical value indicating whether verbose output should be generated.}
}
\value{No return value, called for side effects.}
\references{
Van der Elst, W. (2023). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} New York: Springer. 
}
\author{
Wim Van der Elst}
\seealso{\code{\link{Stage.2.NormTable}}
}
\examples{
# Replicate the normative table that was obtained in 
# Case study 1 of Chapter 3 in Van der Elst (2023)
# -----------------------------------------------------
library(NormData) # load the NormData package
data(GCSE)        # load the GCSE dataset

# Fit the Stage 1 model
Model.1.GCSE <- Stage.1(Dataset=GCSE, 
    Model=Science.Exam~Gender)

# Make a normative table for raw Science Exam scores = 10, 
# 11, ... 85, stratified by Gender
NormTable.GCSE <- Stage.2.NormTable(Stage.1.Model=Model.1.GCSE,
    Test.Scores=c(10:85), 
    Grid.Norm.Table=data.frame(Gender=c("F", "M")))
summary(NormTable.GCSE)

# Write the normative table to the user's computer
WriteNormTable(NormTable=NormTable.GCSE, 
     NameFile="NormTable.GCSE.xlsx",
     Folder=tempdir()) # Replace tempdir() by the desired folder  


# Replicate the normative table that was obtained in 
# Case study 1 of Chapter 7 in Van der Elst (2023)
# ------------------------------------------------
library(NormData)   # load the NormData package
data(Substitution)  # load the Substitution dataset

# Make the new variable Age.C (= Age centered) that is 
# needed to fit the final Stage 1 model, 
# and add it to the Substitution dataset
Substitution$Age.C <- Substitution$Age - 50

# Fit the final Stage 1 model
Substitution.Model.9 <- Stage.1(Dataset=Substitution, 
   Alpha=0.005, Model=LDST~Age.C+LE, Order.Poly.Var=1) 

# Make a normative table for LDST scores = 10, 12, ... 56,
# stratified by Age and LE
NormTable.LDST <- Stage.2.NormTable(
  Stage.1.Model=Substitution.Model.9,
  Test.Scores=seq(from=10, to=56, by=2),
  Grid.Norm.Table=expand.grid(Age.C=seq(from=-30, to=30, by=1),
     LE=c("Low", "Average", "High")))

# Write the normative table to the user's computer
WriteNormTable(NormTable=NormTable.LDST, 
  NameFile="NormTable.LDST.xlsx",
  Folder=tempdir()) # Replace tempdir() by the desired folder  
}
\keyword{Export normative table}