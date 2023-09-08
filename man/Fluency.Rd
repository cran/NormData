\name{Fluency}
\alias{Fluency}
\docType{data}
\title{Verbal fluency data}
\description{
This dataset contains the scores of the Fruits Verbal Fluency Test. The \eqn{N = 1241} test participants were instructed to generate as many words as possible that belong to the category `fruits' (e.g., apple, orange, banana, etc.) within \eqn{60} seconds. These are simulated data based on the results described in Rivera \emph{et al.} (2019). }  
\usage{data(Fluency)}
\format{
A \code{data.frame} with \eqn{1241} observations on \eqn{3} variables.
  \describe{
    \item{\code{Id}}{The Id number of the test participant.}
    \item{\code{Country}}{The country where the test participant lives, coded as a factor.}
    \item{\code{Fruits}}{The number of correctly generated fruit names. Higher score is better.}
}}
\references{
Rivera \emph{et al.} (2019). Normative Data For Verbal Fluency in Healthy Latin American Adults: Letter M, and Fruits and Occupations Categories. \emph{Neuropsychology, 33,} 287-300.

Van der Elst, W. (2023). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} New York: Springer. 
}
\keyword{Fluency}
\keyword{Dataset}