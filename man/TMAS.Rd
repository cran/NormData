\name{TMAS}
\alias{TMAS}
\docType{data}
\title{TMAS data}
\description{
This dataset contains the scores of the Taylor Manifest Anxiety Scale (TMAS; Taylor, 1953),  administered online. A total of \eqn{523} test participants completed the questionnaire. The TMAS scale score ranges between \eqn{0} and \eqn{50}, with lower scores corresponding to higher levels of anxiety.}  
\usage{data(TMAS)}
\format{
A \code{data.frame} with \eqn{523} observations on \eqn{3} variables.
  \describe{
    \item{\code{Id}}{The Id number of the test participant.}
    \item{\code{Gender}}{The gender of the test participant, coded as a factor.}
    \item{\code{Score}}{The TMAS score. A higher value is iindicative for less anxiety.}
}}
\references{
Taylor, J. (1953). A personality scale of manifest anxiety. \emph{The Journal of Abnormal and Social Psychology, 48(2),} 285-290.

Van der Elst, W. (2024). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} Springer Nature. 
}
\keyword{TMAS}
\keyword{Dataset}