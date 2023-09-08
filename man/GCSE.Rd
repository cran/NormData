\name{GCSE}
\alias{GCSE}
\docType{data}
\title{GCSE exam score}
\description{
Thiis dataset contains the scores on a written science exam (General Certificate of Secondary Education; GCSE) that is taken by \eqn{N = 1905} students in \eqn{73} schools in England. The exam is taken at the end of compulsory schooling, when students are typically \eqn{16} years old. The actual score maximum is \eqn{160}, but here a rescaled score (with max value \eqn{100}) is provided. The data originally come from the package \code{mlmRev}, dataset \code{Gcsemv}.}  
\usage{data(GCSE)}
\format{
A \code{data.frame} with \eqn{1905} observations on \eqn{3} variables.
  \describe{
    \item{\code{Id}}{The Id number of the student.}
    \item{\code{Gender}}{The gender of the student, coded as M = male and F = female.}
    \item{\code{Science.Exam}}{The science exam score.}
}}
\keyword{GCSE}
\keyword{Dataset}