\name{Substitution}
\alias{Substitution}
\docType{data}
\title{Substitution test data}
\description{
Substitution tests are speed-dependent tasks that require the participant to match particular signs (symbols, digits, or letters) to other signs within a specified time period. The LDST is an adaptation of earlier substitution tests, such as the Digit Symbol Substitution Test (DSST; Wechsler, 1981) and the Symbol Digit Modalities Test (SDMT; Smith, 1982). The LDST differs from other substitution tests in that the key consists of 'over-learned' signs,  i.e., letters and digits. These are simulated data that are based on the results described in Van der Elst \emph{et al.} (2006) (see Table 2).} 
\usage{data(Substitution)}
\format{
A \code{data.frame} with 1765 observations on 5 variables.
  \describe{
   \item{\code{Id}}{The Id number of the participant.}
    \item{\code{Age}}{The age of the participant, in years.}
    \item{\code{Gender}}{The gender of the participant, coded as a factor with levels \code{Male} and \code{Female}.}
    \item{\code{LE}}{The Level of Education of the test participant, coded as a factor with levels \code{Low}, \code{Average} and \code{High}.}
     \item{\code{LDST}}{The test score on the LDST (written version), i.e., the number of correct substitutions made in 60 seconds. A higher score reflects better performance.}
}
}
\keyword{Substitution}
\keyword{Dataset}