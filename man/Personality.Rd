\name{Personality}
\alias{Personality}
\docType{data}
\title{Data of the Openness scale of a personality test}
\description{
These are the data of the Openness subscale of International Personality Item Pool (ipip.ori.org). This subscale consists of 5 items: 1 = \emph{I am full of ideas}, 2 = \emph{I avoid difficult reading material}, 3 = \emph{I carry the conversation to a higher level}, 4 = \emph{I spend time reflecting on things}, and 5 = \emph{I will not probe deeply into a subject}. Each item is scored on a 6-point response scale with answer categories 1 = very inaccurate, 2 = moderately inaccurate, 3 = slightly inaccurate, 4 = slightly accurate, 5 = moderately accurate, and 6 = very accurate. The Openness scale score corresponds to the sum of the individual item scores, with items 2 and 5 being reverse scored. The raw Openness scale score ranges between 5 and 30. A higher score is indicative of higher levels of curiosity, intellectualism, imagination, and aesthetic interests (McCrae, 1994).

The data were collected as part of the Synthetic Apeture Personality Assessment (SAPA http://sapa-project.org) web-based personality assessment project.
}  
\usage{data(Personality)}
\format{
A \code{data.frame} with 2137 observations on 3 variables.
  \describe{
    \item{\code{Id}}{The Id number of the participant.}
    \item{\code{LE}}{The Level of Education (LE) of the participant, coded as 1 = less than high school, 2 = finished high school, 3 = some college but did not graduate, 4 = college graduate, and 5 = graduate degree.}
     \item{\code{Openness}}{Level of Openness.}}
}
\references{
McCrae, R. R. (1994). Openness to Experience: expanding the boundaries of factor V. \emph{European Journal of Personality, 8,} 251-272.

Van der Elst, W. (2024). \emph{Regression-based normative data for psychological assessment: A hands-on approach using R.} Springer Nature. 
}
\keyword{Personality}
\keyword{Dataset}