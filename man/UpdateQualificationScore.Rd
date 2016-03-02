\name{UpdateQualificationScore}
\alias{UpdateQualificationScore}
\alias{updatequalscore}
\title{Update a worker's score for a QualificationType}
\description{Update a worker's score for a QualificationType that you created. Scores for built-in QualificationTypes (e.g., location, worker statistics) cannot be updated.}
\usage{
UpdateQualificationScore(qual, workers, values = NULL, increment = NULL, 
                         verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{qual}{A character string containing a QualificationTypeId.}
  \item{workers}{A character string containing a WorkerId, or a vector of character strings containing multiple WorkerIds.}
  \item{values}{A character string containing an integer value to be assigned to the worker, or a vector of character strings containing integer values to be assigned to each worker (and thus must have length equal to the number of workers).}
  \item{increment}{An optional character string specifying, in lieu of \dQuote{values}, the amount that each worker's current QualfiicationScore should be increased.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
A function to update the Qualification score assigned to one or more workers for the specified custom QualificationType. The simplest use is to specify a QualificationTypeId, a WorkerId, and a value to be assigned to the worker. Scores for multiple workers can be updated in one request. 

Additionally, the \code{increment} parameter allows you to increase (or decrease) each of the specified workers scores by the specified amount. This might be useful, for example, to keep a QualificationType that records how many of a specific style of HIT a worker has completed and increase the value of each worker's score by 1 after they complete a HIT.

\code{updatequalscore()} is an alias.
}
\value{A data frame containing the QualificationTypeId, WorkerId, Qualification score, and whether the request to update each was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_UpdateQualificationScoreOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GetQualificationScore}}

\code{\link{GetQualifications}}
}
\examples{
\dontrun{
qual1 <- 
CreateQualificationType(name="Worked for me before",
    description="This qualification is for people who have worked for me before",
    status = "Active",
    keywords="Worked for me before")
AssignQualification(qual1$QualificationTypeId, "A1RO9UJNWXMU65", value="50")
UpdateQualificationScore(qual1$QualificationTypeId, "A1RO9UJNWXMU65", value="95")
UpdateQualificationScore(qual1$QualificationTypeId, "A1RO9UJNWXMU65", increment="1")
DisposeQualificationType(qual1$QualificationTypeId)
}
}
\keyword{Qualifications}