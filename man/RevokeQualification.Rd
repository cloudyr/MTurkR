\name{RevokeQualification}
\alias{RevokeQualification}
\alias{RevokeQualifications}
\alias{revokequal}
\title{Revoke a Qualification from a Worker}
\description{Revoke a Qualification from a worker or multiple workers. This deletes their qualification score and any record thereof.}
\usage{
RevokeQualification(qual, worker, reason = NULL, 
                    verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{qual}{A character string containing a QualificationTypeId.}
  \item{worker}{A character string containing a WorkerId, or a vector of character strings containing multiple WorkerIds.}
  \item{reason}{An optional character string, or vector of character strings of length equal to length of the \code{workers} parameter, supplying each worker with a reason for revoking their Qualification. Workers will see this message.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
A simple function to revoke a Qualification assigned to one or more workers.

\code{RevokeQualifications()} and \code{revokequal()} are aliases.
}
\value{A data frame containing the QualificationTypeId, WorkerId, reason (if applicable), and whether each request was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_RevokeQualificationOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GrantQualification}}

\code{\link{RejectQualification}}
}
\examples{
\dontrun{
qual1 <- 
AssignQualification(workers = "A1RO9UJNWXMU65",
                    name = "Worked for me before",
                    description = "This qualification is for people who have worked for me before",
                    status = "Active",
                    keywords = "Worked for me before")

RevokeQualification(qual = qual1$QualificationTypeId,
                    worker = qual1$WorkerId,
                    reason = "No longer needed")

DisposeQualificationType(qual1$QualificationTypeId)

}
}
\keyword{Qualifications}