\name{GrantQualification}
\alias{GrantQualification}
\alias{GrantQualifications}
\alias{grantqual}
\alias{RejectQualification}
\alias{RejectQualifications}
\alias{rejectrequest}
\title{Grant/Reject Qualification Request}
\description{Grant or reject a worker's request for a Qualification.}
\usage{
GrantQualification(qual.requests, values, 
                   verbose = getOption('MTurkR.verbose', TRUE), ...)

RejectQualification(qual.requests, reason = NULL, 
                    verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{qual.requests}{A character string containing a QualificationRequestId (for example, returned by \code{\link{GetQualificationRequests}}), or a vector of QualificationRequestIds.}
  \item{values}{A character string containing the value of the Qualification to be assigned to the worker, or a vector of values of length equal to the number of QualificationRequests.}
  \item{reason}{An optional character string, or vector of character strings of length equal to length of the \code{qual.requests} parameter, supplying each worker with a reason for rejecting their request for the Qualification. Workers will see this message. Maximum of 1024 characters.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
Qualifications are publicly visible to workers on the MTurk website and workers can request Qualifications (e.g., when a HIT requires a QualificationType that they have not been assigned). QualificationRequests can be retrieved via \code{\link{GetQualificationRequests}}. \code{GrantQualification} grants the specified qualification requests. Requests can be rejected with \code{\link{RejectQualifications}}.

Note that granting a qualification may have the consequence of modifying a worker's existing qualification score. For example, if a worker already has a score of 100 on a given QualificationType and then requests the same QualificationType, a \code{GrantQualification} action might increase or decrease that worker's qualification score.

Similarly, rejecting a qualification is not the same as revoking a worker's Qualification. For example, if a worker already has a score of 100 on a given QualificationType and then requests the same QualificationType, a \code{RejectQualification} leaves the worker's existing Qualification in place. Use \code{\link{RevokeQualification}} to entirely remove a worker's Qualification.

\code{GrantQualifications()} and \code{grantqual()} are aliases; \code{RejectQualifications()} and \code{rejectrequest()} are aliases.
}
\value{A data frame containing the QualificationRequestId, reason for rejection (if applicable; only for \code{RejectQualification}), and whether each request was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GrantQualificationOperation.html}{API Reference: GrantQualification}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_RejectQualificationRequestOperation.html}{API Reference: RejectQualification}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GetQualificationRequests}}
}
\examples{
\dontrun{
# create QualificationType
qual1 <- CreateQualificationType(name="Requestable Qualification",
           description="This is a test qualification that can be requested.",
           status = "Active")

# poll for qualification requests
qrs <- GetQualificationRequests(qual1$QualificationTypeId)

# grant a qualification request
GrantQualification(qrs$QualificationRequestId[1], values = "100")

# correct a worker's score (note use of `SubjectId`, not `WorkerId`)
UpdateQualificationScore(qrs$QualificationTypeId[1], qrs$SubjectId[1], value = "95")

# reject a qualification request
RejectQualification(qrs$QualificationTypeId[2], reason = "Sorry!")

# cleanup
DisposeQualificationType(qual1$QualificationTypeId)
}
}
\keyword{Qualifications}
