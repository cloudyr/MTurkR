\name{RejectQualification}
\alias{RejectQualification}
\alias{RejectQualifications}
\alias{rejectrequest}
\title{Reject a Worker's Qualification Request}
\description{Reject a Worker's request for a Qualification.}
\usage{
RejectQualification(qual.request, reason = NULL, 
                    verbose = getOption('MTurkR.verbose'), ...)
}
\arguments{
  \item{qual.request}{A character string containing a QualificationRequestId, or a vector of character strings containing multiple QualificationRequestIds (e.g., as returned by \code{\link{GetQualificationRequests}}).}
  \item{reason}{An optional character string, or vector of character strings of length equal to length of the \code{qual.requests} parameter, supplying each worker with a reason for rejecting their request for the Qualification. Workers will see this message. Maximum of 1024 characters.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MturkR.verbose')}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{Qualifications are publicly visible to workers on the MTurk website and workers can request Qualifications (e.g., when a HIT requires a QualificationType that they have not been assigned). QualificationRequests can be retrieved via \code{\link{GetQualificationRequests}}. This function rejects specified qualification requests.

\code{RejectQualifications()} and \code{rejectrequest()} are aliases.
}
\value{A dataframe containing the QualificationRequestId, reason for rejection (if applicable), and whether each request was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_RejectQualificationRequestOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GetQualificationRequests}}

\code{\link{GrantQualification}}
}
\examples{
\dontrun{
RejectQualification("2YCIA0RYNJ9262B1D82MPTUEXAMPLE",
                    reason="I am not currently granting this QualificationType.")
}
}
\keyword{Qualifications}