\name{GrantQualification}
\alias{GrantQualification}
\alias{GrantQualifications}
\alias{grantqual}
\title{Grant Qualification Request}
\description{Grant a Worker's request for a Qualification.}
\usage{
GrantQualification(qual.requests, values, verbose = getOption('MTurkR.verbose'), ...)
}
\arguments{
  \item{qual.requests}{A character string containing a QualificationRequestId (for example, returned by \code{\link{GetQualificationRequests}}), or a vector of QualificationRequestIds.}
  \item{values}{A character string containing the value of the Qualification to be assigned to the worker, or a vector of values of length equal ot hte number of QualificationRequests.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MturkR.verbose')}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
Qualifications are publicly visible to workers on the MTurk website and workers can request Qualifications (e.g., when a HIT requires a QualificationType that they have not been assigned). QualificationRequests can be retrieved via \code{\link{GetQualificationRequests}}. This function grants specified qualification requests. Requests can be rejected with \code{\link{RejectQualifications}}.

\code{GrantQualifications()} and \code{grantqual()} are aliases.
}
\value{A dataframe containing the QualificationRequestId and whether each request was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GrantQualificationOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GetQualificationRequests}}

\code{\link{RejectQualification}}
}
\examples{
\dontrun{
GrantQualification("2YCIA0RYNJ9262B1D82MPTUEXAMPLE","100")
}
}
\keyword{Qualifications}