\name{GrantQualification}
\alias{GrantQualification}
\alias{GrantQualifications}
\alias{grantqual}
\title{Grant Qualification Request}
\description{Grant a Worker's request for a Qualification.}
\usage{
GrantQualification(	qual.requests, values, 
					keypair = credentials(), print = TRUE, browser = FALSE, 
					log.requests = TRUE, sandbox = FALSE)
}
\arguments{
  \item{qual.requests}{A character string containing a QualificationRequestId (for example, returned by \code{\link{GetQualificationRequests}}), or a vector of QualificationRequestIds.}
  \item{values}{A character string containing the value of the Qualification to be assigned to the worker, or a vector of values of length equal ot hte number of QualificationRequests.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
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