\name{RevokeQualification}
\alias{RevokeQualification}
\alias{RevokeQualifications}
\alias{revokequal}
\title{Revoke a Qualification from a Worker}
\description{Revoke a Qualification from a worker or multiple workers. This deletes their qualification score and any record thereof.}
\usage{
RevokeQualification(qual, worker, reason = NULL, 
					keypair = credentials(), print = TRUE, browser = FALSE, 
					log.requests = TRUE, sandbox = FALSE, validation.test = FALSE)
}
\arguments{
  \item{qual}{A character string containing a QualificationTypeId.}
  \item{worker}{A character string containing a WorkerId, or a vector of character strings containing multiple WorkerIds.}
  \item{reason}{An optional character string, or vector of character strings of length equal to length of the \code{workers} parameter, supplying each worker with a reason for revoking their Qualification. Workers will see this message.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{
A simple function to revoke a Qualification assigned to one or more workers.

\code{RevokeQualifications()} and \code{revokequal()} are aliases.
}
\value{A dataframe containing the QualificationTypeId, WorkerId, reason (if applicable), and whether each request was valid.}
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
a <- "2YCIA0RYNJ9262B1D82MPTUEXAMPLE"
b <- "A1RO9UJNWXMU65"
c <- "Performance has dropped below required level"
RevokeQualification(qual=a,worker=b,reason=c)

}
}
\keyword{Qualifications}