\name{RejectAssignment}
\alias{RejectAssignment}
\alias{RejectAssignments}
\alias{reject}
\title{Reject Assignment}
\description{Reject a Worker's assignment (or multiple assignments) submitted for a HIT. Feedback should be provided for why an assignment was rejected.}
\usage{
RejectAssignment(	assignments, feedback = NULL, 
					keypair = credentials(), print = TRUE, browser = FALSE, 
					log.requests = TRUE, sandbox = FALSE, validation.test = FALSE)
}
\arguments{
  \item{assignments}{A character string containing an AssignmentId, or a vector of multiple character strings containing multiple AssignmentIds, to reject.}
  \item{feedback}{An optional character string containing any feedback for a worker. This must have length 1 or length equal to the number of workers.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{Reject assignments, by AssignmentId (as returned by \code{\link{GetAssignment}}). More advanced functionality to quickly reject many or all assignments (ala \code{\link{ApproveAllAssignments}}) is intentionally not provided.

\code{RejectAssignments()} and \code{reject()} are aliases.
}
\value{A dataframe containing the list of AssignmentIds, feedback (if any), and whether or not each rejection request was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_RejectAssignmentOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{ApproveAssignment}}
}
\examples{
\dontrun{
RejectAssignment(assignments="26XXH0JPPSI23H54YVG7BKLEXAMPLE")
}
}
\keyword{Assignments}