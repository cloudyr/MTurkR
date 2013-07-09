\name{Blocking Workers}
\alias{BlockWorker}
\alias{BlockWorkers}
\alias{block}
\alias{UnblockWorker}
\alias{UnblockWorkers}
\alias{unblock}
\alias{GetBlockedWorkers}
\alias{blockedworkers}
\title{Block/Unblock Worker(s)}
\description{Block or unblock a worker. This prevents a worker from completing any HITs for you while they are blocked, but does not affect their ability to complete work for other requesters or affect their worker statistics. \code{GetBlockedWorkers} retrieves your list of currently blocked workers.}
\usage{
BlockWorker(workers, reasons, keypair = credentials(), 
			print = TRUE, browser = FALSE, log.requests = TRUE,
			sandbox = FALSE, validation.test = FALSE)

UnblockWorker(	workers, reasons = NULL, keypair = credentials(), 
				print = TRUE, browser = FALSE, log.requests = TRUE,
				sandbox = FALSE, validation.test = FALSE)

GetBlockedWorkers(pagenumber = NULL, pagesize = NULL, keypair = credentials(), 
				print = TRUE, browser = FALSE, log.requests = TRUE,
				sandbox = FALSE, validation.test = FALSE)
}
\arguments{
  \item{workers}{A character string containing a WorkerId, or a vector of character strings containing multiple WorkerIds.}
  \item{reasons}{A character string containing a reason for blocking or unblocking a worker. This must have length 1 or length equal to the number of workers. It is required for \code{BlockWorker} and optional for \code{UnblockWorker}.}
  \item{pagenumber}{An optional integer (or character string) indicating which page of Blocked Workers search results should be returned. Most users can ignore this.}
  \item{pagesize}{An optional integer (or character string) indicating how many Blocked Workers should be returned per page of results. Most users can ignore this and the function will return the first 65,535 blocks.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{
\code{BlockWorker} prevents the specified worker from completing any of your HITs. \code{UnblockWorker} reverses this operation. 

\code{GetBlockedWorkers} retrieves currently blocked workers and the reason recorded for their block. This operation returns the first 65,535 blocked workers (the default for \code{pagesize}; access to additional blocked workers is available by specifying a \code{pagenumber} greater than 1.

\code{BlockWorkers()} and \code{block()} are aliases for \code{BlockWorker}. \code{UnblockWorkers()} and \code{unblock()} are aliases for \code{UnblockWorker}. \code{blockedworkers()} is an alias for \code{GetBlockedWorkers}.
}
\value{
\code{BlockWorker} and \code{UnblockWorker} return a dataframe containing the list of workers, reasons (for blocking/unblocking them), and whether the request to block/unblock each of them was valid.

\code{GetBlockedWorkers} returns a dataframe containing the list of blocked workers and the recorded reason for the block.
}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_BlockWorkerOperation.html}{API Reference: Block}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_UnblockWorkerOperation.html}{API Reference: Unblock}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GetBlockedWorkersOperation.html}{API Reference: GetBlockedWorkers}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
\dontrun{
BlockWorker("A1RO9UEXAMPLE",reasons="Did not follow photo categorization HIT instructions.")
GetBlockedWorkers()
UnblockWorker("A1RO9UEXAMPLE")
}
}
\keyword{Workers}