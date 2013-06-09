\name{DisableHIT}
\alias{DisableHIT}
\alias{disable}
\title{Disable HIT}
\description{Disabling a HIT is probably not what you want to do. DisableHIT automatically removes the HIT from the MTurk server, approves (and thus pays for) all submitted and pending assignments, and then permanently deletes all assignment data.}
\usage{
DisableHIT(	hit = NULL, hit.type = NULL, response.group = NULL, 
			keypair = credentials(), print = TRUE, browser = FALSE, 
			log.requests = TRUE, sandbox = FALSE)
}
\arguments{
  \item{hit}{A character string containing a HITId or a vector of character strings containing multiple HITIds.}
  \item{hit.type}{An optional character string containing a HITTypeId.}
  \item{response.group}{An optional character string specifying what details of each HIT to return of: \dQuote{Minimal}, \dQuote{HITQuestion}, \dQuote{HITDetail}, \dQuote{HITAssignmentSummary}. For more information, see \url{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_HITDataStructureArticle.html}.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
}
\details{
Disable a HIT (and its assignment data). This is a somewhat risky function because it automatically approves all pending assignments and then disposes of everything. \code{\link{DisposeHIT}} is probably what most users will use to delete HIT and assignment data that is no longer needed.

\code{disable()} is an alias.
}
\value{A dataframe containing a list of HITs and whether the request to disable each of them was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_DisableHITOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateHIT}}

\code{\link{ExtendHIT}}

\code{\link{ExpireHIT}}

\code{\link{DisposeHIT}}
}
\examples{
\dontrun{DisableHIT(hit="2MQB727M0IGF304GJ16S1F4VE3AYDQ")}
\dontrun{DisableHIT(hit.type="2FFNCWYB49F9BBJWA4SJUNST5OFSOW")}
}
\keyword{HITs}