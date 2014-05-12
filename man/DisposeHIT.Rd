\name{DisposeHIT}
\alias{DisposeHIT}
\alias{disposehit}
\title{Dispose HIT}
\description{Dispose of a HIT that is no longer needed. You can only dispose of HITs that are Reviewable, with all assignments either approved or rejected.}
\usage{
DisposeHIT( hit = NULL, hit.type = NULL, response.group = NULL, 
            keypair = getOption('MTurkR.keypair'), print = getOption('MTurkR.print'),
            log.requests = getOption('MTurkR.log'),
            sandbox = getOption('MTurkR.sandbox'),
            validation.test = getOption('MTurkR.test'))
}
\arguments{
  \item{hit}{A character string containing a HITId or a vector of character strings containing multiple HITIds.}
  \item{hit.type}{An optional character string containing a HITTypeId (or a vector of HITTypeIds).}
  \item{response.group}{An optional character string specifying what details of each HIT to return of: \dQuote{Minimal}, \dQuote{HITQuestion}, \dQuote{HITDetail}, \dQuote{HITAssignmentSummary}. For more information, see \url{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_HITDataStructureArticle.html}.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server. Default is \code{FALSE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{
Dispose of a HIT (and its assignment data) when it is no longer needed. Must specify a HITId or a HITTypeId, but not both. HITTypeId uses the \code{\link{SearchHITs}} operation to locate HITs of the specified HITType before disposing of them.

\code{disposehit()} is an alias.
}
\value{A dataframe containing a list of HITs and whether the request to dispose of each of them was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_DisposeHITOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateHIT}}

\code{\link{ExtendHIT}}

\code{\link{ExpireHIT}}

\code{\link{DisableHIT}}
}
\examples{
\dontrun{
DisposeHIT(hit="2MQB727M0IGF304GJ16S1F4VE3AYDQ")
DisposeHIT(hit.type="2FFNCWYB49F9BBJWA4SJUNST5OFSOW")
}
}
\keyword{HITs}