\name{ExtendHIT}
\alias{ExtendHIT}
\alias{extend}
\title{Extend HIT}
\description{Extend the time remaining on a HIT or the number of assignments available for the HIT.}
\usage{
ExtendHIT(hit = NULL, hit.type = NULL, add.assignments = NULL, add.seconds = NULL, 
        unique.request.token = NULL, keypair = credentials(),
        print = getOption('MTurkR.print'), browser = getOption('MTurkR.browser'),
        log.requests = getOption('MTurkR.log'), sandbox = getOption('MTurkR.sandbox'),
        validation.test = getOption('MTurkR.test'))
}
\arguments{
  \item{hit}{An optional character string containing a HITId or a vector of character strings containing multiple HITIds.}
  \item{hit.type}{An optional character string containing a HITTypeId (or a vector of HITTypeIds).}
  \item{add.assignments}{An optional character string containing the number of assignmetns to add to the HIT. Must be between 1 and 1000000000.}
  \item{add.seconds}{An optional character string containing the amount of time to extend the HIT, in seconds (for example, returned by \code{\link{seconds}}). Must be between 1 hour (3600 seconds) and 365 days.}
  \item{unique.request.token}{An optional character string, included only for advanced users.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server. Default is \code{FALSE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{
A useful function for adding time and/or additional assignments to a HIT. If the HIT is already expired, this reactivates the HIT for the specified amount of time. If all assignments have already been submitted, this reactivates the HIT with the specified number of assignments and previously specified expiration. Must specify a HITId xor a HITTypeId. If multiple HITs or a HITTypeId are specified, each HIT is extended by the specified amount.

\code{extend()} is an alias.
}
\value{A dataframe containing the HITId, assignment increment, time increment, and whether each extension request was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_ExtendHITOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateHIT}}

\code{\link{ExpireHIT}}

\code{\link{DisableHIT}}

\code{\link{DisposeHIT}}
}
\examples{
\dontrun{
ExtendHIT(hit="2MQB727M0IGF304GJ16S1F4VE3AYDQ",add.assignments="20")
ExtendHIT(hit="2MQB727M0IGF304GJ16S1F4VE3AYDQ",add.time=seconds(days=1)))
}
}
\keyword{HITs}