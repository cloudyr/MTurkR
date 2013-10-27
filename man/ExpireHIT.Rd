\name{ExpireHIT}
\alias{ExpireHIT}
\alias{expire}
\title{Expire HIT}
\description{Force a HIT to expire immediately, as opposed to at its prespecified expiration time. Expired HITs can be extended with the \code{\link{ExtendHIT}} operation.}
\usage{
ExpireHIT(hit = NULL, hit.type = NULL, keypair = credentials(), 
		print = getOption('MTurkR.print'), browser = getOption('MTurkR.browser'),
        log.requests = getOption('MTurkR.log'), sandbox = getOption('MTurkR.sandbox'),
        validation.test = getOption('MTurkR.test'))
}
\arguments{
  \item{hit}{A character string containing a HITId or a vector of character strings containing multiple HITIds.}
  \item{hit.type}{An optional character string containing a HITTypeId (or a vector of HITTypeIds).}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{
A function to (prematurely) expire a HIT (or multiple HITs), thereby preventing any additional assignments from being completed. Pending assignments can still be submitted. An expired HIT can be reactived by adding additional time to its expiration using \code{\link{ExtendHIT}}.

\code{expire()} is an alias.
}
\value{A dataframe containing the HITId(s) and whether each expiration request was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_ForceExpireHITOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateHIT}}

\code{\link{ExtendHIT}}

\code{\link{DisableHIT}}

\code{\link{DisposeHIT}}
}
\examples{
\dontrun{
ExpireHIT(hit="2MQB727M0IGF304GJ16S1F4VE3AYDQ")
}
}
\keyword{HITs}