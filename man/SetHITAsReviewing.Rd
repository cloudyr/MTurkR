\name{SetHITAsReviewing}
\alias{SetHITAsReviewing}
\alias{reviewing}
\title{Set HIT as \dQuote{Reviewing}}
\description{Update the status of a HIT, from \dQuote{Reviewable} to \dQuote{Reviewing} or the reverse.}
\usage{
SetHITAsReviewing(	hit = NULL, hit.type = NULL, revert = FALSE, 
					keypair = credentials(), print = getOption('MTurkR.print'),
                    browser = getOption('MTurkR.browser'), 
					log.requests = getOption('MTurkR.log'),
                    sandbox = getOption('MTurkR.sandbox'),
                    validation.test = getOption('MTurkR.test'))
}
\arguments{
  \item{hit}{An optional chracter character string containing a HITId, or a vector character strings containing HITIds, whose status are to be changed.}
  \item{hit.type}{An optional character string specifying a HITTypeId (or a vector of HITTypeIds), all the HITs of which should be set as \dQuote{Reviewing} (or the reverse).}
  \item{revert}{An optional logical to revert the HIT from \dQuote{Reviewing} to \dQuote{Reviewable}.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{
A function to change the status of one or more HITs (or all HITs of a given HITType) to \dQuote{Reviewing} or the reverse. This affects what HITs are returned by \code{\link{GetReviewableHITs}}. Must specify a HITId xor a HITTypeId.

\code{reviewing()} is an alias.
}
\value{A dataframe containing HITId, status, and whether the request to change the status of each was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_SetHITAsReviewingOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GetReviewableHITs}}
}
\examples{
\dontrun{
SetHITAsReviewing("2MQB727M0IGF304GJ16S1F4VE3AYDQ")
}
}
\keyword{HITs}