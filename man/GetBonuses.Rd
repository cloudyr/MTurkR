\name{GetBonuses}
\alias{GetBonuses}
\alias{bonuses}
\title{Get Bonus Payments}
\description{Get details of bonuses paid to workers, by HIT, HITType, or Assignment.}
\usage{
GetBonuses(assignment = NULL, hit = NULL, hit.type = NULL, return.all = TRUE, 
            pagenumber = "1", pagesize = "100", keypair = getOption('MTurkR.keypair'), 
            print = getOption('MTurkR.print'), browser = getOption('MTurkR.browser'), 
            log.requests = getOption('MTurkR.log'),
            sandbox = getOption('MTurkR.sandbox'), 
            validation.test = getOption('MTurkR.test'))
}
\arguments{
  \item{assignment}{An optional character string containing an AssignmentId whose bonuses should be returned.}
  \item{hit}{An optional character string containing a HITId whose bonuses should be returned.}
  \item{hit.type}{An optional character string containing a HITTypeId (or a vector of HITTypeIds) whose bonuses should be returned.}
  \item{return.all}{A logical indicating whether all HITs (as opposed to a specified page of the search results) should be returned. Default is \code{TRUE}. Note: This is (temporarily) ignored.}
  \item{pagenumber}{An optional character string indicating which page of search results should be returned. Most users can ignore this.}
  \item{pagesize}{An optional character string indicating how many search results should be returned by each request, between 1 and 100. Most users can ignore this.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{
Retrieve bonuses previously paid to a specified HIT, Assignment, or HITType.

\code{bonuses()} is an alias.
}
\value{A dataframe containing the details of each bonus, specifically: AssignmentId, WorkerId, Amount, CurrencyCode, FormattedPrice, Reason, and GrantTime.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GetBonusPaymentsOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GrantBonus}}
}
\examples{
\dontrun{
GetBonuses(hit="2MQB727M0IGF304GJ16S1F4VE3AYDQ")
GetBonuses(assignment="26XXH0JPPSI23H54YVG7BKLO82DHNU")
}
}
\keyword{Workers}