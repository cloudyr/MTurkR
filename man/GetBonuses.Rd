\name{GetBonuses}
\alias{GetBonuses}
\alias{bonuses}
\title{Get Bonus Payments}
\description{Get details of bonuses paid to workers, by HIT, HITType, or Assignment.}
\usage{
GetBonuses(assignment = NULL, hit = NULL, hit.type = NULL, return.all = TRUE, 
           pagenumber = "1", pagesize = "100",
           verbose = getOption('MTurkR.verbose'), ...)
}
\arguments{
  \item{assignment}{An optional character string containing an AssignmentId whose bonuses should be returned.}
  \item{hit}{An optional character string containing a HITId whose bonuses should be returned.}
  \item{hit.type}{An optional character string containing a HITTypeId (or a vector of HITTypeIds) whose bonuses should be returned.}
  \item{return.all}{A logical indicating whether all HITs (as opposed to a specified page of the search results) should be returned. Default is \code{TRUE}. Note: This is (temporarily) ignored.}
  \item{pagenumber}{An optional character string indicating which page of search results should be returned. Most users can ignore this.}
  \item{pagesize}{An optional character string indicating how many search results should be returned by each request, between 1 and 100. Most users can ignore this.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MturkR.verbose')}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
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