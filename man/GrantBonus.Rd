\name{GrantBonus}
\alias{GrantBonus}
\alias{bonus}
\alias{paybonus}
\title{Pay Bonus to Worker}
\description{Pay a bonus to one or more workers. This function spends money from your MTurk account and will fail if insufficient funds are available.}
\usage{
GrantBonus(	workers, assignments, amounts, reasons, 
			keypair = credentials(), print = getOption('MTurkR.print'),
            browser = getOption('MTurkR.browser'), 
			log.requests = getOption('MTurkR.log'),
            sandbox = getOption('MTurkR.sandbox'),
            validation.test = getOption('MTurkR.test'))
}
\arguments{
  \item{workers}{A character string containing a WorkerId, or a vector of character strings containing multiple WorkerIds.}
  \item{assignments}{A character string containing an AssignmentId for an assignment performed by that worker, or a vector of character strings containing the AssignmentId for an assignment performed by each of the workers specified in \code{workers}.}
  \item{amounts}{A character string containing an amount (in U.S. Dollars) to bonus the worker(s), or a vector (of length equal to the number of workers) of character strings containing the amount to be paid to each worker.}
  \item{reasons}{A character string containing a reason for bonusing the worker(s), or a vector (of length equal to the number of workers) of character strings containing the reason to bonus each worker. The reason is visible to each worker.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{
A simple function to grant a bonus to one or more workers. The function is somewhat picky in that it requires a WorkerId, the AssignmentId for an assignment that worker has completed, an amount, and a reason for the bonus, for each bonus to be paid. Optionally, the amount and reason can be specified as single (character string) values, which will be used for each bonus.

\code{bonus()} and \code{paybonus()} are aliases.
}
\value{A dataframe containing the WorkerId, AssignmentId, amount, reason, and whether each request to bonus was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GrantBonusOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GetBonuses}}
}
\examples{
\dontrun{
a <- "A1RO9UEXAMPLE"
b <- "26XXH0JPPSI23H54YVG7BKLEXAMPLE"
c <- ".50"
d <- "Thanks for your great work on my HITs!"
GrantBonus(workers=a,assignments=b,amounts=c,reasons=d)
}
\dontrun{
a <- c("A1RO9EXAMPLE1","A1RO9EXAMPLE2","A1RO9EXAMPLE3")
b <- 
c("26XXH0JPPSI23H54YVG7BKLEXAMPLE1",
"26XXH0JPPSI23H54YVG7BKLEXAMPLE2",
"26XXH0JPPSI23H54YVG7BKLEXAMPLE3")
c <- c(".50",".10",".25")
d <- "Thanks for your great work on my HITs!"
GrantBonus(workers=a,assignments=b,amounts=c,reasons=d)
}
}
\keyword{Workers}