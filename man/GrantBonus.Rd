\name{GrantBonus}
\alias{GrantBonus}
\alias{bonus}
\alias{paybonus}
\title{Pay Bonus to Worker}
\description{Pay a bonus to one or more workers. This function spends money from your MTurk account and will fail if insufficient funds are available.}
\usage{
GrantBonus(workers, assignments, amounts, reasons, 
           unique.request.token = NULL, 
           verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{workers}{A character string containing a WorkerId, or a vector of character strings containing multiple WorkerIds.}
  \item{assignments}{A character string containing an AssignmentId for an assignment performed by that worker, or a vector of character strings containing the AssignmentId for an assignment performed by each of the workers specified in \code{workers}.}
  \item{amounts}{A character string containing an amount (in U.S. Dollars) to bonus the worker(s), or a vector (of length equal to the number of workers) of character strings containing the amount to be paid to each worker.}
  \item{reasons}{A character string containing a reason for bonusing the worker(s), or a vector (of length equal to the number of workers) of character strings containing the reason to bonus each worker. The reason is visible to each worker and is sent via email. Newlines can be specified with \code{\\n} and tabs can be specified with \code{\\t} in the message body.}
  \item{unique.request.token}{An optional character string, included only for advanced users. It can be used to prevent resending a bonus. A bonus will not be granted if a bonus was previously granted (within a short time window) using the same \code{unique.request.token}.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
A simple function to grant a bonus to one or more workers. The function is somewhat picky in that it requires a WorkerId, the AssignmentId for an assignment that worker has completed, an amount, and a reason for the bonus, for each bonus to be paid. Optionally, the amount and reason can be specified as single (character string) values, which will be used for each bonus.

\code{bonus()} and \code{paybonus()} are aliases.
}
\value{A data frame containing the WorkerId, AssignmentId, amount, reason, and whether each request to bonus was valid.}
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
# Grant a single bonus
a <- "A1RO9UEXAMPLE"
b <- "26XXH0JPPSI23H54YVG7BKLEXAMPLE"
c <- ".50"
d <- "Thanks for your great work on my HITs!\nHope to work with you, again!"
GrantBonus(workers=a, assignments=b, amounts=c, reasons=d)
}
\dontrun{
# Grant bonuses to multiple workers
a <- c("A1RO9EXAMPLE1","A1RO9EXAMPLE2","A1RO9EXAMPLE3")
b <- 
c("26XXH0JPPSI23H54YVG7BKLEXAMPLE1",
"26XXH0JPPSI23H54YVG7BKLEXAMPLE2",
"26XXH0JPPSI23H54YVG7BKLEXAMPLE3")
c <- c(".50",".10",".25")
d <- "Thanks for your great work on my HITs!"
GrantBonus(workers=a, assignments=b, amounts=c, reasons=d)
}
}
\keyword{Workers}
