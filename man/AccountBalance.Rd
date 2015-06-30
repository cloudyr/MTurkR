\name{AccountBalance}
\alias{AccountBalance}
\alias{accountbalance}
\alias{getbalance}
\alias{SufficientFunds}
\title{Retrieve MTurk account balance}
\description{Retrieves the amount of money (in US Dollars) in your MTurk account. \code{SufficientFunds} provides a wrapper that checks whether your account has sufficient funds based upon specified characters of your HIT.}
\usage{
AccountBalance(verbose = getOption('MTurkR.verbose', TRUE), ...)

SufficientFunds(amount = NULL, assignments = NULL, hits = NULL, 
                bonus.ct = NULL, bonus.amount = NULL, masters = FALSE, 
                turkfee = 0.2, turkmin = 0.01, mastersfee = 0.05, ...)
}
\arguments{
  \item{amount}{Intended per-assignment payment amount.}
  \item{assignments}{Number of intended assignments (per HIT, if multiple HITs).}
  \item{hits}{Number of HITs.}
  \item{bonus.ct}{Number of intended bonuses.}
  \item{bonus.amount}{Amount of each bonus.}
  \item{masters}{A logical indicating whether MTurk Masters will be used. Default is \code{FALSE}.}
  \item{turkfee}{Amazon's fee as percentage of payments. Default is 20-percent (as 0.20). Note, however, that MTurk charges an additional 20-percent if the number of \code{assignments} is greater than or equal to 10. This is factored in automatically}
  \item{turkmin}{Amazon's minimum per-assignment fee. Default is $0.01.}
  \item{mastersfee}{Amazon's additional charge for use of MTurk Masters. Default is 5-percent (as 0.05).}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
\code{AccountBalance} takes no substantive arguments. \code{SufficientFunds} is a wrapper for \code{AccountBalance} that accepts as inputs information about intended payments and bonuses to check whether your account has sufficient funds. If \code{sandbox=TRUE}, \code{AccountBalance} always returns \dQuote{$10,000.00}.

\code{accountbalance()} and \code{getbalance()} are aliases for \code{AccountBalance}.
}
\value{Return value is an object of class \dQuote{MTurkResponse}, including an additional character string (\code{balance}) containing the balance of the account in US Dollars. Note: object is returned invisibly.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GetAccountBalanceOperation.html}{API Reference}

\href{https://requester.mturk.com/pricing}{MTurk Pricing Structure}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
\dontrun{
AccountBalance()
SufficientFunds(amount = ".25", assignments = "50", hits = "5")
SufficientFunds(bonus.ct = "150", bonus.amount = ".75")
}
}
