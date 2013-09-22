\name{AccountBalance}
\alias{AccountBalance}
\alias{accountbalance}
\alias{getbalance}
\alias{SufficientFunds}
\title{Retrieve MTurk account balance}
\description{Retrieves the amount of money (in US Dollars) in your MTurk account. \code{SufficientFunds} provides a wrapper that checks whether your account has sufficient funds based upon specified characters of your HIT.}
\usage{
AccountBalance(	keypair = credentials(), print = TRUE, browser = FALSE, 
				log.requests = TRUE, sandbox = getOption('MTurkR.sandbox'),
                validation.test = FALSE)

SufficientFunds(amount, assignments = NULL, hits = NULL, 
				bonus.ct = NULL, bonus.amount = NULL, masters = FALSE, 
				turkfee = 0.1, turkmin = 0.005, mastersfee = 0.2, 
				keypair = credentials(), print = TRUE, log.requests = TRUE,
                sandbox = getOption('MTurkR.sandbox'), validation.test = FALSE)
}
\arguments{
  \item{amount}{Intended per-assignment payment amount.}
  \item{assignments}{Number of intended assignments (per HIT, if multiple HITs).}
  \item{hits}{Number of HITs.}
  \item{bonus.ct}{Number of intended bonuses.}
  \item{bonus.amount}{Amount of each bonus.}
  \item{masters}{A logical indicating whether MTurk Masters will be used. Default is \code{FALSE}.}
  \item{turkfee}{Amazon's fee as percentage of payments. Default is 10-percent (as 0.10).}
  \item{turkmin}{Amazon's minimum per-assignment fee. Default is $0.005.}
  \item{mastersfee}{Amazon's additional charge for use of MTurk Masters. Default is 20-percent (as 0.20).}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{
\code{AccountBalance} takes no substantive arguments. \code{SufficientFunds} is a wrapper for \code{AccountBalance} that accepts as inputs information about intended payments and bonuses to check whether your account has sufficient funds. If \code{sandbox=TRUE}, \code{AccountBalance} always returns \dQuote{$10,000.00}.

\code{accountbalance()} and \code{getbalance()} are aliases for \code{AccountBalance}.
}
\value{
Return value is a character string of the format \dQuote{$10,000.00.}
}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GetAccountBalanceOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
\dontrun{
AccountBalance()
SufficientFunds(amount=".25",assignments="50",hits="5")
SufficientFunds(bonus.ct="150",bonus.amount=".75")
}
}
