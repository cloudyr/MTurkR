\name{GetStatistic}
\alias{GetStatistic}
\alias{statistic}
\alias{RequesterReport}
\alias{GetWorkerStatistic}
\alias{workerstatistic}
\alias{WorkerReport}
\title{MTurk Worker and Requester Statistics}
\description{Get a requester statistic or a statistic for a particular worker. \code{RequesterReport} and \code{WorkerReport} provide wrappers that return all available statistics.}
\usage{
GetStatistic(	statistic, period = "LifeToDate", count = NULL,
				response.group = NULL, keypair = credentials(), print = TRUE, 
				browser = FALSE, log.requests = TRUE, sandbox = getOption('MTurkR.sandbox'),
				validation.test = FALSE)
RequesterReport(period = "LifeToDate", keypair = credentials(), 
				log.requests = TRUE, sandbox = getOption('MTurkR.sandbox'), print = TRUE)

GetWorkerStatistic(	worker, statistic, period = "LifeToDate", count = NULL,
					response.group = NULL, keypair = credentials(), print = TRUE,
					browser = FALSE, log.requests = TRUE, sandbox = getOption('MTurkR.sandbox'),
					validation.test = FALSE)
WorkerReport(	worker, period = "LifeToDate", 
				keypair = credentials(), log.requests = TRUE, 
				sandbox = getOption('MTurkR.sandbox'), print = TRUE)
}
\arguments{
  \item{worker}{A character string containing a WorkerId.}
  \item{statistic}{A character string containing the name of a statistic. Statistics can be retrieved from \code{\link{ListStatistics}}.}
  \item{period}{One of: \dQuote{OneDay}, \dQuote{SevenDays}, \dQuote{ThirtyDays}, \dQuote{LifeToDate}. Default is \dQuote{LifeToDate}.}
  \item{count}{If \code{period="OneDay"}, the number of days to return. Default is 1 (the most recent day).}
  \item{response.group}{An optional character string (or vector of character strings) specifying what details to return of \dQuote{Request}, \dQuote{Minimal}, or \dQuote{Parameters}. For more information, see \url{http://docs.aws.amazon.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_CommonParametersArticle.html}.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{
Retrieve a specific requester or worker statistic. The list of available statistics can be retrieved by calling \code{\link{ListStatistics}}. Useful for monitoring workers or one's own use of the requester system.

\code{statistic()} is an alias for \code{GetStatistic}. \code{workerstatistic()} is an alias for \code{GetWorkerStatistic}.
}
\value{A dataframe containing the requested Statistic (and WorkerId, if \code{GetWorkerStatistic} or \code{WorkerReport} are called), and the value thereof. \code{GetStatistic} and \code{GetWorkerStatistic} return only the requested statistic. \code{RequesterReport} and \code{WorkerReport} return all of the requester and worker statistics, respectively, that are available in \code{\link{ListStatistics}}.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GetRequesterStatisticOperation.html}{API Reference: Requester Statistics}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GetRequesterWorkerStatisticOperation.html}{API Reference: Worker Statistics}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{ListStatistics}}
}
\examples{
\dontrun{
GetStatistic("NumberHITsSubmitted","OneDay")
RequesterReport("ThirtyDays")
GetWorkerStatistic("A1RO9UJNWXMU65","PercentHITsApproved","LifeToDate")
WorkerReport("A1RO9UJNWXMU65","SevenDays")
}
}
\keyword{Workers}