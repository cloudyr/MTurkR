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
GetStatistic(statistic, period = "LifeToDate", count = NULL,
             response.group = NULL, 
             verbose = getOption('MTurkR.verbose', TRUE), ...)
RequesterReport(period = "LifeToDate", 
                verbose = getOption('MTurkR.verbose', TRUE), ...)

GetWorkerStatistic(worker, statistic, period = "LifeToDate", count = NULL,
                   response.group = NULL, 
                   verbose = getOption('MTurkR.verbose', TRUE), ...)
WorkerReport(worker, period = "LifeToDate", 
             verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{worker}{A character string containing a WorkerId.}
  \item{statistic}{A character string containing the name of a statistic. Statistics can be retrieved from \code{\link{ListStatistics}}.}
  \item{period}{One of: \dQuote{OneDay}, \dQuote{SevenDays}, \dQuote{ThirtyDays}, \dQuote{LifeToDate}. Default is \dQuote{LifeToDate}.}
  \item{count}{If \code{period="OneDay"}, the number of days to return. Default is 1 (the most recent day).}
  \item{response.group}{An optional character string (or vector of character strings) specifying what details to return of \dQuote{Request}, \dQuote{Minimal}, or \dQuote{Parameters}. For more information, see \href{http://docs.aws.amazon.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_CommonParametersArticle.html}{Common Parameters}.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
Retrieve a specific requester or worker statistic. The list of available statistics can be retrieved by calling \code{\link{ListStatistics}}. Useful for monitoring workers or one's own use of the requester system.

\code{statistic()} is an alias for \code{GetStatistic}. \code{workerstatistic()} is an alias for \code{GetWorkerStatistic}.
}
\value{A data frame containing the Date, Value, and Statistic (and WorkerId, if \code{GetWorkerStatistic} or \code{WorkerReport} are called), and the value thereof. \code{GetStatistic} and \code{GetWorkerStatistic} return only information about the requested statistic as a data.frame. \code{RequesterReport} and \code{WorkerReport} return all of the requester and worker statistics, respectively, that are available in \code{\link{ListStatistics}}.}
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
GetStatistic("NumberHITsCompleted","OneDay")
RequesterReport("ThirtyDays")
GetWorkerStatistic("A1RO9UJNWXMU65","PercentHITsApproved","LifeToDate")
WorkerReport("A1RO9UJNWXMU65","SevenDays")
}
}
\keyword{Workers}