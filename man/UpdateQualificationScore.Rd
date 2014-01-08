\name{UpdateQualificationScore}
\alias{UpdateQualificationScore}
\alias{updatequalscore}
\title{Update a worker's score for a QualificationType}
\description{Update a worker's score for a QualificationType that you created. Scores for built-in QualificationTypes (e.g., location, worker statistics) cannot be updated.}
\usage{
UpdateQualificationScore(qual, workers, values = NULL, increment = NULL, 
                        keypair = credentials(), print = getOption('MTurkR.print'),
                        browser = getOption('MTurkR.browser'), 
                        log.requests = getOption('MTurkR.log'),
                        sandbox = getOption('MTurkR.sandbox'),
                        validation.test = getOption('MTurkR.test'))
}
\arguments{
  \item{qual}{A character string containing a QualificationTypeId.}
  \item{workers}{A character string containing a WorkerId, or a vector of character strings containing multiple WorkerIds.}
  \item{values}{A character string containing an integer value to be assigned to the worker, or a vector of character strings containing integer values to be assigned to each worker (and thus must have length equal to the number of workers).}
  \item{increment}{An optional character string specifying, in lieu of \dQuote{values}, the amount that each worker's current QualfiicationScore should be increased.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{
A function to update the Qualification score assigned to one or more workers for the specified custom QualificationType. The simplest use is to specify a QualificationTypeId, a WorkerId, and a value to be assigned to the worker. Scores for multiple workers can be updated in one request. 

Additionally, the \code{increment} parameter allows you to increase (or decrease) each of the specified workers scores by the specified amount. This might be useful, for example, to keep a QualificationType that records how many of a specific style of HIT a worker has completed and increase the value of each worker's score by 1 after they complete a HIT.

\code{updatequalscore()} is an alias.
}
\value{A dataframe containing the QualificationTypeId, WorkerId, Qualification score, and whether the request to update each was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_UpdateQualificationScoreOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GetQualificationScore}}

\code{\link{GetQualifications}}
}
\examples{
\dontrun{
UpdateQualificationScore("2YCIA0RYNJ9262B1D82MPTUEXAMPLE","A1RO9UJNWXMU65",values="95")
UpdateQualificationScore("2YCIA0RYNJ9262B1D82MPTUEXAMPLE","A1RO9UJNWXMU65",increment="1")
}
}
\keyword{Qualifications}