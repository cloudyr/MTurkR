\name{GetQualificationScore}
\alias{GetQualificationScore}
\alias{qualscore}
\title{Get a Worker's Qualification Score}
\description{Get a Worker's score for a specific Qualification. You can only retrieve scores for custom QualificationTypes. Scores for built-in QualificationTypes should be retrieved with \code{\link{GetWorkerStatistic}}.}
\usage{
GetQualificationScore(qual, workers, keypair = getOption('MTurkR.keypair'), 
                      print = getOption('MTurkR.print'),
                      log.requests = getOption('MTurkR.log'),
                      sandbox = getOption('MTurkR.sandbox'),
                      validation.test = getOption('MTurkR.test'))
}
\arguments{
  \item{qual}{A character string containing a QualificationTypeId for a custom QualificationType.}
  \item{workers}{A character string containing a WorkerId, or a vector of character strings containing multiple WorkerIds, whose Qualification Scores you want to retrieve.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{
A function to retrive one or more scores for a specified QualificationType. To retrieve all Qualifications of a given QualificationType, use \code{\link{GetQualifications}} instead.

\code{qualscore()} is an alias.
}
\value{A dataframe containing the WorkerId, QualificationTypeId, and the Qualification score for each request.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GetQualificationScoreOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{UpdateQualificationScore}}

\code{\link{GetQualifications}}
}
\examples{
\dontrun{
GetQualificationScore("2YCIA0RYNJ9262B1D82MPTUEXAMPLE","A1RO9UJNWXMU65")
}
}
\keyword{Qualifications}