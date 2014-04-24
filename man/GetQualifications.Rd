\name{GetQualifications}
\alias{GetQualifications}
\alias{getquals}
\title{Get Qualifications}
\description{Get all Qualifications of a particular QualificationType assigned to Workers.}
\usage{
GetQualifications(    qual, status = NULL, return.all = TRUE, 
                    pagenumber = 1, pagesize = 100, keypair = getOption('MTurkR.keypair'),
                    print = getOption('MTurkR.print'),
                    log.requests = getOption('MTurkR.log'),
                    sandbox = getOption('MTurkR.sandbox'),
                    return.qual.dataframe = TRUE,
                    validation.test = getOption('MTurkR.test'))
}
\arguments{
  \item{qual}{A character string containing a QualificationTypeId for a custom (i.e., not built-in) QualificationType.}
  \item{status}{An optional character string specifying whether only \dQuote{Granted} or \dQuote{Revoked} Qualifications should be returned.}
  \item{return.all}{A logical indicating whether all Qualifications (as opposed to a specified page of the search results) should be returned. Default is \code{TRUE}.}
  \item{pagenumber}{An optional character string indicating which page of search results should be returned. Most users can ignore this.}
  \item{pagesize}{An optional character string indicating how many search results should be returned by each request, between 1 and 100. Most users can ignore this.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server. Default is \code{FALSE}.}
  \item{return.qual.dataframe}{A logical specifying whether the Qualification dataframe should be returned. Default is \code{TRUE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{
A function to retrieve Qualifications granted for the specified QualificationType. To retrieve a specific Qualification score (e.g., for one worker), use \code{\link{GetQualificationScore}}.

A practical use for this is with automatically granted QualificationTypes. After workers request and recieve an automatically granted Qualification that is tied to one or more HITs, \code{GetQualifications} can be used to retrieve the WorkerId's for workers that are actively working on those HITs (even before they have submitted an assignment).

\code{getquals()} is an alias.
}
\value{A dataframe containing the QualificationTypeId, WorkerId, and Qualification scores of workers assigned the Qualification.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GetQualificationsForQualificationTypeOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GetQualificationScore}}

\code{\link{UpdateQualificationScore}}
}
\examples{
\dontrun{
GetQualifications("2YCIA0RYNJ9262B1D82MPTUEXAMPLE")
GetQualifications("2YCIA0RYNJ9262B1D82MPTUEXAMPLE",status="Revoked")
}
}
\keyword{Qualifications}
