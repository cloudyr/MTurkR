\name{GetHITsForQualificationType}
\alias{GetHITsForQualificationType}
\alias{gethitsbyqual}
\title{Get HITs by Qualification}
\description{Retrieve HITs according to the QualificationTypes that are required to complete those HITs.}
\usage{
GetHITsForQualificationType(qual, response.group = NULL, return.all = TRUE, 
							pagenumber = 1, pagesize = 100, 
							keypair = credentials(), print = getOption('MTurkR.print'), 
							log.requests = getOption('MTurkR.log'),
                            sandbox = getOption('MTurkR.sandbox'), 
							return.hit.dataframe = TRUE,
                            validation.test = getOption('MTurkR.test'))
}
\arguments{
  \item{qual}{A character string containing a QualificationTypeId.}
  \item{response.group}{An optional character string specifying what details of each HIT to return of: \dQuote{Minimal}, \dQuote{Request}. For more information, see \url{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_HITDataStructureArticle.html}.}
  \item{return.all}{A logical indicating whether all QualificationTypes (as opposed to a specified page of the search results) should be returned. Default is \code{TRUE}.}
  \item{pagenumber}{An optional character string indicating which page of search results should be returned. Most users can ignore this.}
  \item{pagesize}{An optional character string indicating how many search results should be returned by each request, between 1 and 100. Most users can ignore this.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
  \item{return.hit.dataframe}{A logical indicating whether the HIT dataframe should be returned. Default is \code{TRUE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{
A function to retrieve HITs that require the specified QualificationType.

\code{gethitsbyqual()} is an alias.
}
\value{A data frame containing the HITId and other requested characteristics of the qualifying HITs.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GetHITsForQualificationTypeOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GetHIT}}

\code{\link{SearchHITs}}
}
\examples{
\dontrun{
q <- ListQualificationTypes()[7,2] # Location requirement
GetHITsForQualificationType(q)
}
}
\keyword{HITs}
\keyword{Qualifications}