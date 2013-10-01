\name{GetQualificationRequests}
\alias{GetQualificationRequests}
\alias{qualrequests}
\title{Get Qualification Requests}
\description{Retrieve workers' requests for a QualificationType.}
\usage{
GetQualificationRequests(qual = NULL, return.all = TRUE, 
						pagenumber = "1", pagesize = "10", 
						sortproperty = "SubmitTime", sortdirection = "Ascending", 
						keypair = credentials(), print = getOption('MTurkR.print'), 
						log.requests = getOption('MTurkR.log'),
                        sandbox = getOption('MTurkR.sandbox'),
						return.qual.dataframe = TRUE,
                        validation.test = getOption('MTurkR.test'))
}
\arguments{
  \item{qual}{An optional character string containing a QualificationTypeId to which the search should be restricted. If none is supplied, requests made for all QualificationTypes are returned.}
  \item{return.all}{A logical indicating whether all QualificationRequestss (as opposed to a specified page of the search results) should be returned. Default is \code{TRUE}.}
  \item{pagenumber}{An optional character string indicating which page of search results should be returned. Most users can ignore this.}
  \item{pagesize}{An optional character string indicating how many search results should be returned by each request, between 1 and 100. Most users can ignore this.}
  \item{sortproperty}{Either \dQuote{SubmitTime} or \dQuote{QualificationTypeId}. Ignored if \code{return.all=TRUE}. Most users can ignore this.}
  \item{sortdirection}{Either \dQuote{Ascending} or \dQuote{Descending}. Ignored if \code{return.all=TRUE}. Most users can ignore this.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
  \item{return.qual.dataframe}{A logical indicating whether the QualificationTypes should be returned as a dataframe. Default is \code{TRUE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{
A function to retrieve pending Qualification Requests made by workers, either for a specified QualificationType or all QualificationTypes. Specifically, all active, custom QualificationTypes are visible to workers, and workers can request a QualificationType (e.g., when a HIT requires one they do not have). This function retrieves those requests so that they can be granted (with \code{\link{GrantQualification}}) or rejected (with \code{\link{RejectQualification}}).

\code{qualrequests()} is an alias.
}
\value{A dataframe containing the QualificationRequestId, WorkerId, and other information (e.g., Qualification Test results) for each request.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GetQualificationRequestsOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GrantQualification}}

\code{\link{RejectQualification}}
}
\examples{
\dontrun{
GetQualificationRequests()
GetQualificationRequests("2YCIA0RYNJ9262B1D82MPTUEXAMPLE")
}
}
\keyword{Qualifications}