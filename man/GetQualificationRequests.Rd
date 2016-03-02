\name{GetQualificationRequests}
\alias{GetQualificationRequests}
\alias{qualrequests}
\title{Get Qualification Requests}
\description{Retrieve workers' requests for a QualificationType.}
\usage{
GetQualificationRequests(qual = NULL, return.all = TRUE, 
                        pagenumber = "1", pagesize = "10", 
                        sortproperty = "SubmitTime", sortdirection = "Ascending", 
                        return.qual.dataframe = TRUE,
                        verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{qual}{An optional character string containing a QualificationTypeId to which the search should be restricted. If none is supplied, requests made for all QualificationTypes are returned.}
  \item{return.all}{A logical indicating whether all QualificationRequestss (as opposed to a specified page of the search results) should be returned. Default is \code{TRUE}.}
  \item{pagenumber}{An optional character string indicating which page of search results should be returned. Most users can ignore this.}
  \item{pagesize}{An optional character string indicating how many search results should be returned by each request, between 1 and 100. Most users can ignore this.}
  \item{sortproperty}{Either \dQuote{SubmitTime} or \dQuote{QualificationTypeId}. Ignored if \code{return.all=TRUE}. Most users can ignore this.}
  \item{sortdirection}{Either \dQuote{Ascending} or \dQuote{Descending}. Ignored if \code{return.all=TRUE}. Most users can ignore this.}
  \item{return.qual.dataframe}{A logical indicating whether the QualificationTypes should be returned as a data frame. Default is \code{TRUE}.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
A function to retrieve pending Qualification Requests made by workers, either for a specified QualificationType or all QualificationTypes. Specifically, all active, custom QualificationTypes are visible to workers, and workers can request a QualificationType (e.g., when a HIT requires one they do not have). This function retrieves those requests so that they can be granted (with \code{\link{GrantQualification}}) or rejected (with \code{\link{RejectQualification}}).

\code{qualrequests()} is an alias.
}
\value{A data frame containing the QualificationRequestId, WorkerId, and other information (e.g., Qualification Test results) for each request.}
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