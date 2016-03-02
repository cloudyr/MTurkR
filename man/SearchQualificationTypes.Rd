\name{SearchQualificationTypes}
\alias{SearchQualificationTypes}
\alias{searchquals}
\title{Search QualificationTypes}
\description{Search for available QualificationTypes, including yours and others available on the MTurk system created by other requesters.}
\usage{
SearchQualificationTypes(query = NULL, only.mine = TRUE, only.requestable = FALSE, 
                         return.all = FALSE, pagenumber = "1", pagesize = "10", 
                         sortproperty = "Name", sortdirection = "Ascending", 
                         return.qual.dataframe = TRUE,
                         verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{query}{An optional character string containing a search query to be used to search among available QualificationTypes.}
  \item{only.mine}{A logical indicating whether only your QualificationTypes should be returned (the default). If \code{FALSE}, QualificationTypes created by all requesters will be returned.}
  \item{only.requestable}{A logical indicating whether only requestable QualificationTypes should be returned. Default is \code{FALSE}.}
  \item{return.all}{A logical indicating whether all QualificationTypes (as opposed to a specified page of the search results) should be returned. Default is \code{TRUE}.}
  \item{pagenumber}{An optional character string indicating which page of search results should be returned. Most users can ignore this.}
  \item{pagesize}{An optional character string indicating how many search results should be returned by each request, between 1 and 100. Most users can ignore this.}
  \item{sortproperty}{API currently only supports \dQuote{Name}. Most users can ignore this.}
  \item{sortdirection}{Either \dQuote{Ascending} or \dQuote{Descending}. Ignored if \code{return.all=TRUE}. Most users can ignore this.}
  \item{return.qual.dataframe}{A logical indicating whether the QualificationTypes should be returned as a data frame. Default is \code{TRUE}.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
Retrieve available QualificationTypes, optionally only those QualificationTypes created by you and/or those that meet specific search criteria specified in the \code{query} parameter. Given that the total number of QualificationTypes available from all requesters could be infinitely large, specifying both \code{only.mine=FALSE} and \code{return.all=FALSE} will be time-consuming and may cause memory problems.

\code{searchquals()} is an alias.
}
\value{A data frame containing the QualificationTypeId of the newly created QualificationType and other details as specified in the request.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_SearchQualificationTypesOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GetQualificationType}}

\code{\link{CreateQualificationType}}

\code{\link{UpdateQualificationType}}

\code{\link{DisposeQualificationType}}

\code{\link{SearchHITs}}
}
\examples{
\dontrun{
SearchQualificationTypes(only.mine = TRUE, 
                         return.all = TRUE)
SearchQualificationTypes(query = "MIT", 
                         only.mine = FALSE,
                         return.all = FALSE)
}
}
\keyword{Qualifications}