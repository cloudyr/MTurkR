\name{GetReviewableHITs}
\alias{GetReviewableHITs}
\alias{reviewable}
\title{Get Reviewable HITs}
\description{Get HITs that are currently reviewable.}
\usage{
GetReviewableHITs(hit.type = NULL, status = NULL, response.group = "Minimal", 
                  return.all = TRUE, pagenumber = "1", pagesize = "10", 
                  sortproperty = "Enumeration", sortdirection = "Ascending", 
                  verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{hit.type}{An optional character string containing a HITTypeId to consider when looking for reviewable HITs.}
  \item{status}{An optional character string of either \dQuote{Reviewable} or \dQuote{Reviewing} limiting the search to HITs of with either status.}
  \item{response.group}{A character string specifying what details of each HIT to return. API currently only supports \dQuote{Minimal}. For more information, see \href{http://docs.aws.amazon.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_CommonParametersArticle.html}{Common Parameters} and \href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_HITDataStructureArticle.html}{HIT Data Structure}.}
  \item{return.all}{A logical indicating whether all QualificationTypes (as opposed to a specified page of the search results) should be returned. Default is \code{TRUE}.}
  \item{pagenumber}{An optional character string indicating which page of search results should be returned. Most users can ignore this.}
  \item{pagesize}{An optional character string indicating how many search results should be returned by each request, between 1 and 100. Most users can ignore this.}
  \item{sortproperty}{One of \dQuote{Title}, \dQuote{Reward}, \dQuote{Expiration}, \dQuote{CreationTime}, \dQuote{Enumeration}. Ignored if \code{return.all=TRUE}. Most users can ignore this.}
  \item{sortdirection}{Either \dQuote{Ascending} or \dQuote{Descending}. Ignored if \code{return.all=TRUE}. Most users can ignore this.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{A simple function to return the HITIds of HITs currently in \dQuote{Reviewable} or \dQuote{Reviewing} status. To retrieve additional details about each of these HITs, see \code{\link{GetHIT}}. This is an alternative to \code{\link{SearchHITs}}.

\code{reviewable()} is an alias.
}
\value{A data frame containing only a column of HITIds.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GetReviewableHITsOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GetHIT}}

\code{\link{GetHITsForQualificationType}}

\code{\link{SearchHITs}}
}
\examples{
\dontrun{
GetReviewableHITs()
}
}
\keyword{HITs}