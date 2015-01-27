\name{GetHITsForQualificationType}
\alias{GetHITsForQualificationType}
\alias{gethitsbyqual}
\title{Get HITs by Qualification}
\description{Retrieve HITs according to the QualificationTypes that are required to complete those HITs.}
\usage{
GetHITsForQualificationType(qual, response.group = NULL, return.all = TRUE, 
                            pagenumber = 1, pagesize = 100, 
                            verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{qual}{A character string containing a QualificationTypeId.}
  \item{response.group}{An optional character string specifying what details of each HIT to return of: \dQuote{Minimal}, \dQuote{Request}. For more information, see \href{http://docs.aws.amazon.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_CommonParametersArticle.html}{Common Parameters} and \href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_HITDataStructureArticle.html}{HIT Data Structure}.}
  \item{return.all}{A logical indicating whether all QualificationTypes (as opposed to a specified page of the search results) should be returned. Default is \code{TRUE}.}
  \item{pagenumber}{An optional character string indicating which page of search results should be returned. Most users can ignore this.}
  \item{pagesize}{An optional character string indicating how many search results should be returned by each request, between 1 and 100. Most users can ignore this.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
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