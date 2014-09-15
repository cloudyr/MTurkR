\name{DisposeHIT}
\alias{DisposeHIT}
\alias{disposehit}
\title{Dispose HIT}
\description{Dispose of a HIT that is no longer needed. You can only dispose of HITs that are Reviewable, with all assignments either approved or rejected.}
\usage{
DisposeHIT( hit = NULL, hit.type = NULL, response.group = NULL, 
            verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{hit}{A character string containing a HITId or a vector of character strings containing multiple HITIds.}
  \item{hit.type}{An optional character string containing a HITTypeId (or a vector of HITTypeIds).}
  \item{response.group}{An optional character string specifying what details of each HIT to return of: \dQuote{Minimal}, \dQuote{HITQuestion}, \dQuote{HITDetail}, \dQuote{HITAssignmentSummary}. For more information, see \url{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_HITDataStructureArticle.html}.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
Dispose of a HIT (and its assignment data) when it is no longer needed. Must specify a HITId or a HITTypeId, but not both. HITTypeId uses the \code{\link{SearchHITs}} operation to locate HITs of the specified HITType before disposing of them.

\code{disposehit()} is an alias.
}
\value{A dataframe containing a list of HITs and whether the request to dispose of each of them was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_DisposeHITOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateHIT}}

\code{\link{ExtendHIT}}

\code{\link{ExpireHIT}}

\code{\link{DisableHIT}}
}
\examples{
\dontrun{
DisposeHIT(hit="2MQB727M0IGF304GJ16S1F4VE3AYDQ")
DisposeHIT(hit.type="2FFNCWYB49F9BBJWA4SJUNST5OFSOW")
}
}
\keyword{HITs}