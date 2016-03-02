\name{GetReviewResultsForHIT}
\alias{GetReviewResultsForHIT}
\alias{reviewresults}
\title{Get ReviewPolicy Results for a HIT}
\description{Get HIT- and/or Assignment-level ReviewPolicy Results for a HIT}
\usage{
GetReviewResultsForHIT(hit, assignment = NULL, policy.level = NULL,
                       retrieve.results = TRUE, retrieve.actions = TRUE,
                       return.all = FALSE, pagenumber = 1, pagesize = 400,
                       verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{hit}{A character string containing a HITId.}
  \item{assignment}{An optional character string containing an AssignmentId. If specified, only results pertaining to that assignment will be returned.}
  \item{policy.level}{Either \code{HIT} or \code{Assignment}. If \code{NULL} (the default), all data for both policy levels is retrieved.}
  \item{retrieve.results}{Optionally retrieve ReviewResults. Default is \code{TRUE}.}
  \item{retrieve.actions}{Optionally retrieve ReviewActions. Default is \code{TRUE}.}
  \item{return.all}{A logical indicating whether all Qualifications (as opposed to a specified page of the search results) should be returned. Default is \code{FALSE}.}
  \item{pagenumber}{An optional character string indicating which page of search results should be returned. Most users can ignore this.}
  \item{pagesize}{An optional character string indicating how many search results should be returned by each request, between 1 and 400. Most users can ignore this.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{A simple function to return the results of a ReviewPolicy. This is intended only for advanced users, who should reference MTurk documentation for further information or see the notes in \code{\link{GenerateHITReviewPolicy}}.

\code{reviewresults} is an alias.
}
\value{A four-element list containing up to four named data frames, depending on what ReviewPolicy (or ReviewPolicies) were attached to the HIT and whether results or actions are requested: \code{AssignmentReviewResult}, \code{AssignmentReviewAction}, \code{HITReviewResult}, and/or \code{HITReviewAction}.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GetReviewResultsForHitOperation.html}{API Reference}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_ReviewPoliciesArticle.html}{API Reference (ReviewPolicies)}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_HITReviewPolicyDataStructureArticle.html}{API Reference (Data Structure)}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateHIT}}

\code{\link{GenerateHITReviewPolicy}}
}
%\examples{}
\keyword{HITs}
