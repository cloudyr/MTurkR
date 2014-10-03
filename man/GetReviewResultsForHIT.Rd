\name{GetReviewResultsForHIT}
\alias{GetReviewResultsForHIT}
\alias{reviewresults}
\title{Get ReviewPolicy Results for a HIT}
\description{Get HIT- and/or Assignment-level ReviewPolicy Results for a HIT}
\usage{
GetReviewResultsForHIT(hit, assignment = NULL, policy.level = NULL,
                       retrieve.results = TRUE, retrieve.actions = TRUE,
                       verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{hit}{A character string containing a HITId.}
  \item{assignment}{An optional character string containing an AssignmentId. If specified, only results pertaining to that assignment will be returned.}
  \item{policy.level}{Either \code{HIT} or \code{Assignment}. If omitted, the default behavior is to retrieve all data for both policy levels. }
  \item{retrieve.results}{Optionally retrieve ReviewResults. Default is \code{TRUE}.}
  \item{retrieve.actions}{Optionally retrieve ReviewActions. Default is \code{TRUE}.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{A simple function to return the results of a ReviewPolicy. This is intended only for advanced users, who should reference MTurk documentation for further information or see the notes in \code{\link{GenerateReviewPolicy}}.

\code{reviewresults} is an alias.
}
\value{A list containing up to four named dataframes, depending on what ReviewPolicy (or ReviewPolicies) were attached to the HIT: \code{AssignmentReviewResult}, \code{AssignmentReviewAction}, \code{HITReviewResult}, and/or \code{HITReviewAction}.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GetReviewResultsForHitOperation.html}{API Reference}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_ReviewPoliciesArticle.html}{API Reference (ReviewPolicies)}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_HITReviewPolicyDataStructureArticle.html}{API Reference (Data Structure)}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateHIT}}

\code{\link{GenerateReviewPolicy}}
}
%\examples{}
\keyword{HITs}