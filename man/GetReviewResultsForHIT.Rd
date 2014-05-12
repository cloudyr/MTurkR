\name{GetReviewResultsForHIT}
\alias{GetReviewResultsForHIT}
\alias{reviewresults}
\title{Get ReviewPolicy Results for a HIT}
\description{Get HIT- and/or Assignment-level ReviewPolicy Results for a HIT}
\usage{
GetReviewResultsForHIT(    hit, assignment = NULL, policy.level = NULL,
                        retrieve.results = TRUE, retrieve.actions = TRUE,
                        keypair = getOption('MTurkR.keypair'), print = getOption('MTurkR.print'),
                        log.requests = getOption('MTurkR.log'),
                        sandbox = getOption('MTurkR.sandbox'),
                        validation.test = getOption('MTurkR.test'))
}
\arguments{
  \item{hit}{A character string containing a HITId.}
  \item{assignment}{An optional character string containing an AssignmentId. If specified, only results pertaining to that assignment will be returned.}
  \item{policy.level}{Either \code{HIT} or \code{Assignment}. If omitted, the default behavior is to retrieve all data for both policy levels. }
  \item{retrieve.results}{Optionally retrieve ReviewResults. Default is \code{TRUE}.}
  \item{retrieve.actions}{Optionally retrieve ReviewActions. Default is \code{TRUE}.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}  
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server. Default is \code{FALSE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{A simple function to return the results of a ReviewPolicy. This is intended only for advanced users, who should reference MTurk documentation for further information.

\code{reviewresults} is an alias.
}
\value{A list containing up to four named dataframes, depending on what ReviewPolicy (or ReviewPolicies) were attached to the HIT: \code{AssignmentReviewResult}, \code{AssignmentReviewAction}, \code{HITReviewResult}, \code{HITReviewAction}.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GetReviewResultsForHitOperation.html}{API Reference}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_ReviewPoliciesArticle.html}{API Reference (ReviewPolicies)}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_HITReviewPolicyDataStructureArticle.html}{API Reference (Data Structure)}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateHIT}}
}
\examples{
\dontrun{
GetReviewResultsForHIT()
}
}
\keyword{HITs}