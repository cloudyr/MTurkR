\name{RejectAssignment}
\alias{RejectAssignment}
\alias{RejectAssignments}
\alias{reject}
\title{Reject Assignment}
\description{Reject a Worker's assignment (or multiple assignments) submitted for a HIT. Feedback should be provided for why an assignment was rejected.}
\usage{
RejectAssignment(assignments, feedback = NULL, 
                 verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{assignments}{A character string containing an AssignmentId, or a vector of multiple character strings containing multiple AssignmentIds, to reject.}
  \item{feedback}{An optional character string containing any feedback for a worker. This must have length 1 or length equal to the number of workers.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{Reject assignments, by AssignmentId (as returned by \code{\link{GetAssignment}}). More advanced functionality to quickly reject many or all assignments (ala \code{\link{ApproveAllAssignments}}) is intentionally not provided.

\code{RejectAssignments()} and \code{reject()} are aliases.
}
\value{A data frame containing the list of AssignmentIds, feedback (if any), and whether or not each rejection request was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_RejectAssignmentOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{ApproveAssignment}}
}
\examples{
\dontrun{
RejectAssignment(assignments="26XXH0JPPSI23H54YVG7BKLEXAMPLE")
}
}
\keyword{Assignments}