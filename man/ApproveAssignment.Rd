\name{ApproveAssignment}
\alias{ApproveAssignment}
\alias{ApproveAssignments}
\alias{approve}
\alias{ApproveAllAssignments}
\alias{approveall}
\title{Approve Assignment(s)}
\description{Approve one or more submitted assignments, or approve all assignments for a given HIT or HITType. Also allows you to approve a previously rejected assignment. This function spends money from your MTurk account.}
\usage{
ApproveAssignment(assignments, feedback = NULL, rejected = FALSE, 
                  verbose = getOption('MTurkR.verbose', TRUE), ...)

ApproveAllAssignments(hit = NULL, hit.type = NULL, feedback = NULL, 
                      verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{assignments}{A character string containing an AssignmentId, or a vector of multiple character strings containing multiple AssignmentIds, to approve.}
  \item{hit}{A character string containing a HITId all of whom's assignments are to be approved.}
  \item{hit.type}{A character string containing a HITTypeId (or a vector of HITTypeIds) all of whom's HITs' assignments are to be approved.}
  \item{feedback}{An optional character string containing any feedback for a worker. This must have length 1 or length equal to the number of workers. Maximum of 1024 characters. For \code{ApproveAllAssignments}, must be length 1.}
  \item{rejected}{A logical indicating whether the assignment(s) had previously been rejected (default \code{FALSE}). Approval of previously rejected assignments must be conducted separately from other approvals.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
Approve assignments, by AssignmentId (as returned by \code{\link{GetAssignment}} or by HITId or HITTypeId. Must specify \code{assignments} xor \code{hit} xor \code{hit.type}. \code{ApproveAllAssignments} approves all assignments of a given HIT or HITType without first having to perform \code{\link{GetAssignment}}.

\code{ApproveAssignments()} and \code{approve()} are aliases for \code{ApproveAssignment}. \code{approveall()} is an alias for \code{ApproveAllAssignments}.
}
\value{A dataframe containing the list of AssignmentIds, feedback (if any), and whether or not each approval request was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_ApproveAssignmentOperation.html}{API Reference: Approve Assignment}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_ApproveRejectedAssignmentOperation.html}{API Reference: Approve Rejected Assignment}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{RejectAssignment}}
}
\examples{
\dontrun{
a <- ApproveAssignment(assignments = "26XXH0JPPSI23H54YVG7BKLEXAMPLE")

b <- ApproveAssignment(
assignments = c("26XXH0JPPSI23H54YVG7BKLEXAMPLE1","26XXH0JPPSI23H54YVG7BKLEXAMPLE2"),
feedback = "Great work!")

c <- ApproveAllAssignments(hit = "2MQB727M0IGF304GJ16S1F4VE3AYDQ")

d <- ApproveAllAssignments(hit.type = "2FFNCWYB49F9BBJWA4SJUNST5OFSOW")
}
}
\keyword{Assignments}