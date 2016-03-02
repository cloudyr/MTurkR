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

ApproveAllAssignments(hit = NULL, hit.type = NULL, annotation = NULL, 
                      feedback = NULL, 
                      verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{assignments}{A character string containing an AssignmentId, or a vector of multiple character strings containing multiple AssignmentIds, to approve.}
  \item{hit}{A character string containing a HITId all of whose assignments are to be approved. Must specify \code{hit} xor \code{hit.type} xor \code{annotation}.}
  \item{hit.type}{A character string containing a HITTypeId (or a vector of HITTypeIds) all of whose HITs' assignments are to be approved. Must specify \code{hit} xor \code{hit.type} xor \code{annotation}.}
  \item{annotation}{An optional character string specifying the value of the \code{RequesterAnnotation} field for a batch of HITs. This can be used to approve all assignments for all HITs from a \dQuote{batch} created in the online Requester User Interface (RUI). To use a batch ID, the batch must be written in a character string of the form \dQuote{BatchId:78382;}, where \dQuote{73832} is the batch ID shown in the RUI. Must specify \code{hit} xor \code{hit.type} xor \code{annotation}.}
  \item{feedback}{An optional character string containing any feedback for a worker. This must have length 1 or length equal to the number of workers. Maximum of 1024 characters. For \code{ApproveAllAssignments}, must be length 1.}
  \item{rejected}{A logical indicating whether the assignment(s) had previously been rejected (default \code{FALSE}). Approval of previously rejected assignments must be conducted separately from other approvals.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
Approve assignments, by AssignmentId (as returned by \code{\link{GetAssignment}} or by HITId or HITTypeId. Must specify \code{assignments} xor \code{hit} xor \code{hit.type}. \code{ApproveAllAssignments} approves all assignments of a given HIT or HITType without first having to perform \code{\link{GetAssignment}}.

\code{ApproveAssignments()} and \code{approve()} are aliases for \code{ApproveAssignment}. \code{approveall()} is an alias for \code{ApproveAllAssignments}.
}
\value{A data frame containing the list of AssignmentIds, feedback (if any), and whether or not each approval request was valid.}
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
# Approve one assignment
ApproveAssignment(assignments = "26XXH0JPPSI23H54YVG7BKLEXAMPLE")

# Approve multiple assignments with the same feedback
ApproveAssignment(assignments = c("26XXH0JPPSI23H54YVG7BKLEXAMPLE1",
                                  "26XXH0JPPSI23H54YVG7BKLEXAMPLE2"),
                  feedback = "Great work!")


# Approve all assignments for a given HIT
ApproveAllAssignments(hit = "2MQB727M0IGF304GJ16S1F4VE3AYDQ")
# Approve all assignments for a given HITType
ApproveAllAssignments(hit.type = "2FFNCWYB49F9BBJWA4SJUNST5OFSOW")
# Approve all assignments for a given batch from the RUI
ApproveAllAssignments(annotation="BatchId:78382;")
}
}
\keyword{Assignments}