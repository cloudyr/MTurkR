\name{GetAssignment}
\alias{GetAssignment}
\alias{GetAssignments}
\alias{assignment}
\alias{assignments}
\title{Get Assignment(s)}
\description{Get an assignment or multiple assignments for one or more HITs (or a HITType) as a dataframe.}
\usage{
GetAssignment(	assignment = NULL, hit = NULL, hit.type = NULL, status = NULL, 
				return.all = FALSE, pagenumber = "1", pagesize = "10", 
				sortproperty = "SubmitTime", sortdirection = "Ascending", 
				response.group = NULL, keypair = credentials(), print = TRUE, browser = FALSE,
				log.requests = TRUE, sandbox = getOption('MTurkR.sandbox'), return.assignment.dataframe = TRUE,
				validation.test = FALSE)
}
\arguments{
  \item{assignment}{An optional character string specifying the AssignmentId of an assignment to return.}
  \item{hit}{An optional character string specifying the HITId whose assignments are to be returned, or a vector of character strings specifying multiple HITIds all of whose assignments are to be returned.}
  \item{hit.type}{An optional character string specifying the HITTypeId of one or more HITs whose assignments are to be returned.}
  \item{status}{An optional character string (of \dQuote{Approved},\dQuote{Rejected},\dQuote{Submitted}), specifying whether only a subset of assignments should be returned. If \code{NULL}, all assignments are returned (the default). Only applies when \code{hit} or \code{hit.type} are specified; ignored otherwise.}
  \item{return.all}{If \code{TRUE}, all available assignments are returned. Otherwise, only assignments falling within the specified \code{pagenumber} and \code{pagesize} search results are returned.}
  \item{pagenumber}{An optional character string indicating which page of search results should be returned (only appropriate when specifying a single HITId). Most users can ignore this.}
  \item{pagesize}{An optional character string indicating how many search results should be returned by each request (only appropriate when specifying a single HITId), between 1 and 100. Most users can ignore this.}
  \item{sortproperty}{One of \dQuote{AcceptTime}, \dQuote{SubmitTime}, \dQuote{AssignmentStatus}. Ignored if \code{return.all=TRUE}. Most users can ignore this.}
  \item{sortdirection}{Either \dQuote{Ascending} or \dQuote{Descending}. Ignored if \code{return.all=TRUE}. Most users can ignore this.}
  \item{response.group}{An optional character string (or vector of character strings) specifying what details to return. If \code{assignment} is specified, \code{response.group} can include any of \dQuote{Request}, \dQuote{Minimal}, \dQuote{AssignmentFeedback}, \dQuote{HITDetail}, and/or \dQuote{HITQuestion}. If \code{hit} or \code{hit.type} is specified, \code{response.group} can include \dQuote{Request}, \dQuote{Minimal}, and/or \dQuote{AssignmentFeedback}. For more information, see \url{http://docs.aws.amazon.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_CommonParametersArticle.html}.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
  \item{return.assignment.dataframe}{A logical specifying whether the Assignment dataframe should be returned. Default is \code{TRUE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{This function returns the requested assignments. The function must specify an AssignmentId xor a HITId xor a HITTypeId. If an AssignmentId is specified, only that assignment is returned. If a HIT or HITType is specified, default behavior is to return all assignments through a series of sequential (but invisible) API calls meaning that returning large numbers of assignments (or assignments for a large number of HITs in a single request) may be time consuming.

\code{GetAssignments()}, \code{assignment()}, and \code{assignments()} are aliases.
}
\value{Optionally a dataframe containing Assignment data, including workers responses to any questions specified in the \code{question} parameter of the \code{CreateHIT} function.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GetAssignmentOperation.html}{API Reference: GetAssignment}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GetAssignmentsForHITOperation.html}{API Reference: GetAssignmentsForHIT }
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GetHIT}}

\code{\link{ApproveAssignment}}

\code{\link{ApproveAllAssignments}}

\code{\link{RejectAssignment}}
}
\examples{
\dontrun{
GetAssignment(assignments="26XXH0JPPSI23H54YVG7BKLEXAMPLE")
GetAssignment(hit="2MQB727M0IGF304GJ16S1F4VE3AYDQ",return.all=TRUE)
GetAssignment(hit.type="2FFNCWYB49F9BBJWA4SJUNST5OFSOW",return.all=FALSE,pagenumber="1",pagesize="50")
}
}
\keyword{Assignments}