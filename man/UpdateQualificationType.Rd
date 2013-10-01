\name{UpdateQualificationType}
\alias{UpdateQualificationType}
\alias{updatequal}
\title{Update a Worker QualificationType}
\description{Update characteristics of a QualificationType.}
\usage{
UpdateQualificationType(qual, description = NULL, status = NULL, 
						retry.delay = NULL, test = NULL, answerkey = NULL, 
						test.duration = NULL, auto = NULL, auto.value = NULL, 
						keypair = credentials(), print = getOption('MTurkR.print'),
                        browser = getOption('MTurkR.browser'),
                        log.requests = getOption('MTurkR.log'),
                        sandbox = getOption('MTurkR.sandbox'),
                        validation.test = getOption('MTurkR.test'))
}
\arguments{
  \item{qual}{A character string containing a QualificationTypeId.}
  \item{description}{A character string describing the Qualification. This is visible to workers. Maximum of 2000 characters.}
  \item{status}{A character vector of \dQuote{Active} or \dQuote{Inactive}, indicating whether the QualificationType should be active and visible.}
  \item{retry.delay}{An optional time (in seconds) indicating how long workers have to wait before requesting the QualificationType after an initial rejection.}
  \item{test}{An optional character string consisting of a QuestionForm data structure, used as a test a worker must complete before the  QualificationType is granted to them.}
  \item{answerkey}{An optional character string consisting of an AnswerKey data structure, used to automatically score the test. If a previous test with an associated AnswerKey is updated, the new test will not have an AnswerKey unless a new one is included in the same call (even if it is identical to the previous AnswerKey).}
  \item{test.duration}{An optional time (in seconds) indicating how long workers have to complete the test.}
  \item{auto}{A logical indicating whether the Qualification is automatically granted to workers who request it. Default is \code{FALSE}.}
  \item{auto.value}{An optional parameter specifying the value that is automatically assigned to workers when they request it (if the Qualification is automatically granted).}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{
A function to update the characteristics of a QualificationType. Name and keywords cannot be modified after a QualificationType is created.

\code{updatequal()} is an alias.
}
\value{A dataframe containing the QualificationTypeId of the newly created QualificationType and other details as specified in the request.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_UpdateQualificationTypeOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GetQualificationType}}

\code{\link{CreateQualificationType}}

\code{\link{DisposeQualificationType}}

\code{\link{SearchQualificationTypes}}
}
\examples{
\dontrun{
qual1 <- CreateQualificationType(title="Worked for me before",description="This qualification is for people who have worked for me before",keywords="Worked for me before",sandbox=TRUE)
qual2 <- UpdateQualificationType(qual.test,description="This qualification is for everybody!",auto=TRUE,auto.value="5",sandbox=TRUE)
}
}
\keyword{Qualifications}