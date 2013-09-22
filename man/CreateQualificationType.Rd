\name{CreateQualificationType}
\alias{CreateQualificationType}
\alias{createqual}
\title{Create QualificationType}
\description{Create a QualificationType. This creates a QualificationType, but does not assign it to any workers. All characteristics of the QualificationType (except name and keywords) can be changed later with \code{\link{UpdateQualificationType}}.}
\usage{
CreateQualificationType(name, description, status, keywords = NULL, 
						retry.delay = NULL, test = NULL, answerkey = NULL, 
						test.duration = NULL,
						validate.test = FALSE, validate.answerkey = FALSE,
						auto = NULL, auto.value = NULL, keypair = credentials(),
						print = TRUE, browser = FALSE, log.requests = TRUE,
						sandbox = getOption('MTurkR.sandbox'), validation.test = FALSE)
}
\arguments{
  \item{name}{A name for the QualificationType. This is visible to workers. It cannot be modified by \code{\link{UpdateQualificationType}}.}
  \item{description}{A longer description of the QualificationType. This is visible to workers. Maximum of 2000 characters.}
  \item{status}{A character vector of \dQuote{Active} or \dQuote{Inactive}, indicating whether the QualificationType should be active and visible.}
  \item{keywords}{An optional character string containing a comma-separated set of keywords by which workers can search for the QualificationType. Maximum 1000 characters. These cannot be modified by \code{\link{UpdateQualificationType}}.}
  \item{retry.delay}{An optional time (in seconds) indicating how long workers have to wait before requesting the QualificationType after an initial rejection.}
  \item{test}{An optional character string consisting of a QuestionForm data structure, used as a test a worker must complete before the  QualificationType is granted to them.}
  \item{answerkey}{An optional character string consisting of an AnswerKey data structure, used to automatically score the test, perhaps as returned by \code{\link{GenerateAnswerKey}}.}
  \item{test.duration}{An optional time (in seconds) indicating how long workers have to complete the test.}
  \item{validate.test}{A logical specifying whether the \code{test} parameter should be validated against the relevant MTurk schema prior to creating the QualificationType (operation will fail if it does not validate, and will return validation information). Default is \code{FALSE}.}
  \item{validate.answerkey}{A logical specifying whether the \code{answerkey} parameter should be validated against the relevant MTurk schema prior to creating the QualificationType (operation will fail if it does not validate, and will return validation information). Default is \code{FALSE}.}
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
A function to create a QualificationType. Active QualificationTypes are visible to workers and to other requesters. All characteristics of the QualificationType, other than the name and keywords, can later be modified by \code{\link{UpdateQualificationType}}. Qualifications can then be used to assign Qualifications to workers with \code{\link{AssignQualification}} and invoked as QualificationRequirements in \code{\link{RegisterHITType}} and/or \code{\link{CreateHIT}} operations.

Note that when \code{browser=TRUE}, specifying this operation with \code{test} (and, optionally, \code{answer}) parameters may produce unintended behavior due to constraints on the length of the URL imposed by some web browsers.

\code{createqual()} is an alias.
}
\value{A dataframe containing the QualificationTypeId of the newly created QualificationType and other details as specified in the request.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_CreateQualificationTypeOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GetQualificationType}}

\code{\link{DisposeQualificationType}}

\code{\link{UpdateQualificationType}}

\code{\link{SearchQualificationTypes}}
}
\examples{
\dontrun{
qual1 <- CreateQualificationType(name="Worked for me before",
		description="This qualification is for people who have worked for me before",
		keywords="Worked for me before",sandbox=TRUE)
qual1
}
}
\keyword{Qualifications}