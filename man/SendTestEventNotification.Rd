\name{SendTestEventNotification}
\alias{SendTestEventNotification}
\alias{notificationtest}
\title{Test a Notification}
\description{Test a HITType Notification, for example, to try out a HITType Notification before creating a HIT.}
\usage{
SendTestEventNotification(	notification, test.event.type, 
							keypair = credentials(), print = TRUE, browser = FALSE, 
							log.requests = TRUE, sandbox = FALSE)
}
\arguments{
  \item{notification}{A character string containing a URL query parameter-formatted Notification structure (e.g., returned by \code{\link{GenerateNotification}}).}
  \item{test.event.type}{A character string containing one of: \code{AssignmentAccepted}, \code{AssignmentAbandoned}, \code{AssignmentReturned}, \code{AssignmentSubmitted}, \code{HITReviewable}, \code{HITExpired} (the default), \code{Ping}.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
}
\details{
Test a Notification configuration. The test mimics whatever the Notification configuration will do when the event described in \code{test.event.type} occurs.

Note: The API operation underlying \code{\link{SetHITTypeNotification}} and \code{SendTestEventNotification} has been deprecated by Amazon is not receiving ongoing development, but remains available for backward compatibility.

\code{notificationtest()} is an alias.
}
\value{Potentially an email sent to the email address specified in the \code{notification} parameter. Otherwise, nothing}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_SendTestEventNotificationOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GenerateNotification}}

\code{\link{SetHITTypeNotification}}
}
\examples{
\dontrun{
a <- GenerateNotification("requester@example.com",event.type="HITExpired")
SendTestEventNotification(a,test.event.type="HITReviewable") # no notification email will be sent
SendTestEventNotification(a,test.event.type="HITExpired") # notification email will be sent
}
}
\keyword{Notifications}