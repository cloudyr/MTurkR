\name{SendTestEventNotification}
\alias{SendTestEventNotification}
\alias{notificationtest}
\title{Test a Notification}
\description{Test a HITType Notification, for example, to try out a HITType Notification before creating a HIT.}
\usage{
SendTestEventNotification(  notification, test.event.type, 
                            keypair = getOption('MTurkR.keypair'), print = getOption('MTurkR.print'),
                            log.requests = getOption('MTurkR.log'),
                            sandbox = getOption('MTurkR.sandbox'),
                            validation.test = getOption('MTurkR.test'))
}
\arguments{
  \item{notification}{A character string containing a URL query parameter-formatted Notification structure (e.g., returned by \code{\link{GenerateNotification}}).}
  \item{test.event.type}{A character string containing one of: \code{AssignmentAccepted}, \code{AssignmentAbandoned}, \code{AssignmentReturned}, \code{AssignmentSubmitted}, \code{HITReviewable}, \code{HITExpired} (the default), \code{Ping}.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{
Test a Notification configuration. The test mimics whatever the Notification configuration will do when the event described in \code{test.event.type} occurs. For example, if a Notification has been configured to send an email anytime an Assignment is Submitted, testing for an AssignmentSubmitted event should trigger an email. Similarly, testing for an AssignmentReturned event should do nothing.

\code{notificationtest} is an alias.
}
\value{Potentially either an email sent to the email address specified in the \code{notification} parameter, or an AWS Simple Queue Service event. Otherwise, nothing.}
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
