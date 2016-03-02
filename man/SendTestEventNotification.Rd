\name{SendTestEventNotification}
\alias{SendTestEventNotification}
\alias{notificationtest}
\title{Test a Notification}
\description{Test a HITType Notification, for example, to try out a HITType Notification before creating a HIT.}
\usage{
SendTestEventNotification(notification, test.event.type, 
                          verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{notification}{A character string containing a URL query parameter-formatted Notification structure (e.g., returned by \code{\link{GenerateNotification}}).}
  \item{test.event.type}{A character string containing one of: \code{AssignmentAccepted}, \code{AssignmentAbandoned}, \code{AssignmentReturned}, \code{AssignmentSubmitted}, \code{HITReviewable}, \code{HITExpired} (the default), or \code{Ping}.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
Test a Notification configuration. The test mimics whatever the Notification configuration will do when the event described in \code{test.event.type} occurs. For example, if a Notification has been configured to send an email any time an Assignment is Submitted, testing for an AssignmentSubmitted event should trigger an email. Similarly, testing for an AssignmentReturned event should do nothing.

\code{notificationtest} is an alias.
}
\value{A data frame containing the notification, the event type, and details on whether the request was valid. As a side effect, a notification will be sent to the configured destination (either an email or an SQS queue).}
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
hittype <- 
RegisterHITType(title="10 Question Survey",
                description=
                "Complete a 10-question survey about news coverage and your opinions",
                reward=".20", 
                duration=seconds(hours=1), 
                keywords="survey, questionnaire, politics")

a <- GenerateNotification("requester@example.com", event.type = "HITExpired")
SetHITTypeNotification(hit.type = hittype$HITTypeId, 
                       notification = a,
                       active = TRUE)
# send test notification
SendTestEventNotification(a, test.event.type="HITExpired")
}
}
\keyword{Notifications}
