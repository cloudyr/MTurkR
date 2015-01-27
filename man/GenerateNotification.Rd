\name{GenerateNotification}
\alias{GenerateNotification}
\title{Generate Notification}
\description{Generate a HITType Notification data structure for use in \code{\link{SetHITTypeNotification}}.}
\usage{
GenerateNotification(destination, transport = "Email", event.type, 
                     version = "2006-05-05", event.number = "1")
}
\arguments{
  \item{destination}{Currently, a character string containing a complete email address (if \code{transport="Email"}) or the SQS URL (if \code{transport="SQS"}).}
  \item{transport}{Currently only \dQuote{\code{Email}} and \dQuote{\code{SQS}} are supported. AWS recommends the use of the SQS transport.}
  \item{event.type}{A character string containing one of: \code{AssignmentAccepted}, \code{AssignmentAbandoned}, \code{AssignmentReturned}, \code{AssignmentSubmitted}, \code{HITReviewable}, \code{HITExpired}, or \code{Ping}.}
  \item{version}{Version of the HITType Notification API to use. Intended only for advanced users.}
  \item{event.number}{Intended only for advanced users to construct custom Notifications.}
}
\details{Generate a Notification data structure for use in the \code{notification} option of \code{\link{SetHITTypeNotification}}.}
\value{A character string containing a URL query parameter-formatted Notification data structure.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_NotificationDataStructureArticle.html}{API Reference}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMechanicalTurkRequester/Concepts_NotificationsArticle.html}{API Reference: Concept}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{SetHITTypeNotification}}

\code{\link{SendTestEventNotification}}
}
%\examples{}
\keyword{Notifications}
