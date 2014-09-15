\name{SetHITTypeNotification}
\alias{SetHITTypeNotification}
\alias{setnotification}
\title{Configure a HITType Notification}
\description{Configure a notification to be sent when specific actions occur for the specified HITType.}
\usage{
SetHITTypeNotification(hit.type, notification = NULL, active = NULL, 
                       verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{hit.type}{A character string specifying the HITTypeId of the HITType for which notifications are being configured.}
  \item{notification}{A character string containing a URL query parameter-formatted Notification structure (e.g., returned by \code{\link{GenerateNotification}}).}
  \item{active}{A logical indicating whether the Notification is active or inactive.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
Configure a notification to be sent to the requester whenever an event (specified in the \code{Notification} object) occurs. This is useful for receiving email notifications about, for example, when assignments are submitted or HITs are completed.

Note: The API operation underlying \code{SetHITTypeNotification} and \code{\link{SendTestEventNotification}} has been deprecated by Amazon is not receiving ongoing development, but remains available for backward compatibility.

\code{setnotification()} is an alias.
}
\value{A dataframe containing details of the Notification and whether or not the request was successfully executed by MTurk.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_SetHITTypeNotificationOperation.html}{API Reference: Operation}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMechanicalTurkRequester/Concepts_NotificationsArticle.html}{API Reference: Concept}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GenerateNotification}}

\code{\link{SendTestEventNotification}}
}
\examples{
\dontrun{
a <- GenerateNotification("requester@example.com",event.type="HITExpired")
SetHITTypeNotification(hit.type="2FFNCWYB49F9BBJWA4SJUNST5OFSOW",notification=a,active=TRUE)
}
}
\keyword{Notifications}