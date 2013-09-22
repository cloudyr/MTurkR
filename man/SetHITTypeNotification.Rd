\name{SetHITTypeNotification}
\alias{SetHITTypeNotification}
\alias{setnotification}
\title{Configure a HITType Notification}
\description{Configure a notification to be sent when specific actions occur for the specified HITType.}
\usage{
SetHITTypeNotification(	hit.type, notification = NULL, active = NULL, 
						keypair = credentials(), print = TRUE, browser = FALSE, 
						log.requests = TRUE, sandbox = getOption('MTurkR.sandbox'),
                        validation.test = FALSE)
}
\arguments{
  \item{hit.type}{A character string specifying the HITTypeId of the HITType for which notifications are being configured.}
  \item{notification}{A character string containing a URL query parameter-formatted Notification structure (e.g., returned by \code{\link{GenerateNotification}}).}
  \item{active}{A logical indicating whether the Notification is active or inactive.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
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