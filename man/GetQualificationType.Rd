\name{GetQualificationType}
\alias{GetQualificationType}
\alias{qualtype}
\title{Get QualificationType}
\description{Get the details of a Qualification Type.}
\usage{
GetQualificationType(qual, keypair = credentials(), print = getOption('MTurkR.print'),
                    browser = getOption('MTurkR.browser'), 
                    log.requests = getOption('MTurkR.log'),
                    sandbox = getOption('MTurkR.sandbox'),
                    return.qual.dataframe = TRUE,
                    validation.test = getOption('MTurkR.test'))
}
\arguments{
  \item{qual}{A character string containing a QualificationTypeId.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
  \item{return.qual.dataframe}{A logical indicating whether the QualificationType dataframe should be returned. Default is \code{TRUE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{
Retrieve characteristics of a specified QualificationType (as originally specified by \code{\link{CreateQualificationType}}).

\code{qualtype()} is an alias.
}
\value{A dataframe containing the QualificationTypeId of the newly created QualificationType and other details as specified in the request.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GetQualificationTypeOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateQualificationType}}

\code{\link{UpdateQualificationType}}

\code{\link{DisposeQualificationType}}

\code{\link{SearchQualificationTypes}}
}
\examples{
\dontrun{
GetQualificationType("2YCIA0RYNJ9262B1D82MPTUEXAMPLE")
}
}
\keyword{Qualifications}