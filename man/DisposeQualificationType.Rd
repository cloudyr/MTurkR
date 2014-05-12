\name{DisposeQualificationType}
\alias{DisposeQualificationType}
\alias{disposequal}
\title{Dispose QualificationType}
\description{Dispose of a QualificationType. This deletes the QualificationType, Qualification scores for all workers, and all records thereof.}
\usage{
DisposeQualificationType(qual, keypair = getOption('MTurkR.keypair'),
                         print = getOption('MTurkR.print'), 
                         log.requests = getOption('MTurkR.log'),
                         sandbox = getOption('MTurkR.sandbox'),
                         validation.test = getOption('MTurkR.test'))
}
\arguments{
  \item{qual}{A character string containing a QualificationTypeId.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server. Default is \code{FALSE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{
A function to dispose of a QualificationType that is no longer needed. All information about the QualificationType and all workers' Qualifications of that type are permanently deleted.

\code{disposequal()} is an alias.
}
\value{A dataframe containing the QualificationTypeId and whether the request to dispose was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_DisposeQualificationTypeOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GetQualificationType}}

\code{\link{CreateQualificationType}}

\code{\link{UpdateQualificationType}}

\code{\link{SearchQualificationTypes}}
}
\examples{
\dontrun{
DisposeQualificationType("2YCIA0RYNJ9262B1D82MPTUEXAMPLE")
}
}
