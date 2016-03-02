\name{DisposeQualificationType}
\alias{DisposeQualificationType}
\alias{disposequal}
\title{Dispose QualificationType}
\description{Dispose of a QualificationType. This deletes the QualificationType, Qualification scores for all workers, and all records thereof.}
\usage{
DisposeQualificationType(qual, verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{qual}{A character string containing a QualificationTypeId.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
A function to dispose of a QualificationType that is no longer needed. All information about the QualificationType and all workers' Qualifications of that type are permanently deleted.

\code{disposequal()} is an alias.
}
\value{A data frame containing the QualificationTypeId and whether the request to dispose was valid.}
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
qual1 <- 
CreateQualificationType(name="Worked for me before",
    description="This qualification is for people who have worked for me before",
    status = "Active",
    keywords="Worked for me before")
DisposeQualificationType(qual1$QualificationTypeId)
}
}
