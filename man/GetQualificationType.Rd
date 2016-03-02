\name{GetQualificationType}
\alias{GetQualificationType}
\alias{qualtype}
\title{Get QualificationType}
\description{Get the details of a Qualification Type.}
\usage{
GetQualificationType(qual, verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{qual}{A character string containing a QualificationTypeId.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
Retrieve characteristics of a specified QualificationType (as originally specified by \code{\link{CreateQualificationType}}).

\code{qualtype()} is an alias.
}
\value{A data frame containing the QualificationTypeId of the newly created QualificationType and other details as specified in the request.}
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
qual1 <- 
CreateQualificationType(name="Worked for me before",
    description="This qualification is for people who have worked for me before",
    status = "Active",
    keywords="Worked for me before")
GetQualificationType(qual1$QualificationTypeId)
DisposeQualificationType(qual1$QualificationTypeId)
}
}
\keyword{Qualifications}