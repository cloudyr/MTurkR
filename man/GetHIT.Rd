\name{GetHIT}
\alias{GetHIT}
\alias{gethit}
\alias{hit}
\alias{HITStatus}
\alias{status}
\title{Get HIT}
\description{Retrieve various details of a HIT as a dataframe. What details are returned depend upon the requested ResponseGroup.}
\usage{
GetHIT(hit, response.group = NULL,
       return.hit.dataframe = TRUE, return.qual.dataframe = TRUE,
       verbose = getOption('MTurkR.verbose', TRUE), ...)

HITStatus(hit = NULL, hit.type = NULL, verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{hit}{An optional character string specifying the HITId of the HIT to be retrieved. Must specify \code{hit} xor \code{hit.type}.}
  \item{hit.type}{An optional character string specifying the HITTypeId (or a vector of HITTypeIds) of the HIT(s) to be retrieved. Must specify \code{hit} xor \code{hit.type}, otherwise all HITs are returned in \code{HITStatus}.}
  \item{response.group}{An optional character string (or vector of character strings) specifying what details of each HIT to return of: \dQuote{Request}, \dQuote{Minimal}, \dQuote{HITDetail}, \dQuote{HITQuestion}, \dQuote{HITAssignmentSummary}. For more information, see \url{http://docs.aws.amazon.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_CommonParametersArticle.html}.}
  \item{return.hit.dataframe}{A logical indicating whether the dataframe of HITs should be returned. Default is \code{TRUE}.}
  \item{return.qual.dataframe}{A logical indicating whether the list of each HIT's QualificationRequirements (stored as dataframes in that list) should be returned. Default is \code{TRUE}.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
\code{GetHIT} retrieves characteristics of a HIT. \code{HITStatus} is a wrapper that retrieves the Number of Assignments Pending, Number of Assignments Available, Number of Assignments Completed for the HIT(s), which is helpful for checking on the progress of currently available HITs. Specifying a \code{hit.type} causes the function to first search for avialable HITs of that HITType, then return the requested information for each HIT.

\code{gethit()} and \code{hit()} are aliases for \code{GetHIT}. \code{status()} is an alias for \code{HITStatus}.
}
\value{Optionally a dataframe containing the HITs and a list of each HIT's QualificationRequirements (stored as dataframes in that list in the order that HITs were retrieved.). The exact characteristics of each HIT returned depend upon the \code{response.group} parameter.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GetHITOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GetHITsForQualificationType}}

\code{\link{GetReviewableHITs}}

\code{\link{SearchHITs}}
}
\examples{
\dontrun{
# register HITType
hittype <- 
RegisterHITType(title="10 Question Survey",
                description=
                "Complete a 10-question survey about news coverage and your opinions",
                reward=".20", 
                duration=seconds(hours=1), 
                keywords="survey, questionnaire, politics")

a <- GenerateExternalQuestion("http://www.example.com/","400")
hit1 <- 
CreateHIT(hit.type = hittype$HITTypeId, question = a$string)

GetHIT(hit1$HITId)
HITStatus(hit1$HITId)

# cleanup
DisableHIT(hit1$HITId)
}
}
\keyword{HITs}