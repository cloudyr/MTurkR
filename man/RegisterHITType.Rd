\name{RegisterHITType}
\alias{RegisterHITType}
\alias{hittype}
\title{Register a HITType}
\description{Register a HITType on MTurk, in order to create one or more HITs to show up as a group to workers.}
\usage{
RegisterHITType(title, description, reward, duration, keywords = NULL, 
                auto.approval.delay = NULL, qual.req = NULL, 
                verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{title}{A character string containing the title for the HITType. All HITs of this HITType will be visibly grouped to workers according to this title. Maximum of 128 characters.}
  \item{description}{A character string containing a description of the HITType. This is visible to workers. Maximum of 2000 characters.}
  \item{reward}{A character string containing the per-assignment reward amount, in U.S. Dollars (e.g., \dQuote{0.15}).}
  \item{duration}{A character string containing the amount of time workers have to complete an assignment for HITs of this HITType, in seconds (for example, as returned by \code{\link{seconds}}). Minimum of 30 seconds and maximum of 365 days.}
  \item{keywords}{An optional character string containing a comma-separated set of keywords by which workers can search for HITs of this HITType. Maximum of 1000 characters.}
  \item{auto.approval.delay}{An optional character string specifying the amount of time, in seconds (for example, as returned by \code{\link{seconds}}), before a submitted assignment is automatically granted. Maximum of 30 days.}
  \item{qual.req}{An optional character string containing one or more QualificationRequirements data structures, for example as returned by \code{\link{GenerateQualificationRequirement}}.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{All HITs of a given HITType are visibly grouped together for workers and share common properties (e.g., reward amount, QualificationRequirements). This function registers a HITType in the MTurk system, which can then be used when creating individual HITs. If a requester wants to change these properties for a specific HIT, the HIT should be changed to a new HITType (see \code{\link{ChangeHITType}}).

\code{hittype()} is an alias.
}
\value{A two-column data frame containing the HITTypeId of the newly registered HITType and an indicator for whether the registration request was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_RegisterHITTypeOperation.html}{API Reference: Operation}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMechanicalTurkRequester/Concepts_HITTypesArticle.html}{API Reference: Concept}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateHIT}}

\code{\link{ChangeHITType}}
}
\examples{
\dontrun{
RegisterHITType(title="10 Question Survey",
                description=
                "Complete a 10-question survey about news coverage and your opinions",
                reward=".20", 
                duration=seconds(hours=1), 
                keywords="survey, questionnaire, politics")
}
}
\keyword{HITs}