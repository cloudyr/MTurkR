\name{ChangeHITType}
\alias{ChangeHITType}
\alias{changehittype}
\title{Change HITType Properties of a HIT}
\description{Change the HITType of a HIT from one HITType to another (e.g., to change the title, description, or qualification requirements associated with a HIT). This will cause a HIT to no longer be grouped with HITs of the previous HITType and instead be grouped with those of the new HITType. You cannot change the payment associated with a HIT without expiring the current HIT and creating a new one.}
\usage{
ChangeHITType(hit = NULL, old.hit.type = NULL, new.hit.type = NULL, 
              title = NULL, description = NULL, reward = NULL, duration = NULL,
              keywords = NULL,
              auto.approval.delay = NULL, qual.req = NULL, 
              old.annotation = NULL,
              verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{hit}{An optional character string containing the HITId whose HITTypeId is to be changed, or a vector of character strings containing each of multiple HITIds to be changed. Must specify \code{hit} xor \code{old.hit.type} xor \code{annotation}.}
  \item{old.hit.type}{An optional character string containing the HITTypeId (or a vector of HITTypeIds) whose HITs are to be changed to the new HITTypeId. Must specify \code{hit} xor \code{old.hit.type} xor \code{annotation}.}
  \item{new.hit.type}{An optional character string specifying the new HITTypeId that this HIT should be visibly grouped with (and whose properties, e.g. reward amount, this HIT should inherit).}
  \item{title}{An optional character string containing the title for the HITType. All HITs of this HITType will be visibly grouped to workers according to this title.}
  \item{description}{An optional character string containing a description of the HITType. This is visible to workers.}
  \item{reward}{An optional character string containing the per-assignment reward amount, in U.S. Dollars (e.g., \dQuote{0.15}).}
  \item{duration}{An optional character string containing the duration of each HIT, in seconds (for example, as returned by \code{\link{seconds}}).}
  \item{keywords}{An optional character string containing a comma-separated set of keywords by which workers can search for HITs of this HITType.}
  \item{auto.approval.delay}{An optional character string specifying the amount of time, in seconds (for example, as returned by \code{\link{seconds}}), before a submitted assignment is automatically granted.}
  \item{qual.req}{An optional character string containing one a QualificationRequirement data structure, as returned by \code{\link{GenerateQualificationRequirement}}.}
  \item{old.annotation}{An optional character string specifying the value of the \code{RequesterAnnotation} field for a batch of HITs to change the HITType of. This can be used to change the HITType for all HITs from a \dQuote{batch} created in the online Requester User Interface (RUI). To use a batch ID, the batch must be written in a character string of the form \dQuote{BatchId:78382;}, where \dQuote{73832} is the batch ID shown in the RUI. Must specify \code{hit} xor \code{old.hit.type} xor \code{annotation}.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
This function changes the HITType of a specified HIT (or multiple specific HITs or all HITs of a specified HITType) to a new HITType.  \code{hit} xor \code{old.hit.type} must be specified. Then, either a new HITTypeId can be specified or a new HITType can be created by atomically by specifying the characteristics of the new HITType.

\code{changehittype()} is an alias.
}
\value{A data frame listing the HITId of each HIT who HITType was changed, its old HITTypeId and new HITTypeId, and whether the request for each HIT was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_ChangeHITTypeOfHITOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateHIT}}

\code{\link{RegisterHITType}}
}
\examples{
\dontrun{
hittype1 <- 
RegisterHITType(title = "10 Question Survey",
                description =
                  "Complete a 10-question survey about news coverage and your opinions",
                reward = ".20", 
                duration = seconds(hours=1), 
                keywords = "survey, questionnaire, politics")
a <- GenerateExternalQuestion("http://www.example.com/", "400")
hit <- CreateHIT(hit.type = hittype1$HITTypeId, 
                 assignments = 1,
                 expiration = seconds(days=1),
                 question = a$string)

# change to HITType with new reward amount
hittype2 <- 
RegisterHITType(title = "10 Question Survey",
                description =
                  "Complete a 10-question survey about news coverage and your opinions",
                reward = ".45", 
                duration = seconds(hours=1), 
                keywords = "survey, questionnaire, politics")
ChangeHITType(hit = hit$HITId,
              new.hit.type=hittype2$HITTypeId)

# Change to new HITType, with arguments stated atomically
ChangeHITType(hit = hit$HITId,
              title = "10 Question Survey", 
              description =
                "Complete a 10-question survey about news coverage and your opinions", 
              reward = ".20", 
              duration = seconds(hours=1), 
              keywords = "survey, questionnaire, politics")

# expire and dispose HIT
ExpireHIT(hit = hit$HITId)
DisposeHIT(hit = hit$HITId)
}
}
\keyword{HITs}