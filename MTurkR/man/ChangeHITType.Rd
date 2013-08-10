\name{ChangeHITType}
\alias{ChangeHITType}
\alias{changehittype}
\title{Change HITType Properties of a HIT}
\description{Change the HITType of a HIT from one HITType to another (e.g., to change the title, description, or qualification requirements associated with a HIT). This will cause a HIT to no longer be grouped with HITs of the previous HITType and instead be grouped with those of the new HITType. You cannot change the payment associated with a HIT without expiring the current HIT and creating a new one.}
\usage{
ChangeHITType(	hit = NULL, old.hit.type = NULL, new.hit.type = NULL, 
				title = NULL, description = NULL, reward = NULL, duration = NULL, keywords = NULL, 
				auto.approval.delay = NULL, qual.req = NULL, 
				keypair = credentials(), print = TRUE, browser = FALSE, 
				log.requests = TRUE, sandbox = FALSE, validation.test = FALSE)
}
\arguments{
  \item{hit}{An optional character string containing the HITId whose HITTypeId is to be changed, or a vector of character strings containing each of multiple HITIds to be changed.}
  \item{old.hit.type}{An optional character string containing the HITTypeId whose HITs are to be changed to the new HITTypeId.}
  \item{new.hit.type}{An optional character string specifying the new HITTypeId that this HIT should be visibly grouped with (and whose properties, e.g. reward amount, this HIT should inherit).}
  \item{title}{An optional character string containing the title for the HITType. All HITs of this HITType will be visibly grouped to workers according to this title.}
  \item{description}{An optional character string containing a description of the HITType. This is visible to workers.}
  \item{reward}{An optional character string containing the per-assignment reward amount, in U.S. Dollars (e.g., \dQuote{0.15}).}
  \item{duration}{An optional character string containing the duration of each HIT, in seconds (for example, as returned by \code{\link{seconds}}).}
  \item{keywords}{An optional character string containing a comma-separated set of keywords by which workers can search for HITs of this HITType.}
  \item{auto.approval.delay}{An optional character string specifying the amount of time, in seconds (for example, as returned by \code{\link{seconds}}), before a submitted assignment is automatically granted.}
  \item{qual.req}{An optional character string containing one a QualificationRequirement data structure, as returned by \code{\link{GenerateQualificationRequirement}}.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{
This function changes the HITType of a specified HIT (or multiple specific HITs or all HITs of a specified HITType) to a new HITType.  \code{hit} xor \code{old.hit.type} must be specified. Then, either a new HITTypeId can be specified or a new HITType can be created by atomically by specifying the characteristics of the new HITType.

\code{changehittype()} is an alias.
}
\value{A dataframe listing the HITId of each HIT who HITType was changed, its old HITTypeId and new HITTypeId, and whether the request for each HIT was valid.}
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
ChangeHITType(hit="2MQB727M0IGF304GJ16S1F4VE3AYDQ",new.hit.type="2KAVWAVKI62N29TGK5Q9FQH6BRJQIY")
ChangeHITType(old.hit.type="2FFNCWYB49F9BBJWA4SJUNST5OFSOW",new.hit.type="2KAVWAVKI62N29TGK5Q9FQH6BRJQIY")
ChangeHITType(hit="2MQB727M0IGF304GJ16S1F4VE3AYDQ",title="10 Question Survey", description="Complete a 10-question survey about news coverage and your opinions", reward=".20", duration=seconds(hours=1), keywords="survey, questionnaire, politics")
}
}
\keyword{HITs}