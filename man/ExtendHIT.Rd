\name{ExtendHIT}
\alias{ExtendHIT}
\alias{extend}
\title{Extend HIT}
\description{Extend the time remaining on a HIT or the number of assignments available for the HIT.}
\usage{
ExtendHIT(hit = NULL, hit.type = NULL, annotation = NULL,
          add.assignments = NULL, add.seconds = NULL, 
          unique.request.token = NULL, verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{hit}{An optional character string containing a HITId or a vector of character strings containing multiple HITIds. Must specify \code{hit} xor \code{hit.type} xor \code{annotation}.}
  \item{hit.type}{An optional character string containing a HITTypeId (or a vector of HITTypeIds). Must specify \code{hit} xor \code{hit.type} xor \code{annotation}.}
  \item{annotation}{An optional character string specifying the value of the \code{RequesterAnnotation} field for a batch of HITs. This can be used to extend all HITs from a \dQuote{batch} created in the online Requester User Interface (RUI). To use a batch ID, the batch must be written in a character string of the form \dQuote{BatchId:78382;}, where \dQuote{73832} is the batch ID shown in the RUI. Must specify \code{hit} xor \code{hit.type} xor \code{annotation}.}
  \item{add.assignments}{An optional character string containing the number of assignments to add to the HIT. Must be between 1 and 1000000000.}
  \item{add.seconds}{An optional character string containing the amount of time to extend the HIT, in seconds (for example, returned by \code{\link{seconds}}). Must be between 1 hour (3600 seconds) and 365 days.}
  \item{unique.request.token}{An optional character string, included only for advanced users.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
A useful function for adding time and/or additional assignments to a HIT. If the HIT is already expired, this reactivates the HIT for the specified amount of time. If all assignments have already been submitted, this reactivates the HIT with the specified number of assignments and previously specified expiration. Must specify a HITId xor a HITTypeId. If multiple HITs or a HITTypeId are specified, each HIT is extended by the specified amount.

\code{extend()} is an alias.
}
\value{A data frame containing the HITId, assignment increment, time increment, and whether each extension request was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_ExtendHITOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateHIT}}

\code{\link{ExpireHIT}}

\code{\link{DisableHIT}}

\code{\link{DisposeHIT}}
}
\examples{
\dontrun{
a <- GenerateExternalQuestion("https://www.example.com/","400")
hit1 <- CreateHIT(title = "Example",
                  description = "Simple Example HIT",
                  reward = ".01",
                  expiration = seconds(days = 4),
                  duration = seconds(hours = 1),
                  keywords = "example",
                  question = a$string)

# add assignments
ExtendHIT(hit = hit1$HITId, add.assignments = "20")

# add time
ExtendHIT(hit = hit1$HITId, add.seconds = seconds(days=1)))

# add assignments and time
ExtendHIT(hit = hit1$HITId, add.assignments = "20", add.seconds = seconds(days=1)))

# cleanup
DisableHIT(hit = hit1$HITId)

}
\dontrun{
# Extend all HITs of a given batch from the RUI
ExtendHIT(annotation="BatchId:78382;", add.assignments = "20")
}
}
\keyword{HITs}
