\name{SetHITAsReviewing}
\alias{SetHITAsReviewing}
\alias{reviewing}
\title{Set HIT as \dQuote{Reviewing}}
\description{Update the review status of a HIT, from \dQuote{Reviewable} to \dQuote{Reviewing} or the reverse.}
\usage{
SetHITAsReviewing(hit = NULL, hit.type = NULL, annotation = NULL,
                  revert = FALSE, 
                  verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{hit}{An optional character string containing a HITId, or a vector character strings containing HITIds, whose status are to be changed. Must specify \code{hit} xor \code{hit.type} xor \code{annotation}.}
  \item{hit.type}{An optional character string specifying a HITTypeId (or a vector of HITTypeIds), all the HITs of which should be set as \dQuote{Reviewing} (or the reverse). Must specify \code{hit} xor \code{hit.type} xor \code{annotation}.}
  \item{annotation}{An optional character string specifying the value of the \code{RequesterAnnotation} field for a batch of HITs. This can be used to set the review status all HITs from a \dQuote{batch} created in the online Requester User Interface (RUI). To use a batch ID, the batch must be written in a character string of the form \dQuote{BatchId:78382;}, where \dQuote{73832} is the batch ID shown in the RUI. Must specify \code{hit} xor \code{hit.type} xor \code{annotation}.}
  \item{revert}{An optional logical to revert the HIT from \dQuote{Reviewing} to \dQuote{Reviewable}.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
A function to change the status of one or more HITs (or all HITs of a given HITType) to \dQuote{Reviewing} or the reverse. This affects what HITs are returned by \code{\link{GetReviewableHITs}}. Must specify a HITId xor a HITTypeId.

\code{reviewing()} is an alias.
}
\value{A data frame containing HITId, status, and whether the request to change the status of each was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_SetHITAsReviewingOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GetReviewableHITs}}
}
\examples{
\dontrun{
a <- GenerateExternalQuestion("http://www.example.com/","400")
hit1 <- 
CreateHIT(hit.type="2FFNCWYB49F9BBJWA4SJUNST5OFSOW", question=a$string)
SetHITAsReviewing(hit1$HITId)

# cleanup
DisableHIT(hit1$HITId)
}
}
\keyword{HITs}
