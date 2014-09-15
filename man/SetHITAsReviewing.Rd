\name{SetHITAsReviewing}
\alias{SetHITAsReviewing}
\alias{reviewing}
\title{Set HIT as \dQuote{Reviewing}}
\description{Update the status of a HIT, from \dQuote{Reviewable} to \dQuote{Reviewing} or the reverse.}
\usage{
SetHITAsReviewing(hit = NULL, hit.type = NULL, revert = FALSE, 
                  verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{hit}{An optional chracter character string containing a HITId, or a vector character strings containing HITIds, whose status are to be changed.}
  \item{hit.type}{An optional character string specifying a HITTypeId (or a vector of HITTypeIds), all the HITs of which should be set as \dQuote{Reviewing} (or the reverse).}
  \item{revert}{An optional logical to revert the HIT from \dQuote{Reviewing} to \dQuote{Reviewable}.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
A function to change the status of one or more HITs (or all HITs of a given HITType) to \dQuote{Reviewing} or the reverse. This affects what HITs are returned by \code{\link{GetReviewableHITs}}. Must specify a HITId xor a HITTypeId.

\code{reviewing()} is an alias.
}
\value{A dataframe containing HITId, status, and whether the request to change the status of each was valid.}
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
SetHITAsReviewing("2MQB727M0IGF304GJ16S1F4VE3AYDQ")
}
}
\keyword{HITs}