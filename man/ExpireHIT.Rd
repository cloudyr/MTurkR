\name{ExpireHIT}
\alias{ExpireHIT}
\alias{expire}
\title{Expire HIT}
\description{Force a HIT to expire immediately, as opposed to at its prespecified expiration time. Expired HITs can be extended with the \code{\link{ExtendHIT}} operation.}
\usage{
ExpireHIT(hit = NULL, hit.type = NULL, verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{hit}{A character string containing a HITId or a vector of character strings containing multiple HITIds.}
  \item{hit.type}{An optional character string containing a HITTypeId (or a vector of HITTypeIds).}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
A function to (prematurely) expire a HIT (or multiple HITs), thereby preventing any additional assignments from being completed. Pending assignments can still be submitted. An expired HIT can be reactived by adding additional time to its expiration using \code{\link{ExtendHIT}}.

\code{expire()} is an alias.
}
\value{A dataframe containing the HITId(s) and whether each expiration request was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_ForceExpireHITOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateHIT}}

\code{\link{ExtendHIT}}

\code{\link{DisableHIT}}

\code{\link{DisposeHIT}}
}
\examples{
\dontrun{
ExpireHIT(hit="2MQB727M0IGF304GJ16S1F4VE3AYDQ")
}
}
\keyword{HITs}