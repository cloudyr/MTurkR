\name{ExpireHIT}
\alias{ExpireHIT}
\alias{expire}
\title{Expire HIT}
\description{Force a HIT to expire immediately, as opposed to at its prespecified expiration time. Expired HITs can be extended with the \code{\link{ExtendHIT}} operation.}
\usage{
ExpireHIT(hit = NULL, hit.type = NULL, annotation = NULL,
          verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{hit}{A character string containing a HITId or a vector of character strings containing multiple HITIds. Must specify \code{hit} xor \code{hit.type} xor \code{annotation}, otherwise all HITs are returned in \code{HITStatus}.}
  \item{hit.type}{An optional character string containing a HITTypeId (or a vector of HITTypeIds). Must specify \code{hit} xor \code{hit.type} xor \code{annotation}, otherwise all HITs are returned in \code{HITStatus}.}
  \item{annotation}{An optional character string specifying the value of the \code{RequesterAnnotation} field for a batch of HITs. This can be used to expire all HITs from a \dQuote{batch} created in the online Requester User Interface (RUI). To use a batch ID, the batch must be written in a character string of the form \dQuote{BatchId:78382;}, where \dQuote{73832} is the batch ID shown in the RUI. Must specify \code{hit} xor \code{hit.type} xor \code{annotation}, otherwise all HITs are returned in \code{HITStatus}.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
A function to (prematurely) expire a HIT (or multiple HITs), thereby preventing any additional assignments from being completed. Pending assignments can still be submitted. An expired HIT can be reactivated by adding additional time to its expiration using \code{\link{ExtendHIT}}.

\code{expire()} is an alias.
}
\value{A data frame containing the HITId(s) and whether each expiration request was valid.}
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
a <- GenerateExternalQuestion("http://www.example.com/","400")
hit1 <- 
CreateHIT(hit.type="2FFNCWYB49F9BBJWA4SJUNST5OFSOW", question = a$string)

# expire HIT
ExpireHIT(hit = hit1$HITId)

# Expire all HITs of a given batch from the RUI
ExpireHIT(annotation="BatchId:78382;")
}
}
\keyword{HITs}