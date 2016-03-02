\name{DisableHIT}
\alias{DisableHIT}
\alias{disable}
\title{Disable HIT}
\description{Disabling a HIT is probably not what you want to do. DisableHIT automatically removes the HIT from the MTurk server, approves (and thus pays for) all submitted and pending assignments, and then permanently deletes all assignment data.}
\usage{
DisableHIT(hit = NULL, hit.type = NULL, annotation = NULL,
           response.group = NULL, 
           verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{hit}{A character string containing a HITId or a vector of character strings containing multiple HITIds. Must specify \code{hit} xor \code{hit.type} xor \code{annotation}.}
  \item{hit.type}{An optional character string containing a HITTypeId (or a vector of HITTypeIds). Must specify \code{hit} xor \code{hit.type} xor \code{annotation}.}
  \item{annotation}{An optional character string specifying the value of the \code{RequesterAnnotation} field for a batch of HITs. This can be used to disable all HITs from a \dQuote{batch} created in the online Requester User Interface (RUI). To use a batch ID, the batch must be written in a character string of the form \dQuote{BatchId:78382;}, where \dQuote{73832} is the batch ID shown in the RUI. Must specify \code{hit} xor \code{hit.type} xor \code{annotation}.}
  \item{response.group}{An optional character string specifying what details of each HIT to return of: \dQuote{Minimal}, \dQuote{HITQuestion}, \dQuote{HITDetail}, \dQuote{HITAssignmentSummary}. For more information, see \href{http://docs.aws.amazon.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_CommonParametersArticle.html}{Common Parameters} and \href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_HITDataStructureArticle.html}{HIT Data Structure}.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
Disable a HIT (and its assignment data). This is a somewhat risky function because it automatically approves all pending assignments and then disposes of everything. \code{\link{DisposeHIT}} is probably what most users will use to delete HIT and assignment data that is no longer needed.

\code{disable()} is an alias.
}
\value{A data frame containing a list of HITs and whether the request to disable each of them was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_DisableHITOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateHIT}}

\code{\link{ExtendHIT}}

\code{\link{ExpireHIT}}

\code{\link{DisposeHIT}}
}
\examples{
\dontrun{
# Disable a single HIT
b <- GenerateExternalQuestion("http://www.example.com/","400")
hit1 <- CreateHIT(hit.type="2FFNCWYB49F9BBJWA4SJUNST5OFSOW", 
                  expiration = seconds(days = 1),
                  question=b$string)
DisableHIT(hit = hit1$HITId)

# Disable all HITs of a given HITType
DisableHIT(hit.type = hit1$HITTypeId)

# Disable all HITs of a given batch from the RUI
DisableHIT(annotation="BatchId:78382;")
}
}
\keyword{HITs}