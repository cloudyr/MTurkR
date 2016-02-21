\name{GenerateExternalQuestion}
\alias{GenerateExternalQuestion}
\title{Generate ExternalQuestion}
\description{Generate an ExternalQuestion data structure for use in the \sQuote{Question} parameter of the \code{\link{CreateHIT}} operation.}
\usage{
GenerateExternalQuestion(url, frame.height = 400)
}
\arguments{
  \item{url}{A character string containing the URL (served over HTTPS) of a HIT file stored anywhere other than the MTurk server.}
  \item{frame.height}{A character string containing the integer value (in pixels) of the frame height for the ExternalQuestion iframe.}
}
\details{An ExternalQuestion is a HIT stored anywhere other than the MTurk server that is displayed to workers within an HTML iframe of the specified height. The URL should point to a page --- likely an HTML form --- that can retrieve several URL GET parameters for \dQuote{AssignmentId} and \dQuote{WorkerId}, which are attached by MTurk when opening the URL. The page should also be able to submit those parameters plus any assignment data to \url{https://www.mturk.com/mturk/externalSubmit} (for the live MTurk site) or \url{https://workersandbox.mturk.com/mturk/externalSubmit} (for the sandbox site), using either the HTTP GET or POST methods.

Note: \code{url} must be HTTPS. See \href{http://en.wikipedia.org/wiki/HTTP_Secure}{Wikipedia:HTTP Secure} for details.
}
\value{A list containing \code{xml.parsed}, an XML data structure, \code{string}, xml formatted as a character string, and \code{url.encoded}, character string containing a URL query parameter-formatted HTMLQuestion data structure for use in the \code{question} parameter of \code{\link{CreateHIT}}.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_ExternalQuestionArticle.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateHIT}}

\code{\link{GenerateHITLayoutParameter}}
}
\examples{
\dontrun{
a <- GenerateExternalQuestion(url="http://www.example.com/", frame.height="400")

hit1 <- 
CreateHIT(title = "Survey",
          description = "5 question survey",
          reward = ".10",
          expiration = seconds(days = 4),
          duration = seconds(hours = 1),
          keywords = "survey, questionnaire",
          question = a$string)

ExpireHIT(hit1$HITId)
DisposeHIT(hit1$HITId)
}
}
\keyword{HITs}
