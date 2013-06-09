\name{GenerateExternalQuestion}
\alias{GenerateExternalQuestion}
\title{Generate ExternalQuestion}
\description{Generate an ExternalQuestion data structure for use in the \sQuote{Question} parameter of the \code{\link{CreateHIT}} operation.}
\usage{
GenerateExternalQuestion(url, frame.height)
}
\arguments{
  \item{url}{A character string containing the URL of a HIT file stored anywhere other than the MTurk server.}
  \item{frame.height}{A character string containing the integer value (in pixels) of the frame height for the ExternalQuestion iframe.}
}
\details{An ExternalQuestion is a HIT stored anywhere other than the MTurk server that is displayed to workers within an iframe of the specified height. The URL should point to a page --- likely an HTML form --- that can retrieve several URL GET parameters for \dQuote{AssignmentId} and \dQuote{WorkerId}, which are attached by MTurk when opening the URL. The page should also be able to submit those parameters plus any assignment data to \url{https://www.mturk.com/mturk/externalSubmit} using either the HTTP GET or POST methods.}
\value{A character string containing a URL query parameter-formatted ExternalQuestion data structure.}
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
GenerateExternalQuestion(url="http://www.example.com/myhit.html",frame.height="400")
}
}
\keyword{HITs}