\name{GenerateHTMLQuestion}
\alias{GenerateHTMLQuestion}
\title{Generate HTMLQuestion}
\description{Generate an HTMLQuestion data structure for use in the \sQuote{Question} parameter of \code{\link{CreateHIT}}.}
\usage{
GenerateHTMLQuestion(character = NULL, file = NULL, frame.height = 450)
}
\arguments{
  \item{character}{An optional character string from which to construct the HTMLQuestion data structure.}
  \item{file}{An optional character string containing a filename from which to construct the HTMLQuestion data structure.}
  \item{frame.height}{A character string containing the integer value (in pixels) of the frame height for the HTMLQuestion iframe.}
}
\details{Must specify either \code{character} or \code{file}.}
\value{A list containing \code{xml.parsed}, an XML data structure, \code{string}, xml formatted as a character string, and \code{url.encoded}, character string containing a URL query parameter-formatted HTMLQuestion data structure for use in the \code{question} parameter of \code{\link{CreateHIT}}.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_HTMLQuestionArticle.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateHIT}}

\code{\link{GenerateExternalQuestion}}

\code{\link{GenerateHITLayoutParameter}}
}
\examples{
\dontrun{
GenerateHTMLQuestion(file="mynewhit.html")
}
}
\keyword{HITs}