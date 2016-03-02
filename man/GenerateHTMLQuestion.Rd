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
\details{
Must specify either \code{character} or \code{file}.

To be valid, an HTMLQuestion data structure must be a complete XHTML document, including doctype declaration, head and body tags, and a complete HTML form (including the \code{form} tag with a submit URL, the assignmentId for the assignment as a form field, at least one substantive form field (can be hidden), and a submit button that posts to the external submit URL; see \code{\link{GenerateExternalQuestion}}.). If you fail to include a complete \code{form}, workers will be unable to submit the HIT. See the API Documentation for a complete example.

MTurkR comes pre-installed with several simple examples of HTMLQuestion HIT templates, which can be found by examining the \samp{templates} directory of the installed package directory. These examples include simple HTMLQuestion forms, as well as templates for categorization, linking to off-site surveys, and sentiment analysis. Note that the examples, while validated complete, do not include CSS styling.
}
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
f <- system.file("templates/htmlquestion1.xml", package = "MTurkR")
a <- GenerateHTMLQuestion(file=f)

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
