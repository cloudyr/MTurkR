\name{GetFileUpload}
\alias{GetFileUpload}
\alias{geturls}
\title{Get Files Uploaded by Workers}
\description{Get the URL for a file uploaded by a worker as part of a HIT, or download the file(s) directly to the working directory.}
\usage{
GetFileUpload(assignment, questionIdentifier, download = FALSE, 
              open.file.in.browser = FALSE, 
              verbose = getOption('MTurkR.verbose', TRUE), 
              ...)
}
\arguments{
  \item{assignment}{A character string containing an AssignmentId, or a vector of character strings each containing an AssignmentId.}
  \item{questionIdentifier}{A question identifer for a file upload question, as specified in the \code{question} parameter of \code{CreateHIT} or in the placeholder of a HIT template created in the RUI.}
  \item{download}{A logical specifying whether the file(s) should be downloaded and saved in the working directory. Default is \code{FALSE}.}
  \item{open.file.in.browser}{A logical specifying whether the file should be opened in the user's default web browser.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
Note that a FileUploadURL is only valid for 60 seconds (per MTurk documentation), so URLs should either be retrieved one at a time or files should be automatically downloaded to the working directory with the \code{download = TRUE}. If \code{browser = TRUE}, request is executed in the user's default web browser, whereas if \code{open.file.in.browser = TRUE}, the request is executed in R and the file itself is opened in the browser.

If downloaded, files are saved in the local directory with names of the form \dQuote{QuestionIdentifer_AssignmentId_UploadedFileName.UploadedFileExtension}. One may want to use \code{setwd} to move to an appropriate directory before initiating this operation with \code{download = TRUE}.

\code{geturls()} is an alias.
}
\value{A dataframe containing the AssignmentId, questionIdenifier, temporary file URL, and an indicator of whether each request was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GetFileUploadURLOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
\dontrun{
a <- GenerateExternalQuestion("http://www.example.com/","400")
hit1 <- 
CreateHIT(title = "Upload a file",
          description = "Upload a file",
          reward = ".10",
          duration = seconds(days=1),
          keywords = "file, upload",
          question = a$string)
ExpireHIT(hit1$HITId)
a <- GetAssignments(hit = hit1$HITId)
f <- GetFileUpload(a, "dictation", download = TRUE)

# cleanup
DisposeHIT(hit1$HITId)
}
}
\keyword{Assignments}