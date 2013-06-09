\name{GetFileUpload}
\alias{GetFileUpload}
\alias{geturls}
\title{Get Files Uploaded by Workers}
\description{Get the URL for a file uploaded by a worker as part of a HIT, or download the file(s) directly to the working directory.}
\usage{
GetFileUpload(assignment, questionIdentifier, download = FALSE, file.ext = NULL, 
				open.file.in.browser = FALSE, keypair = credentials(), print = TRUE, 
				browser = FALSE, log.requests = TRUE, sandbox = FALSE)
}
\arguments{
  \item{assignment}{A character string containing an AssignmentId, or a vector of character strings each containing an AssignmentId.}
  \item{questionIdentifier}{A question identifer for a file upload question, as specified in the \code{question} parameter of \code{CreateHIT} or in the placeholder of a HIT template created in the RUI.}
  \item{download}{A logical specifying whether the file(s) should be downloaded and saved in the working directory. Default is \code{FALSE}.}
  \item{file.ext}{An optional character string specifying the file extension of the uploaded files (used only if \code{download=TRUE}).}
  \item{open.file.in.browser}{A logical specifying whether the file should be opened in the user's default web browser.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
}
\details{
Note that a FileUploadURL is only valid for 60 seconds (per MTurk documentation), so URLs should either be retrieved one at a time or files should be automatically downloaded to the working directory with the \code{download=TRUE}. If \code{browser=TRUE}, request is executed in the user's default web browser, whereas if \code{open.file.in.browser=TRUE}, the request is executed in R and the file itself is opened in the browser.

\code{geturls()} is an alias.
}
\value{Depending on the specification, either a character string containing a temporary URL (which lasts 60 seconds) where the uploaded file can be downloaded, or the files themselves are opened in the user's default web browser, or directly downloaded and saved to the working directory.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GetFileUploadURLOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
\dontrun{
GetFileUpload("26XXH0JPPSI23H54YVG7BKLO82DHNU","dictation",download=TRUE)
}
}
\keyword{Assignments}