\name{GenerateHITsFromTemplate}
\alias{GenerateHITsFromTemplate}
\alias{hitsfromtemplate}
\title{Generate HITs from a Template}
\description{Generate individual HIT .html files from a local .html HIT template file, in the same fashion as the MTurk Requester User Interface (RUI). This function facilitates the use of ExternalQuestion data structures.}
\usage{
GenerateHITsFromTemplate(template, input, filenames = NULL, write.files = FALSE)
}
\arguments{
  \item{template}{A file handle for an .html HIT template saved in the working directory.}
  \item{input}{A dataframe containing one row for each HIT to be created and columns named identically to the placeholders in the HIT template file. Operation will fail if variable names do not correspond.}
  \item{filenames}{An optional list of filenames for the HITs to be created. Must be equal to the number of rows in \code{input}.}
  \item{write.files}{A logical specifying whether HIT .html files should be created and stored in the working directory. Or, alternatively, whether HITs should be returned as character vectors in a list.}
}
\details{
Generate HITs from a HIT template (containing placeholders for input data). The tool provides functionality analogous to the MTurk RUI HIT template and can be performed on .html files generated therein.

\code{hitsfromtemplate()} is an alias.
}
\value{If \code{write.files=FALSE}, a list containing a character string for each HIT generated from the template. Otherwise, one or more HITs in the form of .html files stored in the working directory, with filenames specified by the \code{filenames} option or, if \code{filenames=NULL} of the form \dQuote{NewHIT1.html}, \dQuote{NewHIT2.html}, etc.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/RequesterUI/CreatingaHITTemplate.html}{API Reference: Operation}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_ExternalQuestionArticle.html}{API Reference: ExternalQuestion Data Strucure}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
\dontrun{
a <- data.frame(c("HIT text 1","HIT text 2","HIT text 3"))
names(a) <- "hitvariable"
GenerateHITsFromTemplate("mynewhit.html",input=a,write.files=TRUE)
}
}
\keyword{HITs}