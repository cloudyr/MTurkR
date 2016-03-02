\name{GenerateHITsFromTemplate}
\alias{GenerateHITsFromTemplate}
\title{Generate HITs from a Template}
\description{Generate individual HIT .html files from a local .html HIT template file, in the same fashion as the MTurk Requester User Interface (RUI).}
\usage{
GenerateHITsFromTemplate(template, input, filenames = NULL, write.files = FALSE)
}
\arguments{
  \item{template}{A character string or filename for an .html HIT template}
  \item{input}{A data.frame containing one row for each HIT to be created and columns named identically to the placeholders in the HIT template file. Operation will fail if variable names do not correspond.}
  \item{filenames}{An optional list of filenames for the HITs to be created. Must be equal to the number of rows in \code{input}.}
  \item{write.files}{A logical specifying whether HIT .html files should be created and stored in the working directory. Or, alternatively, whether HITs should be returned as character vectors in a list.}
  \item{\dots}{Additional arguments passed to \code{\link{CreateHIT}}.}
}
\details{
\code{GenerateHITsFromTemplate} generates individual HIT question content from a HIT template (containing placeholders for input data of the form \code{\$\{variablename\}}). The tool provides functionality analogous to the MTurk RUI HIT template and can be performed on .html files generated therein. The HITs are returned as a list of character strings. If \code{write.files = TRUE}, a side effect occurs in the form of one or more .html files being written to the working directory, with filenames specified by the \code{filenames} option or, if \code{filenames=NULL} of the form \dQuote{NewHIT1.html}, \dQuote{NewHIT2.html}, etc.
}
\value{A list containing a character string for each HIT generated from the template.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/RequesterUI/CreatingaHITTemplate.html}{API Reference: Operation}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_ExternalQuestionArticle.html}{API Reference: ExternalQuestion Data Structure}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{\code{\link{BulkCreateFromTemplate}}}
\examples{
\dontrun{
# create/edit template HTML file
# should have placeholders of the form `${varName}` for variable values
temp <- system.file("templates/htmlquestion2.xml", package = "MTurkR")
readLines(temp)

# create/load data.frame of template variable values
a <- data.frame(hittitle = c("HIT title 1","HIT title 2","HIT title 3"),
                hitvariable = c("HIT text 1","HIT text 2","HIT text 3"), 
                stringsAsFactors=FALSE)

# create HITs from template and data.frame values
temps <- GenerateHITsFromTemplate(template = temp, input = a)

# create HITs from template
hittype1 <- RegisterHITType(title = "2 Question Survey",
              description = "Complete a 2-question survey",
              reward = ".20", 
              duration = seconds(hours=1), 
              keywords = "survey, questionnaire, politics")
hits <- lapply(temps, function(x) {
    CreateHIT(hit.type = hittype1$HITTypeId, 
              expiration = seconds(days = 1),
              assignments = 2,
              question = GenerateHTMLQuestion(x)$string)
})

# cleanup
ExpireHIT(hit.type = hittype1$HITTypeId)
DisposeHIT(hit.type = hittype1$HITTypeId)
}
}
\keyword{HITs}
