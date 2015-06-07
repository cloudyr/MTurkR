\name{BulkCreate}
\alias{BulkCreate}
\alias{BulkCreateFromTemplate}
\alias{BulkCreateFromURLs}
\alias{BulkCreateFromHITLayout}
\title{Generate Multiple HITs}
\description{Generate multiple HITs, possibly from an HTML template file, HITLayout parameters, or a vector of External Question URLs.}
\usage{
BulkCreate(questions, annotation, verbose = FALSE, ...)

BulkCreateFromTemplate(template, input, annotation, type = "HTMLQuestion", 
                       frame.height = 450, verbose = FALSE, ...)

BulkCreateFromURLs(url, frame.height = 450, annotation, verbose = FALSE, ...)

BulkCreateFromHITLayout(hitlayoutid, input, annotation, verbose = FALSE, ...)
}
\arguments{
  \item{questions}{A character vector where each entry is a valid argument for the \code{question} argument to \code{\link{CreateHIT}}, or a list of objects of class \dQuote{ExternalQuestion} (created by \code{\link{GenerateExternalQuestion}}) or \code{HTMLQuestion} (created by \code{\link{GenerateHTMLQuestion}}). Each entry in this vector or list represents one HIT to be created.}
  \item{template}{A character string or filename for a HIT template (probably a character string containing an HTML document or a path to a .html file).}
  \item{input}{A data.frame containing one row for each HIT to be created and columns named identically to the placeholders in the HIT template file (for \code{BulkCreateFromTemplate}) or the HITLayout parameters (for \code{BulkCreateFromHITLayout}). Operation will fail if variable names do not correspond.}
  \item{type}{A character string specifying how to wrap the resulting HIT question contents for use in \code{\link{CreateHIT}}. If set to \dQuote{HTMLQuestion}, \code{template} is passed to \code{\link{GenerateHTMLQuestion}} before use.}
  \item{url}{A character vector of URLs (served over HTTPS) of HIT files stored anywhere other than the MTurk server. See \code{\link{GenerateExternalQuestion}}.}
  \item{frame.height}{A character string containing the integer value (in pixels) of the frame height for the ExternalQuestion iframe. See \code{\link{GenerateExternalQuestion}}.}
  \item{hitlayoutid}{An optional character string including a HITLayoutId retrieved from a HIT \dQuote{project} template generated in the Requester User Interface at \samp{https://requester.mturk.com/create/projects}. If the HIT template includes variable placeholders, must also specify \code{hitlayoutparameters}.}
  \item{annotation}{Either a one-element character vector containing a description for this group of HITs, or a character vector equal to the number of HITs to be created. This value is only visible to the requester. See \code{\link{CreateHIT}} for details.}
  \item{verbose}{Optionally print the results of the API request (and other details) to the standard output. Default is \code{FALSE}. Note that this overrides the default set by \code{getOption('MTurkR.verbose')} because in the case of many HITs, this output could become unwieldy.}
  \item{\dots}{Additional arguments passed to \code{\link{CreateHIT}}. See examples.}
}
\details{
These functions provide a wrapper for \code{\link{CreateHIT}} to be able to produce a group of HITs with identical properties. \code{BulkCreateFromTemplate} and \code{BulkCreateFromHITLayout} provide further wrappers that make it easy to create a group of HITs in a manner similar to using the Requester User Interface (RUI). \code{BulkCreateFromURLs} allows you to create multiple ExternalQuestion HITs.

The \code{annotation} field is required in order to group the HITs together and facilitate monitoring the group using other MTurkR functions. Note that these functions do not create a \dQuote{batch} as used by the RUI; a batch can only be created through that interface.
}
\value{A list of data.frames, with each data.frame containing details of the HITs created. If all \code{CreateHIT} operations succeed, this response value can easily be collapsed into a single data.frame using \code{do.call("rbind", value)}.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_CreateHITOperation.html}{API Reference: CreateHIT}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/RequesterUI/CreatingaHITTemplate.html}{Requester User Interface: HIT Template}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_ExternalQuestionArticle.html}{API Reference: ExternalQuestion}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateHIT}}

\code{\link{GenerateHITsFromTemplate}}

\code{\link{GenerateHITLayoutParameter}}
}
\examples{
\dontrun{
## BulkCreate ##

# load a vector of HTML files from the working directory
qvec <- sapply(list.files(pattern = ".html"), function(x) {
          paste0(readLines(x, warn = FALSE), collapse = "\n")
        })
# create a HIT from each question file
hits1 <- BulkCreate(questions = qvec,
                    annotation = paste("Bulk Create", Sys.Date()),
                    title = "Categorize an image",
                    description = "Categorize this image",
                    reward = ".05",
                    expiration = seconds(days = 4),
                    duration = seconds(minutes = 5),
                    keywords = "categorization, image, moderation, category")

# cleanup
ExpireHIT(annotation = paste("Bulk Create", Sys.Date()))
DisposeHIT(annotation = paste("Bulk Create", Sys.Date()))
}

\dontrun{
## BulkCreateFromURLs ##

# create three HITs from the template
hits2 <- 
BulkCreateFromURLs(url = paste0("https://www.example.com/",1:3,".html"),
                   frame.height = 400,
                   annotation = paste("Bulk From URLs", Sys.Date()),
                   title = "Categorize an image",
                   description = "Categorize this image",
                   reward = ".05",
                   expiration = seconds(days = 4),
                   duration = seconds(minutes = 5),
                   keywords = "categorization, image, moderation, category")

# cleanup
ExpireHIT(annotation = paste("Bulk From URLs", Sys.Date()))
DisposeHIT(annotation = paste("Bulk From URLs", Sys.Date()))
}

\dontrun{
## BulkCreateFromTemplate ##

# load template HTML file
# should have placeholders of the form `${varName}` for variable values
temp <- system.file("templates/htmlquestion2.xml", package = "MTurkR")

# create/load data.frame of template variable values
a <- data.frame(hittitle = c("HIT title 1", "HIT title 2", "HIT title 3"),
                hitvariable = c("HIT text 1", "HIT text 2", "HIT text 3"), 
                stringsAsFactors = FALSE)

# create three HITs from the template
hits3 <- 
BulkCreateFromTemplate(template = temp,
                       input = a,
                       annotation = paste("Bulk From Template", Sys.Date()),
                       title = "Categorize an image",
                       description = "Categorize this image",
                       reward = ".05",
                       expiration = seconds(days = 4),
                       duration = seconds(minutes = 5),
                       keywords = "categorization, image, moderation, category")

# cleanup
ExpireHIT(annotation = paste("Bulk From Template", Sys.Date()))
DisposeHIT(annotation = paste("Bulk From Template", Sys.Date()))
}

\dontrun{
## BulkCreateFromHITLayout ##

# retrieve HITLayoutID from Requester User Interface
layoutid <- "23ZGOOGQSCM61T1H5H9U0U00OQWFFU"

# create/load data.frame of HITLayout variable values
b <- data.frame(hittitle = c("HIT title 1", "HIT title 2", "HIT title 3"),
                hitvariable = c("HIT text 1", "HIT text 2", "HIT text 3"), 
                stringsAsFactors = FALSE)

# create three HITs from the template
hits4 <- 
BulkCreateFromHITLayout(hitlayoutid = layoutid,
                        input = b,
                        annotation = paste("Bulk From Layout", Sys.Date()),
                        title = "Categorize an image",
                        description = "Categorize this image",
                        reward = ".05",
                        expiration = seconds(days = 4),
                        duration = seconds(minutes = 5),
                        keywords = "categorization, image, moderation, category")

# cleanup
ExpireHIT(annotation = paste("Bulk From Layout", Sys.Date()))
DisposeHIT(annotation = paste("Bulk From Layout", Sys.Date()))
}
}
\keyword{HITs}
