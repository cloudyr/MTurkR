\name{BulkCreate}
\alias{BulkCreate}
\alias{BulkCreateFromTemplate}
\alias{BulkCreateFromHITLayout}
\title{Generate Multiple HITs}
\description{Generate multiple HITs, possibly from an HTML template file or HITLayout parameters.}
\usage{
BulkCreate(questions, annotation, ...)

BulkCreateFromTemplate(template, input, type = "HTMLQuestion", annotation, ...)

BulkCreateFromHITLayout(hitlayoutid, parameters, annotation, ...)
}
\arguments{
  \item{template}{A character string or filename for an HIT template (probably a character string containing an HTML document or a path to a .html file).}
  \item{input}{A data.frame containing one row for each HIT to be created and columns named identically to the placeholders in the HIT template file. Operation will fail if variable names do not correspond.}
  \item{type}{A character string specifying how to wrap the resulting HIT question contents for use in \code{\link{CreateHIT}}. If set to \dQuote{HTMLQuestion}, \code{template} is passed to \code{\link{GenerateHTMLQuestion}} before use.}
  \item{hitlayoutid}{An optional character string including a HITLayoutId retrieved from a HIT template generated in the Requester User Interface, \href{https://requester.mturk.com/hit_templates}{HIT Templates}. If the HIT template includes variable placeholders, must also specify \code{hitlayoutparameters}.}
  \item{parameters}{A list containing a named character string, where each element is a value and its corresponding name is the name of a HITLayout parameter field. See examples.}
  \item{annotation}{Either a one-element character vector containing a description for this group of HITs, or a character vector equal to the number of HITs to be created. This value is only visible to the requester. See \code{\link{CreateHIT}} for details.}
  \item{\dots}{Additional arguments passed to \code{\link{CreateHIT}}.}
}
\details{These functions provide a wrapper for \code{\link{CreateHIT}} to be able to produce a group of HITs with identical properties. \code{BulkCreateFromTemplate} and \code{BulkCreateFromHITLayout} provide further wrappers that make it easy to create a group of HITs in a manner similar to using the Requester User Interface (RUI). The \code{annotation} field is required in order to group the HITs together and facilitate monitoring the group using other MTurkR functions. Note that these functions do not create a \dQuote{batch} as used by the RUI; a batch can only be created through that interface.}
\value{A list containing each HIT created.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/RequesterUI/CreatingaHITTemplate.html}{API Reference: Operation}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_ExternalQuestionArticle.html}{API Reference: ExternalQuestion Data Strucure}
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
# BulkCreate
}

\dontrun{
# BulkCreateFromTemplate
}

\dontrun{
# BulkCreateFromHITLayout
}
}
\keyword{HITs}
