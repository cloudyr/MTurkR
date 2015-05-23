\name{GenerateHITLayoutParameter}
\alias{GenerateHITLayoutParameter}
\title{Generate a HITLayout Parameter}
\description{Generate a HITLayout parameter based upon the names of HIT template variables and the values to substitute for those variables in a single HIT. Used in collaboration with a HIT Layout ID from \samp{https://requester.mturk.com/create/projects} in the \code{\link{CreateHIT}} operation.}
\usage{
GenerateHITLayoutParameter(names, values)
}
\arguments{
  \item{names}{A character string containing the name of a HIT template variable or a vector of character strings containing the names of multiple HIT template variables. This is optional if \code{values} has a non-empty names attribute.}
  \item{values}{A character string containing the value of a HIT template variable to be inserted for a specific HIT or a vector of character strings containing the values of multiple HIT template variables to be inserted for a specific HIT. If \code{values} has a non-empty names attribute and \code{names} is missing, the names attribute is used.}
}
\details{This function provides the content for the \code{hitlayoutparameters} option of \code{\link{CreateHIT}}. Specifically, a HIT Template created in the MTurk Requester User Interface (RUI) has a number of placeholder variables for content to be inserted. This fucntion supplies the content to be inserted into the template for one HIT. If multiple HITs are being created from one template, then \code{GenerateHITLayoutParameter} should be run once for each HIT.

Analogous functionality for producing .html files on the local workstation (e.g., to create multiple external HITs from the same template) is provided by \code{\link{GenerateHITsFromTemplate}}.
}
\value{A character string containing URL query parameter-formatted HITLayout parameters, to be used in the \code{hitlayoutparameters} parameter of \code{\link{CreateHIT}}.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_HITLayoutArticle.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateHIT}}

\code{\link{GenerateExternalQuestion}}

\code{\link{GenerateHITsFromTemplate}}
}
\examples{
\dontrun{
# examples of specifying 'names' and 'values'
a <- GenerateHITLayoutParameter(names = "hitvariable", 
                                values = "Text for HIT 1")
b <- GenerateHITLayoutParameter(names = "hitvariable", 
                                values = "Text for HIT 2")
c <- GenerateHITLayoutParameter(names = c("hitvariable1","hitvariable2"),
                                values = c("Headline for HIT1","Text for HIT 1"))

# example using a named character string in lieu of specifying 'names'
d <- GenerateHITLayoutParameter(values = c(hitvariable1 = "Headline for HIT1", 
                                           hitvariable2 = "Text for HIT 1"))

# create HIT using layout parameter
hit1 <- 
CreateHIT(title = "Survey",
          description = "5 question survey",
          reward = ".10",
          expiration = seconds(days=4),
          duration = seconds(hours = 1),
          keywords = "survey, questionnaire",
            # retrieved from MTurk web interface:
          hitlayoutid = "23ZGOOGQSCM61T1H5H9U0U00OQWFFU", 
          hitlayoutparameters = a)

# cleanup
DisableHIT(hit1$HITId)
}
}
\keyword{HITs}