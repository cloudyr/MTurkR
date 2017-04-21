\name{CreateHIT}
\alias{CreateHIT}
\alias{createhit}
\alias{create}
\title{Create HIT}
\description{Create a single HIT. This is the most important function in the package. It creates a HIT based upon the specified parameters: (1) characteristics inherited from a HITType or specification of those parameters and (2) some kind of Question data structure. Use \code{\link{BulkCreate}} to create multiple HITs with shared properties.}
\usage{
CreateHIT(hit.type = NULL, question = NULL, validate.question = FALSE,
          expiration, assignments = "1", 
          assignment.review.policy = NULL, hit.review.policy = NULL, 
          annotation = NULL, unique.request.token = NULL, 
          title = NULL, description = NULL, reward = NULL, duration = NULL,
          keywords = NULL, 
          auto.approval.delay = NULL, qual.req = NULL, 
          hitlayoutid = NULL, hitlayoutparameters = NULL, 
          response.group = NULL, verbose = getOption('MTurkR.verbose', TRUE), ...)
}
\arguments{
  \item{hit.type}{An optional character string specifying the HITTypeId that this HIT should be visibly grouped with (and whose properties, e.g. reward amount, this HIT should inherit).}
  \item{question}{A mandatory (unless layoutid is specified) character string containing a QuestionForm, HTMLQuestion, or ExternalQuestion data structure. In lieu of a question parameter, a \code{hitlayoutid} and, optionally, \code{hitlayoutparameters} can be specified.}
  \item{validate.question}{A logical specifying whether the \code{question} parameter should be validated against the relevant MTurk schema prior to creating the HIT (operation will fail if it does not validate, and will return validation information). Default is \code{FALSE}.}
  \item{expiration}{The time (in seconds) that the HIT should be available to workers. Must be between 30 and 31536000 seconds.}
  \item{assignments}{A character string specifying the number of assignments}
  \item{assignment.review.policy}{An optional character string containing an Assignment-level ReviewPolicy data structure as returned by \code{\link{GenerateAssignmentReviewPolicy}}.}
  \item{hit.review.policy}{An optional character string containing a HIT-level ReviewPolicy data structure as returned by \code{\link{GenerateHITReviewPolicy}}.}
  \item{annotation}{An optional character string annotating the HIT. This is not visible to workers, but can be used as a label by which to identify the HIT from the API.}
  \item{unique.request.token}{An optional character string, included only for advanced users. It can be used to prevent creating a duplicate HIT. A HIT will not be creatd if a HIT was previously granted (within a short time window) using the same \code{unique.request.token}.}
  \item{title}{A character string containing the title for the HITType. All HITs of this HITType will be visibly grouped to workers according to this title. Maximum of 128 characters.}
  \item{description}{A character string containing a description of the HITType. This is visible to workers. Maximum of 2000 characters.}
  \item{reward}{A character string containing the per-assignment reward amount, in U.S. Dollars (e.g., \dQuote{0.15}).}
  \item{duration}{A character string containing the amount of time workers have to complete an assignment for HITs of this HITType, in seconds (for example, as returned by \code{\link{seconds}}). Minimum of 30 seconds and maximum of 365 days.}
  \item{keywords}{An optional character string containing a comma-separated set of keywords by which workers can search for HITs of this HITType. Maximum of 1000 characters.}
  \item{auto.approval.delay}{An optional character string specifying the amount of time, in seconds (for example, as returned by \code{\link{seconds}}), before a submitted assignment is automatically granted. Maximum of 30 days.}
  \item{qual.req}{An optional character string containing one or more QualificationRequirements data structures, for example as returned by \code{\link{GenerateQualificationRequirement}}.}
  \item{hitlayoutid}{An optional character string including a HITLayoutId retrieved from a HIT \dQuote{project} template generated in the Requester User Interface at \samp{https://requester.mturk.com/create/projects}. If the HIT template includes variable placeholders, must also specify \code{hitlayoutparameters}.}
  \item{hitlayoutparameters}{An optional character string containing URL query parameter-formatted HITLayout parameters, for example returned by \code{\link{GenerateHITLayoutParameter}}. Must be specified along with a \code{hitlayoutid}.}
  \item{response.group}{An optional character string (or vector of character strings) specifying what details of each HIT to return of: \dQuote{Request}, \dQuote{Minimal}, \dQuote{HITDetail}, \dQuote{HITQuestion}, \dQuote{HITAssignmentSummary}. For more information, see \href{http://docs.aws.amazon.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_CommonParametersArticle.html}{Common Parameters} and \href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_HITDataStructureArticle.html}{HIT Data Structure}.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MTurkR.verbose', TRUE)}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{This function creates a new HIT and makes it available to workers. Characteristics of the HIT can either be specified by including a valid HITTypeId for \dQuote{hit.type} or creating a new HITType by atomically specifying the characteristics of a new HITType.

When creating a HIT, some kind of Question data structure must be specified. Either, a QuestionForm, HTMLQuestion, or ExternalQuestion data structure can be specified for the \code{question} parameter or, if a HIT template created in the Requester User Interface (RUI) is being used, the appropriate \code{hitlayoutid} can be specified. If the HIT template contains variable placeholders, then the \code{hitlayoutparameters} should also be specified.

When creating a ExternalQuestion HITs, the \code{\link{GenerateHITsFromTemplate}} function can emulate the HIT template functionality by converting a template .html file into a set of individual HIT .html files (that would also have to be uploaded to a web server) and executing \code{CreateHIT} for each of these external files with an appropriate ExternalQuestion data structure specified for the \code{question} parameter.

\code{createhit()} and \code{create()} are aliases. \code{\link{BulkCreate}} can be used to create multiple HITs in a single call.
}
\value{A data frame containing the HITId and other details of the newly created HIT.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_CreateHITOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{BulkCreate}}

\code{\link{ExtendHIT}}

\code{\link{ExpireHIT}}

\code{\link{DisableHIT}}

\code{\link{DisposeHIT}}

\code{\link{RegisterHITType}}

\code{\link{GenerateHITReviewPolicy}}

\code{\link{GenerateQualificationRequirement}}
}
\examples{
\dontrun{
## CreateHIT using HITLayout from MTurk Requester User Interface ##
a <- GenerateHITLayoutParameter("message","Text to display in HIT")
hit1 <- CreateHIT(hit.type = "2FFNCWYB49F9BBJWA4SJUNST5OFSOW",
                  hitlayoutid = "23ZGOOGQSCM61T1H5H9U0U00OQWFFU", 
                  expiration = seconds(days = 4),
                  hitlayoutparameters = a)

## CreateHIT using ExternalQuestion HIT URL ##
eq <- GenerateExternalQuestion("https://www.example.com/","400")

### Specifying a HITTypeId ###
hit2 <- CreateHIT(hit.type = "2FFNCWYB49F9BBJWA4SJUNST5OFSOW", 
                  expiration = seconds(days = 4),
                  question = eq$string)

### Creating a new HITTypeId atomically ###
hit3 <- CreateHIT(title = "Survey",
                  description = "5 question survey",
                  reward = ".10",
                  expiration = seconds(days = 4),
                  duration = seconds(hours = 1),
                  keywords = "survey, questionnaire",
                  question = eq$string)

## CreateHIT using HTMLQuestion HIT Contents ##
f1 <- system.file("templates/htmlquestion1.xml", package = "MTurkR")
hq <- GenerateHTMLQuestion(file = f1)
hit4 <- CreateHIT(hit.type = "2FFNCWYB49F9BBJWA4SJUNST5OFSOW", 
                  expiration = seconds(days = 4),
                  question = hq$string)

## CreateHIT using QuestionForm HIT Contents ##
f2 <- system.file("templates/tictactoe.xml", package = "MTurkR")
qf <- GenerateHTMLQuestion(file = f2)
hit5 <- CreateHIT(hit.type = "2FFNCWYB49F9BBJWA4SJUNST5OFSOW", 
                  expiration = seconds(days = 4),
                  question = qf$string)


## Cleanup examples ##
ExpireHIT(hit1$HITId)
ExpireHIT(hit2$HITId)
ExpireHIT(hit3$HITId)
ExpireHIT(hit4$HITId)
ExpireHIT(hit5$HITId)
DisposeHIT(hit1$HITId)
DisposeHIT(hit2$HITId)
DisposeHIT(hit3$HITId)
DisposeHIT(hit4$HITId)
DisposeHIT(hit5$HITId)
}
}
\keyword{HITs}
