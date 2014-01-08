\name{XMLToDataFrame}
%\alias{MTurkXMLToDataFrame}
\alias{AnswerKeyToDataFrame}
\alias{AssignmentsToDataFrame}
\alias{BonusPaymentsToDataFrame}
\alias{ExternalQuestionToDataFrame}
\alias{HITsToDataFrame}
\alias{HTMLQuestionToDataFrame}
\alias{QualificationRequestsToDataFrame}
\alias{QualificationRequirementsToDataFrame}
\alias{QualificationsToDataFrame}
\alias{QualificationTypesToDataFrame}
\alias{QuestionFormAnswersToDataFrame}
\alias{QuestionFormToDataFrame}
\alias{ReviewResultsToDataFrame}
\alias{WorkerBlockToDataFrame}
\title{Parse MTurk XML Responses and Convert to Dataframe}
\description{Parse MTurk XML Responses of specified types and convert the XML to an R dataframe.}
\usage{
AnswerKeyToDataFrame(xml=NULL, xml.parsed=NULL)
AssignmentsToDataFrame(xml = NULL, xml.parsed = NULL, return.assignment.xml = FALSE)
BonusPaymentsToDataFrame(xml = NULL, xml.parsed = NULL)
ExternalQuestionToDataFrame(xml = NULL, xml.parsed = NULL)
HITsToDataFrame(xml = NULL, xml.parsed = NULL, return.hit.xml = FALSE,
                return.qual.list = TRUE, sandbox = getOption('MTurkR.sandbox'))
HTMLQuestionToDataFrame(xml = NULL, xml.parsed = NULL)
QualificationRequestsToDataFrame(xml = NULL, xml.parsed = NULL)
QualificationRequirementsToDataFrame(xml = NULL, xml.parsed = NULL, xmlnodeset = NULL,
                                     hit = NULL, hit.number = NULL,
                                     sandbox = getOption('MTurkR.sandbox'))
QualificationsToDataFrame(xml = NULL, xml.parsed = NULL)
QualificationTypesToDataFrame(xml = NULL, xml.parsed = NULL)
QuestionFormAnswersToDataFrame(xml = NULL, xml.parsed = NULL)
QuestionFormToDataFrame(xml = NULL, xml.parsed = NULL)
ReviewResultsToDataFrame(xml = NULL, xml.parsed = NULL)
WorkerBlockToDataFrame(xml = NULL, xml.parsed = NULL)
}
\arguments{
  \item{xml}{A character string containing a full MTurk XML response. Must specify this or the \code{xml.parsed} parameter.}
  \item{xml.parsed}{A full MTurk XML response parsed by the \code{xmlParse}. Must specify this or the \code{xml} parameter.}
  \item{xmlnodeset}{An XML nodeset.}
  \item{return.assignment.xml}{A logical indicating whether workers' responses to HIT questions should be returned.}
  \item{return.hit.xml}{A logical indicating whether the HIT XML should be returned. Default is \code{FALSE}.}
  \item{return.qual.list}{A logical indicating whether the QualificationRequirement list should be returned. Default is \code{TRUE}.}
  \item{hit}{An optional parameter included for advanced users, to return only one of the specified HITs.}
  \item{hit.number}{An optional parameter included for advanced users, to return only one of the specified HITs.}
  \item{sandbox}{A logical indicating whether GetQualificationType, called internally, should query the sandbox for user-defined QualificationTypes.}
}
\details{Mostly internal functions to convert XML-formatted MTurk responses into more useful R dataframes. These are mostly internal to the extent that most users will never call them directly, but they may be useful if one needs to examine information stored in the MTurkR log file, or if \code{\link{genericmturkr}} is used.}
\value{A dataframe (or list of dataframes, in some cases) containing the request data.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_DataStructuresArticle.html}{API Reference: Data Structures}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
%\examples{}
