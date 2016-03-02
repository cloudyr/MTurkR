\name{XML}
\alias{as.data.frame.MTurkResponse}
\alias{as.data.frame.AnswerKey}
\alias{as.data.frame.Assignments}
\alias{as.data.frame.BonusPayments}
\alias{as.data.frame.ExternalQuestion}
\alias{as.data.frame.HITs}
\alias{as.data.frame.HTMLQuestion}
\alias{as.data.frame.QualificationRequests}
\alias{as.data.frame.QualificationRequirements}
\alias{as.data.frame.Qualifications}
\alias{as.data.frame.QualificationTypes}
\alias{as.data.frame.QuestionFormAnswers}
\alias{as.data.frame.QuestionForm}
\alias{as.data.frame.ReviewResults}
\alias{as.data.frame.WorkerBlock}
\title{Parse MTurk XML to Data Frame}
\description{Parse MTurk XML Responses to R data.frames.}
\usage{
as.data.frame.AnswerKey(xml.parsed)
as.data.frame.Assignments(xml.parsed, 
                          return.assignment.xml = FALSE)
as.data.frame.BonusPayments(xml.parsed)
as.data.frame.ExternalQuestion(xml.parsed)
as.data.frame.HITs(xml.parsed, 
                   return.hit.xml = FALSE,
                   return.qual.list = TRUE, 
                   sandbox = getOption('MTurkR.sandbox'))
as.data.frame.HTMLQuestion(xml.parsed)
as.data.frame.QualificationRequests(xml.parsed)
as.data.frame.QualificationRequirements(xml.parsed = NULL, 
                                        xmlnodeset = NULL,
                                        hit.number = NULL,
                                        sandbox = getOption('MTurkR.sandbox'))
as.data.frame.Qualifications(xml.parsed)
as.data.frame.QualificationTypes(xml.parsed)
as.data.frame.QuestionFormAnswers(xml.parsed)
as.data.frame.QuestionForm(xml.parsed)
as.data.frame.ReviewResults(xml.parsed)
as.data.frame.WorkerBlock(xml.parsed)
}
\arguments{
  \item{xml.parsed}{A full MTurk XML response parsed by the \code{xmlParse}.}
  \item{xmlnodeset}{An XML nodeset.}
  \item{return.assignment.xml}{A logical indicating whether workers' responses to HIT questions should be returned.}
  \item{return.hit.xml}{A logical indicating whether the HIT XML should be returned. Default is \code{FALSE}.}
  \item{return.qual.list}{A logical indicating whether the QualificationRequirement list should be returned. Default is \code{TRUE}.}
  \item{hit}{An optional parameter included for advanced users, to return only one of the specified HITs.}
  \item{hit.number}{An optional parameter included for advanced users, to return only one of the specified HITs.}
  \item{sandbox}{A logical indicating whether GetQualificationType, called internally, should query the sandbox for user-defined QualificationTypes.}
}
\details{Mostly internal functions to convert XML-formatted MTurk responses into more useful R data frames. These are mostly internal to the extent that most users will never call them directly, but they may be useful if one needs to examine information stored in the MTurkR log file, or if \code{\link{request}} is used directly.}
\value{A data frame (or list of data frames, in some cases) containing the request data.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_DataStructuresArticle.html}{API Reference: Data Structures}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
%\examples{}
