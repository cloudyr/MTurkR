\name{GenerateAnswerKey}
\alias{GenerateAnswerKey}
\alias{AnswerKeyTemplate}
\title{Generate AnswerKey Data Structure}
\description{Generate an AnswerKey data structure for a Qualification test.}
\usage{
GenerateAnswerKey(questions, scoring = NULL)
AnswerKeyTemplate(xml.parsed = NULL)
}
\arguments{
  \item{questions}{A data frame containing QuestionIdentifiers, AnswerOptions, AnswerScores, and DefaultScores. See MTurk API Documentation.}
  \item{scoring}{An optional list containing QualificationValueMapping information. See MTurk API Documentation.}
  \item{xml.parsed}{A complete QuestionForm data structure parsed by \code{xmlParse}. Must specify this or the \code{xml} parameter.}
}
\details{GenerateAnswerKey creates an AnswerKey data structure (possibly from a template created by AnswerKeyTemplate from a QuestionForm data structure), which serves to automatically score a Qualification test, as specified in the \code{test} parameter of \code{\link{CreateQualificationType}}. An AnswerKey data structure is also returned by \code{\link{GetQualificationType}}.}
\value{
GenerateAnswerKey returns a list containing an AnswerKey data structure as a parsed XML tree, character string containing that tree, and a url-encoded character string.

AnswerKeyTemplate returns a list that can be used in the \code{questions} parameter of GenerateAnswerKey. Placeholders are left for AnswerScore values to be manually entered prior to using it in GenerateAnswerKey.
}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_AnswerKeyDataStructureArticle.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateQualificationType}}
}
\examples{
\dontrun{
# generate an AnswerKey from a list of arguments
qs <- list(list(QuestionIdentifier = "Question1",
                AnswerOption = list(SelectionIdentifier="A", AnswerScore=15),
                AnswerOption = list(SelectionIdentifier="B", AnswerScore=10),
                DefaultScore = 5),
           list(QuestionIdentifier = "Question2",
                AnswerOption = list(SelectionIdentifier="D", AnswerScore=10) ) )

scoring1 <- list(PercentageMapping=5)

scoring2 <- list(RangeMapping=list(list(InclusiveLowerBound=0,
                                        InclusiveUpperBound=20,
                                        QualificationValue=5),
                                   list(InclusiveLowerBound=21,
                                        InclusiveUpperBound=100,
                                        QualificationValue=10)),
                 OutOfRangeQualificationValue=0)

ak1 <- GenerateAnswerKey(qs, scoring1)
ak2 <- GenerateAnswerKey(qs, scoring2)

# generate an AnswerKey template from a QualificationTest
qt <- system.file("templates", "qualificationtest1.xml", package = "MTurkR")
akt <- AnswerKeyTemplate(xmlParse(qt))
# use the template to generate an AnswerKey
ak <- GenerateAnswerKey(ak)
}
}
\keyword{Qualifications}