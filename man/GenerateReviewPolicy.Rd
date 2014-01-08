\name{GenerateReviewPolicy}
\alias{GenerateReviewPolicy}
\title{Generate HIT and/or Assignment ReviewPolicies}
\description{Generate a HIT ReviewPolicy and/or Assignment ReviewPolicy data structure for use in \code{\link{CreateHIT}}.}
\usage{
GenerateReviewPolicy(hitpolicy = NULL, assignpolicy = NULL)
}
\arguments{
  \item{hitpolicy}{An optional list.}
  \item{assignpolicy}{An optional list.}
}
\details{Convert an R list into a ReviewPolicy data structure. See examples.}
\value{Returns a list containing one or two named lists, \code{HITReviewPolicy} and/or \code{AssignmentReviewPolicy} as parsed XML, string, and url-encoded string.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_QuestionFormDataStructureArticle.html}{API Reference: QuestionForm}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_ReviewPoliciesArticle.html}{API Reference (ReviewPolicies)}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_HITReviewPolicyDataStructureArticle.html}{API Reference (Data Structure)}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
\dontrun{
lista <- list(    QuestionIds=c("Question1","Question2","Question5"),
                QuestionIdAgreementThreshold=8,
                ExtendMinimumTimeInSeconds=3600)
hpolicy <- GenerateReviewPolicy(hitpolicy=lista)
}
\dontrun{
listb <- list(    AnswerKey=list(    list(Key="QuestionId3",Value="B"),
                                list(Key="QuestionId7",Value="A"),
                                list(Key="QuestionId15",Value="C")),
                ExtendIfKnownAnswerScoreIsLessThan=80,
                ExtendMaximumAssignments=3)
apolicy <- GenerateReviewPolicy(assignpolicy=listb)
}
}
\keyword{HITs}