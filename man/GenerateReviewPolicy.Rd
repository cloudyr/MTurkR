\name{GenerateReviewPolicy}
\alias{GenerateHITReviewPolicy}
\alias{GenerateAssignmentReviewPolicy}
\title{Generate HIT and/or Assignment ReviewPolicies}
\description{Generate a HIT ReviewPolicy and/or Assignment ReviewPolicy data structure for use in \code{\link{CreateHIT}}.}
\usage{
GenerateHITReviewPolicy(...)

GenerateAssignmentReviewPolicy(...)
}
\arguments{
  \item{...}{ReviewPolicy parameters passed as named arguments. See details and examples.}
}
\details{
Converts a list of ReviewPolicy parameters into a ReviewPolicy data structure.

A ReviewPolicy works by testing whether an assignment or a set of assignments satisfies a particular condition. If that condition is satisfied, then specified actions are taken. ReviewPolicies come in two \dQuote{flavors}: Assignment-level ReviewPolicies take actions based on \dQuote{known} answers to questions in the HIT and HIT-level ReviewPolicies take actions based on agreement among multiple assignments. It is possible to specify both Assignment-level and HIT-level ReviewPolicies for the same HIT.

Assignment-level ReviewPolicies involve checking whether that assignment includes particular (\dQuote{correct}) answers. For example, an assignment might be tested to see whether a correct answer is given to one question by each worker as a quality control measure. The ReviewPolicy works by checking whether a specified percentage of known answers are correct. So, if a ReviewPolicy specifies two known answers for a HIT and the worker gets one of those known answers correct, the ReviewPolicy scores the assignment at 50 (i.e., 50 percent). The ReviewPolicy can then be customized to take three kinds of actions depending on that score: \code{ApproveIfKnownAnswerScoreIsAtLeast} (approve the assignment automatically), \code{RejectIfKnownAnswerScoreIsLessThan} (reject the assignment automatically), and \code{ExtendIfKnownAnswerScoreIsLessThan} (add additional assignments and/or time to the HIT automatically). The various actions can be combined to, e.g., both reject an assignment and add further assignments if a score is below the threshold, or reject below a threshold and approve above, etc.

HIT-level ReviewPolicies involve checking whether multiple assignments submitted for the same HIT \dQuote{agree} with one another. Agreement here is very strict: answers must be exactly the same across assignments for them to be a matched. As such, it is probably only appropriate to use closed-ended (e.g., multiple choice) questions for HIT-level ReviewPolicies otherwise ReviewPolicy actions might be taken on irrelevant differences (e.g., word capitalization, spacing, etc.). The ReviewPolicy works by checking whether answers to multiple assignments are the same (or at least whether a specified percentage of answers to a given question are the same). For example, if the goal is to categorize an image into one of three categories, the ReviewPolicy will check whether two of three workers agree on the categorization (known as the \dQuote{HIT Agreement Score}, which is a percentage of all workers who agree). Depending on the value of the HIT Agreement Score, actions can be taken. As of October 2014, only one action can be taken: \code{ExtendIfHITAgreementScoreIsLessThan} (extending the HIT in assignments by the number of assignments specified in \code{ExtendMaximumAssignments} or time as specified in  \code{ExtendMinimumTimeInSeconds}).

Another agreement score (the \dQuote{Worker Agreement Score}), measured the percentage of a worker's responses that agree with other workers' answers. Depending on the Worker Agreement Score, two actions can be taken: \code{ApproveIfWorkerAgreementScoreIsAtLeast} (to approve the assignment automatically) or \code{RejectIfWorkerAgreementScoreIsLessThan} (to reject the assignment automatically, with an optional reject reason supplied with \code{RejectReason}). A logical value (\code{DisregardAssignmentIfRejected}) specifies whether to exclude rejected assignments from the calculation of the HIT Agreement Score. 

Note: An optional \code{DisregardAssignmentIfKnownAnswerScoreIsLessThan} excludes assignments if those assignments score below a specified \dQuote{known} answers threshold as determined by a separate Assignment-level ReviewPolicy.
}
\value{A character string that reflects the URL-encoded \code{HITReviewPolicy} or \code{AssignmentReviewPolicy}.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_QuestionFormDataStructureArticle.html}{API Reference: QuestionForm}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_ReviewPoliciesArticle.html}{API Reference (ReviewPolicies)}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_HITReviewPolicyDataStructureArticle.html}{API Reference (Data Structure)}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateHIT}}

\code{\link{GetReviewResultsForHIT}}
}
\examples{
\dontrun{
# HIT-level ReviewPolicies #

## Conditionally extend HIT based on HIT Agreement Score
lista <- list(QuestionIds = c("Question1","Question2","Question5"),
              QuestionAgreementThreshold = 49, # at least 50 percent agreement
              ExtendIfHITAgreementScoreIsLessThan = 50,
              ExtendMinimumTimeInSeconds = 3600,
              ExtendMaximumAssignments = 2,
              DisregardAssignmentIfRejected = TRUE)
policya <- do.call(GenerateHITReviewPolicy, lista)

## Conditionally approve and reject based on Worker Agreement Score
listb <- list(QuestionIds = c("Question1","Question2","Question5"),
              QuestionAgreementThreshold = 65, # at least two of three 'correct' answers
              ApproveIfWorkerAgreementScoreIsAtLeast = 65,
              RejectIfWorkerAgreementScoreIsLessThan = 34,
              DisregardAssignmentIfRejected = TRUE)
policyb <- do.call(GenerateHITReviewPolicy, listb)

# Attach an assignment review policy to a HIT
hit1 <- 
CreateHIT(title = "Survey",
          description = "5 question survey",
          reward = ".10",
          expiration = seconds(days = 4),
          duration = seconds(hours = 1),
          keywords = "survey, questionnaire",
          hit.review.policy = policyb,
          question = GenerateExternalQuestion("http://www.example.com/","400"))

# GetReviewResults
GetReviewResultsForHIT(hit1$HITId)

# cleanup
ExpireHIT(hit1$HITId)
DisposeHIT(hit1$HITId)
}
\dontrun{
# Assignment-level ReviewPolicies #

# Example Policy A
## Conditional approval of assignments based on percent of correct answers
lista <- list(AnswerKey = list("QuestionId1" = "B",
                               "QuestionId2" = "A"),
              ApproveIfKnownAnswerScoreIsAtLeast = 99)
policya <- do.call(GenerateAssignmentReviewPolicy, lista)

# Example Policy B
## Conditional approval of assignments based on percent of correct answers
## Conditional rejection of assignments based on percent of correct answers
listb <- list(AnswerKey = list("QuestionId1" = "B",
                               "QuestionId2" = "A"),
              ApproveIfKnownAnswerScoreIsAtLeast = 99,
              RejectIfKnownAnswerScoreIsLessThan = 1)
policyb <- do.call(GenerateAssignmentReviewPolicy, listb)

# Example Policy C
## Conditionally extend HIT with more time and assignments based on score
listc <- list(AnswerKey = list("QuestionId1" = "B"),
              ExtendIfKnownAnswerScoreIsLessThan = 100,
              ExtendMaximumAssignments = 2, # maximum value is 25
              ExtendMinimumTimeInSeconds = seconds(hours = 1))
policyc <- do.call(GenerateAssignmentReviewPolicy, listc)


# Attach an assignment review policy to a HIT
## load template file
## applying Policy B will approve if form answers are "Something" and "Yes"
##                        and reject if both are incorrect
f <- system.file("templates/htmlquestion3.xml", package = "MTurkR")

hit1 <- 
CreateHIT(title = "Survey",
          description = "5 question survey",
          reward = ".10",
          expiration = seconds(days = 4),
          duration = seconds(hours = 1),
          keywords = "survey, questionnaire",
          assignment.review.policy = policyb,
          question = GenerateHTMLQuestion(file = f))

# GetReviewResults
GetReviewResultsForHIT(hit1$HITId)

# cleanup
ExpireHIT(hit1$HITId)
DisposeHIT(hit1$HITId)
}
\dontrun{
# HIT- and Assignment-level ReviewPolicies

# Example Policy D
## Conditional approval of assignments based on percent of correct answers
listd <- list(AnswerKey = list("QuestionId1" = "B",
                               "QuestionId2" = "A"),
              ApproveIfKnownAnswerScoreIsAtLeast = 99)
policyd <- do.call(GenerateAssignmentReviewPolicy, listd)

# Example Policy E
## Conditionally extend HIT based on HIT Agreement Score
liste <- list(QuestionIds = c("QuestionId3","QuestionId4","QuestionId5"),
              QuestionAgreementThreshold = 65, # at least 2/3rd agreement
              ExtendIfHITAgreementScoreIsLessThan = 66,
              ExtendMinimumTimeInSeconds = 3600,
              ExtendMaximumAssignments = 2,
              DisregardAssignmentIfRejected = TRUE)
policye <- do.call(GenerateHITReviewPolicy, liste)

## load template file
## applying 'policya' will approve if form answers are "Something" and "Yes"
f <- system.file("templates/htmlquestion3.xml", package = "MTurkR")

# Create HIT
hit2 <- 
CreateHIT(title = "Survey",
          description = "5 question survey",
          reward = ".10",
          expiration = seconds(days = 4),
          duration = seconds(hours = 1),
          keywords = "survey, questionnaire",
          assignment.review.policy = policyd,
          hit.review.policy = policye,
          question = GenerateHTMLQuestion(file = f))

# GetReviewResults
GetReviewResultsForHIT(hit2$HITId)

# cleanup
ExpireHIT(hit2$HITId)
DisposeHIT(hit2$HITId)
}
}
\keyword{HITs}
