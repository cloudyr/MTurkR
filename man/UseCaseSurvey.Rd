\name{Use Case: Surveys}
\alias{survey}
\concept{survey}
\concept{survey survey}
\concept{survey panel}
\concept{questionnaire}
\title{Use Case: Surveys}
\description{This page describes how to use MTurkR to collect survey(-experimental) data on Amazon Mechanical Turk.}
\details{
MTurk is widely used as a platform for social research, due to its low costs, ease of use, and quick access to participants. This use case tutorial walks through how to create MTurk HITs using MTurkR. It also describes some advanced features, such as managing panels of MTurk participants, and randomizing content.
}
\section{Creating a survey HIT}{
Because a HIT is simply an HTML form, it is possible to design an entire survey(-experimental) questionnaire directly in MTurk. This could be done using an HTMLQuestion HIT. That would require, however, an extensive knowledge of HTML, likely a fair amount of JavaScript to display questions and validate worker responses, and a large amount of CSS styling. An alternative approach is to use an off-site survey tool (such as SurveyMonkey, Qualtrics, Google Spreadsheet Forms, etc.) to design a questionnaire and then simply use a HIT to direct workers to that task. There are two ways to do this in MTurkR. One is to create a \dQuote{link and code} HIT that includes a link to an off-site tool and a text entry field for a worker to enter a completion code. The other is to embed the questionnaire directly as an ExternalQuestion HIT. The following two sections describe these approaches.

\subsection{Link and code}{
To use the link and code, the HIT must contain the following:

\itemize{
    \item A link to the off-site survey
    \item A text entry form field into which the worker will enter a completion code
    \item Instructions to the worker about how to complete the HIT
    \item Optionally, JavaScript to dynamically display the link and/or entry field
}

MTurkR includes an XML file that could be used to create an HTMLQuestion \dQuote{link and code} HIT. You can find it in \code{system.file("templates/htmlquestion1.xml", package = "MTurkR")}. A more robust example of an HTMLQuestion showing a survey link that is dynamically displayed once the HIT is accepted is included as \code{system.file("templates/surveylink.xml", package = "MTurkR")}. You can also create a HIT of this kind on the MTurk requester user interface (RUI) website.



}
\subsection{ExternalQuestion}{
The link and code method is easier to setup, but invites problems with workers entering fake codes, workers completing the survey but not entering the code (and thus not getting paid), and other human error possibilities. By contrast, the ExternalQuestion method provides a seamless link between the MTurk interface and an off-site tool. Instead of showing the worker an HTML page with a link, configuring an ExternalQuestion actually shows the worker the survey directly inside an iframe in the MTurk worker interface. This eliminates the need for the requester to create codes and for workers to copy them.

The trade-off is that this method requires a somewhat sophisticated survey tool (e.g., Qualtrics) that can handle redirecting the worker back to the ExternalQuestion submit URL for their assignment. The submit URL has to take the form of: \samp{https://www.mturk.com/mturk/externalSubmit?assignmentId=workerspecificassignmentid&foo=bar}. The \samp{foo=bar} part is required by MTurk (it requires the assignmentId and at least one other field). Note: The previous link is for the live server. For testing in the requester sandbox, the base URL is: \samp{https://workersandbox.mturk.com/mturk/externalSubmit}.

Creating the HIT itself is quite simple. It simply requires registering a HITType, building the ExternalQuestion structure (in which you can specify the height of the iframe in pixels), and using it in \code{\link{CreateHIT}}:

\verb{
newhittype <- 
RegisterHITType(title = "10 Question Survey",
                description = "Complete a 10-question survey about news coverage and your opinions",
                reward = ".20", 
                duration = seconds(hours=1), 
                keywords = "survey, questionnaire, politics")
eq <- GenerateExternalQuestion(url="https://www.example.com/myhit.html",frame.height="400")
CreateHIT(hit.type = newhittype$HITTypeId, 
          question = eq$string, 
          expiration = seconds(1), 
          assignments = 1)
}

To make this work with a Qualtrics survey URL, do the following:

\enumerate{
    \item Create the Qualtrics survey
    \item Setup the Qualtrics survey to extract embedded URL parameters (see \href{http://qualtrics.com/university/researchsuite/advanced-building/survey-flow/embedded-data/}{documentation}), which MTurk will attach to the Qualtrics survey link. You need to extract the \samp{assignmentId} field. It is also helpful to extract the \samp{workerId} field to embed it in the survey dataset as a unique identifier for each respondent.
    \item Setup the Qualtrics survey to redirect (at completion) to the ExternalQuestion submit URL, using the \samp{assignmentId} (the embedded data field) and at least one other survey field. See \href{http://qualtrics.com/university/researchsuite/basic-building/basic-survey-options/survey-termination/}{Qualtrics documentation} for details. For the live server, this URL should look like: \samp{https://www.mturk.com/mturk/externalSubmit?assignmentId=$\{e://Field/assignmentId\}&foo=bar} and for the sandbox it should look like: \samp{https://workersandbox.mturk.com/mturk/externalSubmit?assignmentId=$\{e://Field/assignmentId\}&foo=bar}.
    \item Create an ExternalQuestion HIT via MTurkR using the survey link from Qualtrics, as in the example above.
}

}

}
\section{Managing a worker panel}{

The viability of MTurk as a platform for implementing social science experiments is well-understood, but the platform's comparative advantage for implementing complicated panel data collection is underappreciated, in part for technological reasons. The ability to recontact and pay large numbers of workers falls outside the functionality of the RUI. In this use case, the objective is to implement a three-wave panel experiment, where respondents are randomly assigned to a condition at time 1, recontacted to participate in a follow-up survey at time 2, and finally a second follow-up wave at time 3. The first stages of the workflow here are relatively easy. First, an experimental survey instrument is constructed with any tool (e.g., Qualtrics or one's own HTML). Second, a single HIT is created (without a qualification test) to which workers respond by completing the survey-experiment. Note that when only one HIT is needed, the parameters normally assigned with \code{\link{RegisterHITType}} can be specified in \code{\link{CreateHIT}}.

\verb{
newhit <- CreateHIT(question = GenerateExternalQuestion("http://www.test.com/surveylink", 400)$string,
                    title = "5-Question Survey",
                    description = "Take a five-question survey about your political opinions.",
                    reward = ".25",
                    duration = seconds(hours=4),
                    expiration = seconds(days=7),
                    assignments = 1000,
                    keywords = "survey, question, answers, research, politics, opinion",
                    auto.approval.delay = seconds(days=2),
                    qual.req = GenerateQualificationRequirement("Location","==","US",preview=TRUE))
}

Once a sufficient number of responses are collected (i.e., assignments completed), assignments can be reviewed such that only those who pass an attention check are approved and the remainder rejected:

\verb{
review <- GetAssignments(hit=newhit$HITId)
correctcheck <- "7"
approve <- approve(assignments = review$AssignmentId[review$check==correct])
reject <- reject(assignments = review$AssignmentId[!review$check==correct])
}

After reviewing these assignments, MTurkR can be used to Bonus, Contact, and Qualify workers. To implement a panel, workers who completed the original HIT (the time 1 survey) are either granted a QualificationRequirement that gives them access to the second and third waves or they are simply contacted directly with a link to an off-site tool. If using the latter approach, the workers can be paid a bonus if they complete the follow-up surveys.

Using the first method, if we want to post the time 2 or time 3 surveys as new HITs but limit participation to those who have completed before, we would create a QualificationType using \code{\link{CreateQualificationType}}:

\verb{
thenewqual <-
CreateQualificationType(
    name = "Completed Wave 1",
    description = "This qualification is for worker who completed wave 1.",
    status = 'Active',
    keywords = "Worked for me before",
    auto = FALSE)
}

Next, we can use \code{\link{AssignQualification}} to give all workers from your previous HIT a score above the threshold you'll use on the new HIT. So we'll set a score of 50 here, but you could use anything.

\verb{
AssignQualification(
    qual = thenewqual$QualificationTypeId,
    workers = approve$WorkerId,
    value = 1)
}

This will return a data.frame specifying the scores assigned to each worker, so you don't need to store the result.

Finally, when we create the new HIT (for time 2 or time 3 surveys), we simply need to specify the QualificationType you created as a Qualification Requirement (using \code{\link{GenerateQualificationRequirement}}:

\verb{
# generate a QualificationRequirement object
req <- GenerateQualificationRequirement(thenewqual$QualificationTypeId, "==", "1")

# create HIT
newhit <- 
CreateHIT(
    title = "Survey",
    description = "5 question survey",
    reward = ".10",
    duration = seconds(days=1),
    keywords = "survey, questionnaire",
    expiration = seconds(days=7),
    assignments = 50,
    hitlayoutid = "23ZGOOGQSCM61T1H5H9U0U00OQWFFU",
    qual.req = req)
}

This restricts the survey to only be available to workers with the Qualification (i.e., those who completed the time 1 survey). Completions can then be monitored and approved in the same way as at time 1. The process could be repeated for the time 3 survey.

If we wanted to instead simply host the time 2 and time 3 surveys on an external site, we could invite workers from time 1 using \code{\link{ContactWorkers}} and then pay those who complete the survey using \code{\link{GrantBonus}}. We simply need to record which workers complete the survey. In the below example, we'll pass their WorkerId in the survey link, to record this automatically:

\verb{
b1 <- "If you complete a follow-up survey, you will receive a $.50 bonus.\n
       You can complete the survey at the following link: http://www.test.com/link1?WorkerId="
t2 <- ContactWorkers(subjects = "Complete follow-up survey for $.50 bonus",
                     msgs = sapply(approve$WorkerId, FUN=function(worker) paste(b1, worker)), 
                     workers = approve$WorkerId)
}

Once we have tracked completions, we can then pay bonuses by supplying a vector of workers and their corresponding \samp{AssignmentId} (from the time 1 survey). This makes using \code{\link{GrantBonus}} a little clunky to use, but it works:

\verb{
bonus <- GrantBonus(workers = review$WorkerId[review$WorkerId %in% approve$WorkerId],
                    assignments = review$AssignmentId[review$WorkerId %in% approve$WorkerId],
                    amounts = "1.00", 
                    reasons = "Thanks for completing the follow-up survey!")
}

We can then repeat the process for the time 3 survey, using \code{ContactWorkers} and \code{GrantBonus} to invite and pay workers completing follow-up waves. The use of these tools, along with QualificationTypes, enables the maintenance of complex panels of workers with specific demographics or skills.
}
%\section{Randomization}{}
\seealso{
For guidance on some of the functions used in this tutorial, see:
\itemize{
    \item \code{\link{CreateHIT}}
    \item \code{\link{GetAssignments}}
    \item \code{\link{ContactWorkers}}
    \item \code{\link{GrantBonus}}
    \item \code{\link{CreateQualificationType}}
}

For some other tutorials on how to use MTurkR for specific use cases, see the following:
\itemize{
    %\item \link{survey}, for collecting survey(-experimental) data
    \item \link{categorization}, for doing large-scale categorization (e.g., photo moderation or developing a training set)
    \item \link{sentiment}, for doing sentiment coding
    \item \link{webscraping}, for manual scraping of web data
}
}
\keyword{Use Cases}
