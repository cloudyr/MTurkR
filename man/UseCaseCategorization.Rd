\name{Use Case: Categorization}
\alias{categorization}
\concept{categorization}
\concept{moderation}
\concept{content moderation}
\concept{photo moderation}
\title{Use Case: Categorization}
\description{This page describes how to use MTurkR to collect categorization data}
\details{This page describes how to use MTurkR to collect categorization data (perhaps for photo moderation or developing a training set) on Amazon Mechanical Turk. The workflow here is very similar to that for sentiment analysis (see \link{sentiment}).

The basic workflow for categorization is as follows:
\enumerate{
    \item Develop an HTML form template to display and record data
    \item Optionally, develop a Qualification Test to select high-quality workers
    \item Create a HITType (a display group) to organize the HITs you will produce
    \item Create the HITs from the template and an input data.frame
    \item Monitor and retrieve results
}
}
\section{Creating a Categorization Template}{
MTurkR comes pre-installed with a HTMLQuestion HIT template showing the basic form of a categorization project (see \code{system.file("templates/categorization1.xml", package = "MTurkR")}. This template simply displays an image from a specified URL and asks workers to categorize the image as one of five things (person, animal, fruit, vegetable, or something else). Because an HTMLQuestion HIT is just an HTML document containing a form, we have one \samp{<input>} field for each possible category with the name \samp{QuestionId1} which is the data field we are interested in analyzing when we retrieve the results.

For your own project, you may want to add additional elements such as instructions to workers about how to perform the task, additional response options, or perhaps multiple dimensions on which to categorize (resolution, clarity, etc.). The key thing to remember is that the template must contain a template field which is what will be replaced by \code{\link{BulkCreateFromTemplate}} when you create the HITs. Note in the example template that the field is actually used in three places: (1) as the \samp{src} of the HTML image field, (2) as the alt display text (in case the image doesn't load correctly), and (3) in a hidden form field. The last of these ensures that we are able to quickly and easily map the particular images into the categorization results returned by MTurk. If we don't do this, we will need to \code{\link[base]{merge}} the results data with information about what image was displayed to each worker (because of an unfortunate feature of the MTurk API), so this saves us time later on.
}

\section{Setting up the Project}{
Once the template is ready to go, we should think about who we want to complete the categorization task. By default, all HITs are available to all workers regardless of geography, language, experience, or quality. A standard practice is to limit HITs geographically and based on their past approval rating. We can specify this using \code{\link{GenerateQualificationRequirement}}:

\verb{
q1 <- GenerateQualificationRequirement(c("Locale","Approved"), 
                                       c("==",">"), 
                                       c("US",90), 
                                       preview = TRUE)
}

We can use this QualificationRequirement structure to limit our tasks to U.S.-based workers with greater than 90\% past approval. The \code{preview = TRUE} argument means that ineligible workers will not even be able to preview the task.

An alternative approach is actually to create our own Qualification with a test that assesses workers' ability to correctly categorize, possibly while training them to do. A nice feature of such tests is that they can be automatically scored by the MTurk system so there is no need for you to actively manage the pool of eligible workers. This can be combined with the location and other QualificationRequirements just mentioned.

To create a Qualification test requires using some proprietary XML markup called \href{http://docs.aws.amazon.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_QuestionFormDataStructureArticle.html}{\dQuote{QuestionForm}}. This is basically an HTML-like form that specifies different kinds of questions and answer options, which will be displayed to workers interested in obtaining the qualification. An example form is included with MTurkR as \code{system.file("templates/qualificationtest1.xml", package = "MTurkR")}. I will leave it as an exercise to readers to understand the particularities of the format.

To leverage the automatic scoring of a qualification test also requires creating an \href{http://docs.aws.amazon.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_AnswerKeyDataStructureArticle.html}{AnswerKey} that maps answers in the Qualification test to an overall score. A possible AnswerKey for the Qualification test just mentioned is installed as: \code{system.file("templates/answerkey1.xml", package = "MTurkR")}.

To use these as QualificationRequirements for a task, we need to first create an MTurk QualificationType using the test and AnswerKey:

\verb{
qf <- paste0(readLines(system.file("qualificationtest1.xml", package = "MTurkR")), collapse="")
qa <- paste0(readLines(system.file("answerkey1.xml", package = "MTurkR")), collapse="")
qual1 <- CreateQualificationType(name = "Qualification with Test",
           description = "This qualification is a demo",
           test = qf,
           answerkey = qa,
           status = "Active",
           keywords = "test, autogranted")
}

The QualificationType is now registered in the MTurk system and we can use it to create a QualificationRequirement to attach to our categorization HITs (e.g., to only include workers scoring above 50\% on the test:

\verb{
q2 <- GenerateQualificationRequirement(c("Locale","Approved",qual1$QualificationTypeId), 
                                       c("==", ">", ">"), 
                                       c("US", 90, 50), 
                                       preview = TRUE)
}
}

\section{Creating the HITs}{
To actually create the HITs, we need to define some \dQuote{HITType} parameters that regulate what workers can complete the task, how much they will be paid, and what the task will look like when workers search for it on the MTurk worker interface. 

\verb{
newhittype <- 
RegisterHITType(title = "Categorize an Image",
                description = "Categorize an image according to its content",
                reward = ".05", 
                duration = seconds(hours=1), 
                auto.approval.delay = seconds(days = 1),
                qual.req = q2,
                keywords = "categorization, image, coding, rating, sorting")
}

This creates a HITType, which is essentially a group of HITs; all HITs with the same HITType will be displayed together, allowing a worker to quickly complete many of the tasks in sequence. The value of \code{auto.approval.delay} is important; because we might be creating a very large number of HITs, this specifies that workers' answers will be automatically approved after a specific amount of time, preventing us from having to manually approve assignments.

With the HITType created, we can now create the HITs. The HIT template we are using specifies one template field, \samp{imageurl}, which is shown to the workers and also used to set the value of an HTML form field. This means that to use the template, we need to have a data.frame that contains one column, one row per image URL. MTurk does not return information about the HIT itself (e.g., what text was shown) with the results data, so taking this step of creating a hidden HTML form field recording that records the identifier thereby provides a simple way of mapping the displayed text to particular data we will retrieve from MTurk later on.

We can then use \code{BulkCreateFromTemplate} to process the data.frame and produce the HITs. (In this example, the task contains very limited instructions, so in an applied case you may want to provide further details to workers about how to successfully complete the task.)

\verb{
# template file
f <- system.file("templates/categorization1.xml", package = "MTurkR")

# input data.frame
dat <- 
data.frame(imageurl = c("http://example.com/image1.png", "http://example.com/image1.png", "http://example.com/image1.png")
           stringsAsFactors = FALSE)

# create the HITs (this is time consuming)
bulk <- 
BulkCreateFromHITLayout(template = f,
                        input = dat,
                        hit.type = newhittype$HITTypeId,
                        assignments = 3,
                        annotation = paste("Image Categorization", Sys.Date()),
                        expiration = seconds(days = 7))
}

(Note: This does not actually upload images anywhere, so the value sof \samp{imageurl} should be live URLs. If you have files stored locally, you will need to upload them to a high-capacity server first (perhaps Amazon S3).)

The code above processes the template using an input data.frame and creates a HIT for each row in the data.frame. In this case, we have asked for 3 assignments, meaning we would like three workers to categorize each image. The \code{bulk} object will be a data.frame containing one row per HIT. To perform operations on the individual HITs (e.g., expiring them early, retrieving results data, etc.) we will need the \code{HITId} value for each HIT as stored in this data.frame.
}
\section{Monitoring Project}{
Once the HITs are live, they will likely be completed very quickly. If you have a very large project, however, it may take some time. To monitor the status of a project, you can use:

\verb{
HITStatus(annotation = paste("Image Categorization", Sys.Date())
}

or

\verb{
HITStatus(hit.type = newhittype$HITTypeId)
}

With a large number of HITs, the output of this will be fairly verbose, so you may instead simply want to examine the response objects themselves.

You may find a need to cancel HITs or change them in some way and there are numerous \dQuote{maintenance} functions available to do this: \code{\link{ExpireHIT}}, \code{\link{ExtendHIT}} (to add assignments or time), \code{\link{ChangeHITType}} (to change display properties or payment of live HITs), and so forth. All of these functions can be called on a specific HIT, a HITType group, or on a set of HITs identified by their \code{annotation} value (which is a hidden field that can be useful for keeping track of HITs).
}
\section{Retrieving Results}{
The final step of a categorization project is simply to retrieve the results and analyze them. Because we used a HITType and \code{\link{BulkCreateFromTemplate}}, retrieving the data for every assignment for every HIT in the project is very simple, but may be somewhat time consuming depending on the number of assignments involved. All we have to do is:

\verb{
a <- GetAssignments(hit.type = newhittype$HITTypeId, return.all = TRUE)
}

If you have multiple projects under the same HITType (e.g., posted on different days), you can also retrieve assignments using the \code{annotation} field:

\verb{
a <- GetAssignments(annotation = paste("Image Categorization", Sys.Date()), return.all = TRUE)
}

The response object is simply a data.frame. This data.frame will contain metadata about the task (HITTypeId, HITId, AssignmentId, WorkerId, submission and approval times) and values corresponding to every field in the HIT form. In this case, we will have two \dQuote{answer} fields: \samp{QuestionId1}, which contains the score given by the workers, and \samp{imageurl}, providing the unique identifier for every image. Response are always stored as text because of the nature of the MTurk API, so you may need to coerce the data to a different type before performing some kind of analysis.
}

%\details{}
%\examples{\dontrun{}}
\author{Thomas J. Leeper}
\seealso{
For guidance on some of the functions used in this tutorial, see:
\itemize{
    \item \code{\link{CreateQualificationType}}
    \item \code{\link{GenerateQualificationRequirement}}
    \item \code{\link{RegisterHITType}}
    \item \code{\link{BulkCreate}}
    \item \code{\link{HITStatus}}
    \item \code{\link{GetAssignments}}
}

For some other tutorials on how to use MTurkR for specific use cases, see the following:
\itemize{
    \item \link{survey}, for collecting survey(-experimental) data
    %\item \link{categorization}, for doing large-scale categorization (e.g., photo moderation or developing a training set)
    \item \link{sentiment}, for doing sentiment coding
    \item \link{webscraping}, for manual scraping of web data
}
}
\keyword{Use Cases}
