\name{Use Case: Sentiment Analysis}
\alias{sentiment}
\concept{sentiment}
\concept{sentiment analysis}
\title{Use Case: Sentiment Analysis}
\description{This page describes how to use MTurkR to collect sentiment data on Amazon Mechanical Turk.}
\details{A common use case for MTurkR would be to code or score text or image data according to one or more dimensions. For example, you may have a large corpus of text that is divided into sentences and you want to code the sentiment of each sentence. Another would be that you want to score sentiment for messages posted on Twitter (\dQuote{tweets}). This tutorial walks through how to create a simple sentiment analysis project and retrieve the results.

This tutorial is very similar to the one for image or text categorization (see \link{categorization}), though that tutorial also covers the use of Qualification Tests to selecting or training workers for a given task.
}
\section{Creating a Sentiment Analysis HIT}{
MTurkR comes pre-installed with a HTMLQuestion HIT template for creating a sentiment project (see \code{system.file("templates/sentiment1.xml", package = "MTurkR")}. This template simply displays a message value and asks the MTurk worker to provide a sentiment score from -10 to +10 on a range indicator (or \dQuote{slider}). Because an HTMLQuestion HIT is just an HTML document containing a form, we have one \samp{<input>} field with the name \samp{sentiment} which is the data field we are interested in analyzing when we retrieve the results.

The template works well with the \code{\link{BulkCreateFromTemplate}} function, which allows you to quickly produce a very large number of related HITs from a template and an input data.frame.

To begin, we need to define some HITType parameters that regulate what workers can complete the task, how much they will be paid, and what the task will look like when workers search for it on the MTurk worker interface. 

\verb{
newhittype <- 
RegisterHITType(title = "Sentiment Analysis Task",
                description = "Score a tweet for positive or negative sentiment.",
                reward = ".05", 
                duration = seconds(hours=1), 
                auto.approval.delay = seconds(days = 1),
                keywords = "sentiment, coding, twitter, tweet, text, rating")
}

This creates a HITType, which is essentially a group of HITs; all HITs with the same HITType will be displayed together, allowing a worker to quickly complete many of the tasks in sequence. The value of \code{auto.approval.delay} is important; because we might be creating a very large number of HITs, this specifies that workers' answers will be automatically approved after a specific amount of time, preventing us from having to manually approve assignments.

Also, in this example, the task can be complete by any worker anywhere in the world; to change this, specify an object created by \code{\link{GenerateQualificationRequirement}} as the \code{qual.req} argument.

With the HITType created, we can now create the HITs. The HIT template we are using specifies one visible template field, \samp{tweettext}, which is shown to the workers and one hidden template field, \samp{tweetid}, which is hidden and that is also used to set the value of an HTML form field. This means that to use the template, we need to have a data.frame that contains two columns, one with each of those names. MTurk does not return information about the HIT itself (e.g., what text was shown) with the results data, so taking this step of creating a hidden HTML form field recording that records the identifier thereby provides a simple way of mapping the displayed text to particular sentiment data we will retrieve from MTurk later on.

We can then use \code{BulkCreateFromTemplate} to process the data.frame and produce the HITs. (In this example, the task contains very limited instructions, so in an applied case you may want to provide further details to workers about how to successfully complete the task.)

\verb{
# template file
f <- system.file("templates/sentiment1.xml", package = "MTurkR")

# input data.frame
dat <- 
data.frame(tweetid = 1:3,
           tweettext = c("abc", "def", "fgh"), 
           stringsAsFactors = FALSE)

# create the HITs (this is time consuming)
bulk <- 
BulkCreateFromHITLayout(template = f,
                        input = dat,
                        hit.type = newhittype$HITTypeId,
                        assignments = 5,
                        annotation = paste("Twitter Sentiment Project", Sys.Date()),
                        expiration = seconds(days = 7))
}

Here we process the template using an input data.frame and create a HIT for each row in the data.frame. In this case, we have asked for 5 assignments, meaning we would like five workers to score each text. The \code{bulk} object will be a data.frame containing one row per HIT. To perform operations on the individual HITs (e.g., expiring them early, retrieving results data, etc.) we will need the \code{HITId} value for each HIT as stored in this data.frame.
}
\section{Monitoring Project}{
Once the HITs are live, they will likely be completed very quickly. If you have a very large project, however, it may take some time. To monitor the status of a project, you can use:

\verb{
HITStatus(annotation = paste("Twitter Sentiment Project", Sys.Date())
}

or

\verb{
HITStatus(hit.type = newhittype$HITTypeId)
}

With a large number of HITs, the output of this will be fairly verbose, so you may instead simply want to examine the response objects themselves.

You may find a need to cancel HITs or change them in some way and there are numerous \dQuote{maintenance} functions available to do this: \code{\link{ExpireHIT}}, \code{\link{ExtendHIT}} (to add assignments or time), \code{\link{ChangeHITType}} (to change display properties or payment of live HITs), and so forth. All of these functions can be called on a specific HIT, a HITType group, or on a set of HITs identified by their \code{annotation} value (which is a hidden field that can be useful for keeping track of HITs).
}
\section{Retrieving Results}{
The final step of a sentiment analysis project is simply to retrieve the results and analyze them. Because we used a HITType and \code{\link{BulkCreateFromTemplate}}, retrieving the data for every assignment for every HIT in the project is very simple, but may be somewhat time consuming depending on the number of assignments involved. All we have to do is:

\verb{
a <- GetAssignments(hit.type = newhittype$HITTypeId, return.all = TRUE)
}

If you have multiple projects under the same HITType (e.g., posted on different days), you can also retrieve assignments using the \code{annotation} field:

\verb{
a <- GetAssignments(annotation = paste("Twitter Sentiment Project", Sys.Date()), return.all = TRUE)
}

The response object is simply a data.frame. This data.frame will contain metadata about the task (HITTypeId, HITId, AssignmentId, WorkerId, submission and approval times) and values corresponding to every field in the HIT form. In this case, we will have two \dQuote{answer} fields: \samp{sentiment}, which contains the score given by the workers, and \samp{tweetid}, providing the unique identifier for every text. Response are always stored as text because of the nature of the MTurk API, so you may need to coerce the data to a different type before performing some kind of analysis.
}
\author{Thomas J. Leeper}
\seealso{
For guidance on some of the functions used in this tutorial, see:
\itemize{
    \item \code{\link{BulkCreate}}
    \item \code{\link{RegisterHITType}}
    \item \code{\link{HITStatus}}
    \item \code{\link{GetAssignments}}
}

For some other tutorials on how to use MTurkR for specific use cases, see the following:
\itemize{
    \item \link{survey}, for collecting survey(-experimental) data
    \item \link{categorization}, for doing large-scale categorization (e.g., photo moderation or developing a training set)
    %\item \link{sentiment}, for doing sentiment coding
    \item \link{webscraping}, for manual scraping of web data
}
}
\keyword{Use Cases}
