\name{Use Case: Scraping}
\alias{webscraping}
\concept{scraping}
\concept{webscraping}
\concept{web scraping}
\title{Use Case: Scraping}
\description{Use MTurkR to manually scrape web pages or documents}
\details{This page describes how to use MTurkR to scrape human-readable data from the web or other sources using Amazon Mechanical Turk. While webscraping packages (such as XML, xml2, rvest, RSelenium) make it easy to retrieve structured web data and OCR technologies make it possible to extract text from PDFs and images, there are some cases in which data is stored in forms that are primarily human-readable rather than machine-readable. This particularly the case when information is textual but unstructured such as information stored in ad-hoc formats across different web pages (e.g., contact information for businesses) or in formats that are difficult or annoying to automatically scrape (e.g., PDF). This tutorial describes how to use MTurkR to gather data from these kinds of sources.}
\section{Designing a HIT}{
The first step for a scraping project is to identify sources of information and decide what data you need to extract from those sources. Given the potentially massive variation in specific use cases, this tutorial will not address any particular solution in-depth. Once you have identified what information you would like to extract from the documents, you have to create a form representation in which the MTurk workers will submit information for a given document. This can be a QuestionForm (see \code{\link{CreateHIT}}), which is a proprietary markup, or more easily just an HTML form (see \code{\link{GenerateHTMLQuestion}}). An example of QuestionForm markup is given in \code{system.file("templates/tictactoe.xml", package = "MTurkR")} and an example of an HTMLQuestion markup is given in \code{system.file("templates/htmlquestion3.xml", package = "MTurkR")}. Either is perfectly acceptable. For those with HTML experience, HTMLQuestion is the easier approach; for those interested in making a very simple HIT and who have no previous HTML experience, QuestionForm may be easier to work with.

When designing the HIT, it should either contain a template placeholder (see an example in \code{system.file("templates/htmlquestion2.xml", package = "MTurkR")}) or you should create a separate HIT source file for every document or webpage that you wanted to scrape. Using a template is simple if, for example, you want to scrape information from a list of websites because you can create a basic template and then use \code{\link{BulkCreateFromTemplate}} to add the website URL (and perhaps other details) to the HIT. If you are scraping a smaller number of documents that are quite irregular in format, it may be easier to manually create each HIT, save it as an .html file, and then create using either \code{\link{BulkCreate}} (if you store the .html files locally) or \code{\link{BulkCreateFromURLs}} if you upload those files to a remote server (e.g., Amazon S3).

The remainder of this tutorial will assume you have created the files manually and either wish to create HITs from the local files or from publicly accessible URLs for the server to which those HIT files have been uploaded. If neither of these cases applies (e.g., you want to use a HIT template), consider reading the \dQuote{Use Case: Categorization} tutorial (see \code{categorization}).
}
\section{Creating the HITs}{
If you have the HIT files saved locally, you will use \code{\link{BulkCreate}} to upload the HIT file to the MTurk server and use that file to create a single HIT. Begin by storing all of these files in a single folder. You can use \code{\link[base]{list.files}} to retrieve all of the files and format them correctly for use in \code{BulkCreate}. From there, you will create a vector of character strings containing the files and pass these to \code{BulkCreate}:

\verb{
qvec <- sapply(list.files(pattern = ".html"), function(x) {
          paste0(readLines(x, warn = FALSE), collapse = "\n")
        })
# create a HIT from each question file
hits <- BulkCreate(questions = qvec,
                   annotation = "My First Scraping Project",
                   title = "Retrieve information from a website",
                   description = "Find business information for the business named in the HIT.",
                   assignments = 1,
                   reward = ".25",
                   expiration = seconds(days = 4),
                   duration = seconds(minutes = 5),
                   keywords = "scraping, information search, finding, business, details, contact")
}

(Notes: (1) In this example, you are asking for one assignment per HIT. If each HIT is a business you want information for, this will allow one worker to retrieve information for each business. (2) This does not specify any constraints on which workers can complete HITs. If you want to restrict this to particular locations or by other measures, you will need to add a QualificationRequirement to the project (see \code{\link{GenerateQualificationRequirement}}).)

The above code will return a list, where each element is a one-row data.frame (or a representation of an API error message). Assuming all HITs were successfully created, you can then convert this to a single data.frame using \code{do.call("rbind", hits)}. You will want to preserve this data.frame as it contains the HITId value for each HIT, which you can use to manage the HITs.
}
\section{Retrieving Results}{
Once all data have been scraped, you can retrieve the results using \code{\link{GetAssignments}}. (To know if the project is completed, you can monitor it using \code{\link{HITStatus}}.) \code{GetAssignments} can retrieve data for a single assignment, a single HIT, all HITs in a \dQuote{HITType} group, or all HITs with a shared \code{annotation} value.

You may also find a need to cancel HITs or change them in some way and there are numerous \dQuote{maintenance} functions available to do this: \code{\link{ExpireHIT}}, \code{\link{ExtendHIT}} (to add assignments or time), \code{\link{ChangeHITType}} (to change display properties or payment of live HITs), and so forth. All of these functions can be called on a specific HIT, a HITType group, or on a set of HITs identified by their \code{annotation} value (which is a hidden field that can be useful for keeping track of HITs).

In the above code, we set \code{annotation = "My First Scraping Project"} so we can use this to both monitor the project and retrieve assignment data:

\verb{
# check status of project
HITStatus(annotation = "My First Scraping Project")

# get all assignment data
a <- GetAssignments(annotation = "My First Scraping Project", return.all = TRUE)
}

Here, \code{a} will be a data.frame containing information about each assignment. This data.frame will contain metadata about the task (HITTypeId, HITId, AssignmentId, WorkerId, submission and approval times) and values corresponding to every field in the HIT form. Response are always stored as text because of the nature of the MTurk API, so you may need to coerce the data to a different type before performing some kind of analysis.
}
\author{Thomas J. Leeper}
\seealso{
For guidance on some of the functions used in this tutorial, see:
\itemize{
    \item \code{\link{BulkCreate}}
    \item \code{\link{HITStatus}}
    \item \code{\link{GetAssignments}}
}

For some other tutorials on how to use MTurkR for specific use cases, see the following:
\itemize{
    \item \link{survey}, for collecting survey(-experimental) data
    \item \link{categorization}, for doing large-scale categorization (e.g., photo moderation or developing a training set)
    \item \link{sentiment}, for doing sentiment coding
    %\item \link{webscraping}, for manual scraping of web data
}
}
\keyword{Use Cases}
