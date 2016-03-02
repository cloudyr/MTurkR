\name{Miscellaneous}
\alias{ListOperations}
\alias{listops}
\alias{ListQualificationTypes}
\alias{ListStatistics}
\alias{ViewAvailableHITs}
\alias{OpenWorkerPage}
\alias{OpenManageHITPage}
\alias{OpenQualificationPage}
\alias{OpenDownloadPage}
\title{Convenience Functions and RUI Interaction}
\description{These functions provide lists (of QualificationTypes, Requester and Worker Statistics) useful when creating HITs and QualificationTypes, and other functions to open specific pages of the MTurk Requester User Interface (RUI), or in the case of \code{ViewAvailableHITs}, the MTurk worker site.}
\usage{
ListOperations(op = NULL)

ListQualificationTypes(qual = NULL)

ListStatistics(stat = NULL, value.type = NULL, type = NULL)

ViewAvailableHITs(query = NULL, requester = NULL, min.reward = NULL, qualified = NULL)

OpenWorkerPage(workerid = NULL)
OpenManageHITPage(hit = NULL)
OpenDownloadPage(hit, download = FALSE)
OpenQualificationPage()
}
\arguments{
  \item{op}{For \code{ListOperations}: a number indicating which of the operations to return. Probably not useful.}
  \item{qual}{For \code{ListQualificationTypes}: a character string containing the name of a built-in QualificationType. If specified, \code{ListQualificationTypes} returns only the QualificationTypeId of that QualificationType.}
  \item{stat}{For \code{ListStatistics}: an optional character string specifying the name of an MTurk statistic. If specified, only details of that statistic are returned.}
  \item{value.type}{For \code{ListStatistics}: an optional character string specifying whether \dQuote{Double}-type or \dQuote{Long}-type statistics should be returned. If \code{NULL}, both types are returned. Probably not useful.}
  \item{type}{For \code{ListStatistics}: an optional character string specifying whether \dQuote{GetRequesterStatistic}-type or \dQuote{GetRequesterWorkerStatistic}-type statistics should be returned. If \code{NULL}, both types are returned.}
  \item{query}{For \code{ViewAvailableHITs}: an optional character string containing a search query used to search through HITs available on the MTurk worker site.}
  \item{requester}{For \code{ViewAvailableHITs}: an optional character string containing a RequesterId whose HITs are to be searched, for example to see how one's own HITs appear to workers on the MTurk worker site.}
  \item{min.reward}{For \code{ViewAvailableHITs}: an optional character string containing a minimum reward (in U.S. Dollars) criterion to be used when searching available HITs on the worker site.}
  \item{qualified}{For \code{ViewAvailableHITs}: an optional logical specifying whether only HITs for which you are qualified should be searched.}
  \item{workerid}{For \code{OpenWorkerPage}: an optional character string containing the WorkerId of a worker whose Requester User Interface (RUI) management page is to be opened. If \code{NULL}, function opens the list of workers.}
  \item{hit}{For \code{OpenManageHITPage}: an optional character string containing the HITId of a HIT whose Requester User Interface (RUI) management page is to be opened. If \code{NULL}, function opens the list of all HITs. For \code{OpenDownloadPage}: a mandatory character string containing a HITID of a HIT whose assignment data download page should be opened.}
  \item{download}{For \code{OpenDownloadPage}: a logical indicating whether the HIT results should be downloaded directly or whether the Requester User Interface (RUI) ManageHIT page should be opened (the default).}
}
\details{A set of convenience functions to either display various information about MTurk semantics or open specified parts of the Requester User Interface (RUI).}
\value{Either a data frame containing the requested information (in the case of \code{ListOperations}, \code{ListQualificationTypes}, \code{ListStatistics}) or nothing internal to R, but the specified webpage is opened in the user's default web browser (in the case of \code{OpenWorkerPage}, \code{OpenManageHITPage}, \code{OpenDownloadPage}, or \code{OpenQualificationPage}).}
%\references{}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
\dontrun{
ListOperations()
ListQualificationTypes()
ListStatistics()
ViewAvailableHITs(min.reward=".50")

OpenWorkerPage()
OpenWorkerPage("A1RO9UEXAMPLE")
OpenManageHITPage()
OpenDownloadPage("267SI2KYEPLS8QWZYPXWP3EXAMPLE",download=TRUE)
OpenQualificationPage()
}
}
\keyword{Documentation}