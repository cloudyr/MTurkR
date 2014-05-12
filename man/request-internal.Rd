\name{request}
\alias{request}
\alias{print.MTurkResponse}
\title{Execute an MTurk API Request}
\description{This is the workhorse function that makes authenticated HTTP requests to the MTurk API. It is only not exported as of v0.5.}
\usage{
request(keyid = getOption('MTurkR.keypair')[1], operation, GETparameters, 
        secret = getOption('MTurkR.keypair')[2],
        version = "2012-03-25", service = "AWSMechanicalTurkRequester", 
        browser = getOption('MTurkR.browser'),
        log.requests = getOption('MTurkR.log'),
        sandbox = getOption('MTurkR.sandbox'), 
        xml.parse = FALSE, print.errors = TRUE,
        validation.test = getOption('MTurkR.test'))
}
\arguments{
  \item{keyid}{An AWS Access Key ID. Default is from \code{options('MTurkR.keypair')}.}
  \item{operation}{The MTurk API operation to be performed.}
  \item{GETparameters}{An optional character string containing URL query parameters that specify options for the request.}
  \item{secret}{An AWS Secret Access Key. Default is from \code{options('MTurkR.keypair')}.}
  \item{version}{The version of the MTurk API to use. Default is \dQuote{2012-03-25}, under which the MTurkR package was built.}
  \item{service}{The MTurk service to which the authenticated request will be sent. Supplied only for advanced users.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
  \item{xml.parse}{Whether the returned list should contain parsed XML.}
  \item{print.errors}{Whether errors produced by the MTurk API request should be printed.}
  \item{validation.test}{Currently a logical that causes \code{request} to return the URL of the specified REST request. Default is \code{FALSE}. May additionally validate the request (and supply information about that validation) in the future.}
}
\details{This is an internal function that executes MTurk API requests. It is made available for use by advanced users to execute custom API requests. Even advanced users, however, should be satisfied with the functionality provided by \code{\link{genericmturkr}}.}
\value{A list of class \dQuote{MTurkResponse} containing:
  \item{request.url}{The URL of the MTurk API REST request.}
  \item{request.id}{The Request ID created by the API request.}
  \item{valid}{A logical indicating whether or not the request was valid and thus executed as intended.}
  \item{xml}{A character string containing the XML API response.}
  \item{xml.parsed}{The parsed XML response, if requested.}
}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMechanicalTurkRequester/MakingRequestsArticle.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{ParseErrorCodes}}
}
