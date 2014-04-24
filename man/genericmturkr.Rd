\name{genericmturkr}
\alias{genericmturkr}
\alias{makeGETparameters}
\title{Make Arbitrary MTurk API Requests}
\description{A function designed for advanced users to be able to make arbitrary requests to the MTurk API. Most users do not need this.}
\usage{
genericmturkr(operation, parameters = NULL, keypair = getOption('MTurkR.keypair'),
            print = getOption('MTurkR.print'), 
            browser = getOption('MTurkR.browser'),
            log.requests = getOption('MTurkR.log'),
            sandbox = getOption('MTurkR.sandbox'),
            xml.parse = TRUE, validation.test = getOption('MTurkR.test'))

makeGETparameters(parameter, value)
}

\arguments{
  \item{operation}{The MTurk API operation to be performed.}
  \item{parameters}{A character string containing URL query parameters, possibly as returned by \code{makeGETparameters}.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
  \item{xml.parse}{Whether the returned list should contain parsed XML.}
  \item{parameter}{A character string containing a parameter name, or a vector of character strings containing parameter names.}
  \item{value}{A character string containing a value for a parameter, or a vector of character strings containing the value for each of the parameters specified in \code{parameter}.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{
A simple wrapper for \code{\link{authenticate}} and \code{\link{request}} to provide advanced users with generic access to the MTurk API. \code{genericmturkr} executes arbitrary MTurk API requests for the specified MTurk API operation and any \code{parameters}. \code{makeGETparameters} provides a convenience function to generate these. Though this function does not underly the other MTurkR functions, it can produce similar effects (though response information is returned unformatted).

}
\value{A list containing the URL of the MTurk API REST request (\code{request.url}), the Request ID created by the API reqest (\code{request.id}), a logical indicating whether or not the request was valid and thus executed as intended (\code{valid}), and a character string containing the XML-formatted API response (\code{xml}).}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMechanicalTurkRequester/MakingRequests_MakingRESTRequestsArticle.html}{API Reference: Making REST Requests}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
\dontrun{
genericmturkr("GetAccountBalance")
}
\dontrun{
a <- makeGETparameters("AssignmentId","26XXH0JPPSI23H54YVG7BKLEXAMPLE")
data <- genericmturkr("GetAssignment",a)
}
}
