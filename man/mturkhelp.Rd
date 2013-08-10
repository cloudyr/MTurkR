\name{mturkhelp}
\alias{mturkhelp}
\alias{APIReference}
\title{Get some help}
\description{Functions to obtain help with the MTurk API (as opposed to the MTurkR package itself). \code{mturkrhelp} makes help requests directly to the API, while \code{APIReference} simply loads the API documentation.}
\usage{
mturkhelp(	about, helptype = NULL, keypair = credentials(), print = TRUE, 
			browser = FALSE, log.requests = TRUE, validation.test = FALSE)

APIReference()
}
\arguments{
  \item{about}{A character string containing a help query.}
  \item{helptype}{Optionally either \dQuote{Operation} or \dQuote{ResponseGroup}.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{validation.test}{A logical specifying whether only the pre-request checks should be conducted and the request URL returned (without executing the request). Default is \code{FALSE}.}
}
\details{Some basic functions to get help with the MTurk API (as opposed to the MTurkR package). Whiel intended to be user-friendly, sophisticated use of MTurkR may require some understanding of hte MTurk API, which is easily accessed here.}
\value{Nothing. For \code{mturkhelp}: help information is printed to the standard output. For \code{APIReference}: the MTurk API documentation is opened in the user's default browser.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMechanicalTurkGettingStartedGuide/Welcome.html?r=9808}{Getting Started Guide}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMechanicalTurkRequester/Welcome.html?r=349}{Developer Guide}

\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/Welcome.html?r=3235}{API Reference}

\href{http://awsdocs.s3.amazonaws.com/MechTurk/latest/QRC-MTurk.pdf}{Quick Reference}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
\dontrun{
mturkhelp(about="GrantBonus",helptype="Operation")
APIReference()
}
}
\keyword{Documentation}