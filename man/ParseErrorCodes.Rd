\name{ParseErrorCodes}
\alias{ParseErrorCodes}
\title{Parse MTurk API Errors}
\description{Mostly internal function to parse MTurk API error codes (from XML) into an R dataframe.}
\usage{
ParseErrorCodes(xml = NULL, xml.parsed = NULL)
}
\arguments{
  \item{xml}{A character string containing a full MTurk XML response. Must specify this or the \code{xml.parsed} parameter.}
  \item{xml.parsed}{A full MTurk XML response parsed by the \code{xmlParse}. Must specify this or the \code{xml} parameter.}
}
\details{A mostly internal function to parse MTurk API error codes (returned in XML format) into an R dataframe. API errors only emerge when requests are not valid and this is largely an internal function, but may be minimally helpful when reviewing invalid requests in the log file (see \code{\link{readlogfile}}).}
\value{A dataframe containing rows for each API error code and its corresponding message. This dataframe is returned by \code{\link{request}} whenever a request is invalid.}
%\references{}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{request}}
}
%\examples{}
\keyword{error}
