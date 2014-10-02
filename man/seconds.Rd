\name{seconds}
\alias{seconds}
\title{Convert arbitrary times to seconds}
\description{A convenience function to convert arbitrary numbers of days, hours, minutes, and/or seconds into seconds.}
\usage{
seconds(days = NULL, hours = NULL, minutes = NULL, seconds = NULL)
}
\arguments{
  \item{days}{An optional number of days.}
  \item{hours}{An optional number of hours.}
  \item{minutes}{An optional number of minutes.}
  \item{seconds}{An optional number of seconds.}
}
\details{A convenience function to convert arbitrary numbers of days, hours, minutes, and/or seconds into seconds. For example, to be used in setting a HIT expiration time. MTurk only accepts times (e.g., for HIT expirations, or the duration of assignments) in seconds. This function returns an integer value equal to the number of seconds of the input, and can be used atomically within other MTurkR calls (e.g., \code{\link{CreateHIT}}).}
\value{An integer equal to the requested amount of time in seconds.}
%\references{}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
seconds(hours=5, seconds=15)
seconds(days=1)
}
