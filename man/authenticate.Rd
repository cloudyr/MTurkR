\name{authenticate}
\alias{authenticate}
\title{Authenticate an MTurk Request}
\description{A (mostly internal) workhorse function to authenticate requests. This is only supplied for use by advanced users.}
\usage{
authenticate(operation, secret,
             service = "AWSMechanicalTurkRequester", version = "2012-03-25")
}
\arguments{
  \item{operation}{A character string containing the operation to be performed.}
  \item{secret}{The AWS Secret Access Key used to encode the authentication signature.}
  \item{service}{The MTurk service to which the authenticated request will be sent. Supplied only for advanced users.}
  \item{version}{The version of the MTurk API that will be used. Default is the most current version.}
}
\details{This is a mostly internal function that authenticates MTurk API requests. It is made available for use by advanced users to authenticate custom requests.}
\value{A list containing the operation to be performed, a base64 encoded, HMAC signature character string, and a formatted timestamp.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMechanicalTurkRequester/MakingRequestsArticle.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{request}}
}
%\examples{}
