\name{credentials}
\alias{credentials}
\title{Specify MTurk/AWS Credentials}
\description{Specify your Amazon Web Services (including MTurk) access credentials: (1) Access Key ID and (2) Secret Access Key. These are used to authenticate requests to the MTurk API and must be specified before any operations can be successfully performed by MTurkR.}
\usage{
credentials(keypair = NULL)
}
\arguments{
  \item{keypair}{A two-item character vector containing the AWS Access Key ID and AWS Secret Access Key, in that order}
}
\details{This is the first operation that needs to be performed before any MTurK API requests can be successfully performed in a given MTurkR session. The function simply stores the Access Key ID and the Secret Access Key as a two-item character vector in \code{getOption('MTurkR.keypair')}, which is called by default by all MTurkR operations. This operation can also be performed by loading \code{\link{MTurkR.Wizard}}, which prompts for the keypair the first time it loads in a given R session.

}
\value{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position.}
\references{
\href{https://aws-portal.amazon.com/gp/aws/securityCredentials}{AWS Access Credentials}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
\dontrun{
credentials(keypair=c("AKIAJFYYWO3EEXAMPLE","Bpyq5ZqpQGshEKfcfd8CwUXEXAMPLE/tD0oqYGvI"))
}
}