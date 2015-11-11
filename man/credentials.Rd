\name{credentials}
\alias{credentials}
\title{Specify AWS Credentials}
\description{DEPRECATED}
\usage{
credentials(keypair = NULL)
}
\arguments{
  \item{keypair}{A two-item character vector containing the AWS Access Key ID and AWS Secret Access Key, in that order}
}
\details{
Do not use this function. It will be removed in the future.

The preferred way to load AWS credentials is now via environment variables. These environment variables can be set using \code{Sys.setenv()} or by using a local or global \samp{.Renviron} file. To specify AWS credentials, set the \samp{AWS_ACCESS_KEY_ID} and \samp{AWS_SECRET_ACCESS_KEY} environment variables. These can be generated via the AWS IAM management console (\samp{https://console.aws.amazon.com/iam/home?#security_credential}). Note that previously generated secret keys cannot be retrieved.

The \code{credentials()} function provides a legacy way of specify credentials and must be performed before any MTurk API requests can be successfully performed in a given session. The function simply stores the Access Key ID and the Secret Access Key as a two-item character vector in \code{getOption('MTurkR.keypair')}, which is called by default by all MTurkR operations. This operation can also be performed by loading \code{\link{wizard.simple}} or from within the MTurkRGUI package interface, which prompts for the keypair the first time it loads in a given R session.
}
\value{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position.}
%\references{}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
%\examples{}
