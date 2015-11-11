\name{MTurkR-package}
\alias{MTurkR-package}
\alias{MTurkR}
\docType{package}
\title{R Client for the MTurk Requester API}
\description{
This package provides access to the Amazon Mechanical Turk (MTurk) Requester API. The package provides users of the MTurk Requester User Interface  with access to a variety of functions currently unavailable to them (the creation and maintenance of worker Qualifications, email notifications to workers through \code{\link{ContactWorker}}, automated reviewing of assignments using Review Policies, and streamlined bonus payments through \code{\link{GrantBonus}}). It also provides users with all functions available in the RUI directly in R as well as a large number of other functions, and a simple, interactive command-line tool for performing many operations. A relatively fully featured, cross-platform graphical user interface is available in a separate add-on package called \href{https://github.com/leeper/MTurkRGUI}{MTurkRGUI}.

Most users will find themselves using three principal functions: \code{\link{CreateHIT}}, \code{\link{GetAssignments}}, and \code{\link{ApproveAssignments}}, to create one or more HITs on the MTurk server, to retrieve completed assignments, and to approve assignments (and thus pay workers), respectively. As task complexity increases, additional functions are provided to handle worker qualifications, bonuses, emails to workers, automated review policies, bulk creation of HITs, and so forth.

Critically important, nothing in MTurkR will work during a given session without either first setting AWS credentials. The easiest way to do this is to specify \samp{AWS_ACCESS_KEY_ID} and \samp{AWS_SECRET_ACCESS_KEY} environment variables using \code{Sys.setenv()} or by placing these values in an .Renviron file. Credentials can also be specified atomically within each MTurkR function as the \code{keypair} argument, which accepts the credentials as a two-element character vector.

There are five common parameters that can be specified in most MTurkR functions: \code{keypair}, \code{verbose}, \code{log.requests}, and \code{sandbox}. The first of these is the AWS credentials parameter just described and the latter four are logicals. \code{verbose} causes certain information to be displayed on the standard output when functions are executed. \code{log.requests} records details of API calls in the working directory (see \code{\link{readlogfile}}). This is slightly time-consuming, so it can be omitted if you do not need access to log information (e.g., for troubleshooting). Setting the parameter \code{sandbox=TRUE} executes requests in the developer sandbox rather than the live server, which is a useful test environment. All of these options can be set globally using, for example, \code{options('MTurkR.sandbox')}.
}
\author{
Thomas J. Leeper

Maintainer: Thomas J. Leeper <thosjleeper@gmail.com>
}
\references{
\href{http://www.mturk.com}{Amazon Mechanical Turk}

\href{https://github.com/leeper/MTurkR/wiki}{The MTurkR Wiki}, which contains numerous code examples and tutorials.

\href{https://github.com/leeper/MTurkRGUI}{The MTurkRGUI Package}, which provides a graphical user interface for MTurkR.

\href{http://aws.amazon.com/documentation/mturk/}{Amazon Mechanical Turk API Documentation}
}
\keyword{ package }
%\seealso{}
%\examples{}
