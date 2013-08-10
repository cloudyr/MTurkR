\name{MTurkR-package}
\alias{MTurkR-package}
\alias{MTurkR}
\docType{package}
\title{Access to the Amazon Mechanical Turk (MTurk) API via R.}
\description{
This package provides access to the Amazon Mechanical Turk (MTurk) API via R, including all the basic API calls, plus additional wrappers to simplify multiple sequential calls and transform the XML returned by the API requests into R data structures (especially, dataframes). The package provides users of the MTurk Requester User Interface (RUI) with access to a variety of functions currently unavailable to them (the creation and maintenance of worker Qualifications, email notifications to workers through \code{\link{ContactWorker}}, and streamlined bonus payments through \code{\link{GrantBonus}}). It also provides users with all functions available in the RUI directly in R as well as a large number of other functions, with a relatively simple command-line interface.

The core functionality is enacted through \code{\link{request}} and \code{\link{authenticate}}, though most users are unlikely to use these functions directly. (And, the needs of even advanced users can probably be satisfied by the functionality of \code{\link{genericmturkr}} for making arbitrary API requests.) Instead, most users will find themselves using four principal functions: \code{\link{credentials}}, \code{\link{CreateHIT}}, \code{\link{GetAssignments}}, and \code{\link{ApproveAssignments}}, to define one's MTurk (AWS) credentials, to create one or more HITs on the Mturk server, to retrieve completed assignments, and to approve assignments (and thus pay workers), respectively. Critically important, nothing in MTurkR will work during a given session without either first setting AWS credentials with the \code{\link{credentials}} function or specifying those credentials atomically within each function.

There are five common parameters that can be specified in most MTurkR functions: \code{keypair}, \code{print}, \code{browser}, \code{log.requests}, and \code{sandbox}. The first of these is athe AWS credentials parameter just described (whose default can be set globally with \code{\link{credentials}}), and the latter four are logicals. \code{print} causes cretain information to be displayed on the standard output when functions are executed. \code{browser} executes the request in the user's default web browser rather than executing it in R. \code{log.requests} records details of API calls in the working directory (see \code{\link{readlogfile}}). \code{sandbox} executes requests in the developer sandbox rather than the live server.

A lightweight menu-based Wizard (called by \code{\link{MTurkR.Wizard}}) is also available for beginners to interactively connect with MTurk. The wizard is designed to quickly create HITs, approve and reject work, contact or bonus workers, grant Qualifications, and so forth. While helpful, the wizard is intended only to facilitate beginners and is not intended to mimic anything near the full functionality of individual MTurkR functions.
}
\details{
\tabular{ll}{
Package: \tab MTurkR\cr
Type: \tab Package\cr
Version: \tab 0.3.51\cr
Date: \tab 2013-08-10\cr
License: \tab GPL-2\cr
}
}
\author{
Thomas J. Leeper

Maintainer: Thomas J. Leeper <thosjleeper@gmail.com>
}
\references{
\href{http://leeper.github.io/MTurkR/}{MTurkR homepage}

\href{http://www.mturk.com}{Amazon Mechanical Turk}

\href{http://aws.amazon.com/documentation/mturk/}{Amazon Mechanical Turk API Documentation}
}
\keyword{ package }
%\seealso{}
%\examples{}
