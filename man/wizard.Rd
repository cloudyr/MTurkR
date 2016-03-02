\name{wizard.simple}
\alias{wizard.simple}
\title{Interactive MTurkR Mode}
\description{An interactive, menu-based console wizard to perform MTurkR functions.}
\usage{
wizard.simple(graphics = FALSE, sandbox = NULL, ...)
}
\arguments{
  \item{graphics}{Optionally use graphical menus, if available, for the simple wizard. See \code{\link{menu}}. Default is \code{FALSE}.}
  \item{sandbox}{Optionally execute all requests in the MTurk sandbox rather than the live server.  Default (in \code{MTurkR.Wizard}) is FALSE; the default in \code{wizard.simple} is NULL (with the wizard prompting for a value on load.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
An interactive, menu-based wizard (with optionally graphical menus) to perform most MTurkR operations. It is intended as a way for MTurk (and MTurkR) beginners to quickly create and monitor HITs; approve and reject assignments; notify, bonus, and block/unblock workers; manage Qualifications; monitor MTurk statistics; and interact with the MTurk Requester User Interface (RUI). All functionality accepts basic inputs interactively and executes requests without programming individual commands.

The wizard remains under development, but detailed documentation can be found at \url{https://github.com/leeper/MTurkR/wiki/Wizard-Text-Based}.

A more fully featured, interactive graphical wizard, which was previously available as part of MTurkR, has been refactored into a separate package called MTurkRGUI, available from CRAN. Please install and load that package for full documentation, or visit \url{https://github.com/leeper/MTurkR/wiki/Wizard\%20Graphical} for more details.
}
\value{Currently returns nothing.}
%\references{}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
\dontrun{
wizard.simple()
}
}
