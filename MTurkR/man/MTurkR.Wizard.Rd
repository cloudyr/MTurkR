\name{MTurkR.Wizard}
\alias{MTurkR.Wizard}
\alias{mturkr.wizard}
\alias{wizard.simple}
\title{Interactive MTurkR Mode}
\description{An interactive, menu-based wizard to perform MTurkR functions. Designed for beginners and those with aversion to the programming required elsewhere in the package.}
\usage{
MTurkR.Wizard(style="tcltk", sandbox = FALSE)
wizard.simple(graphics = FALSE, sandbox = NULL)
}
\arguments{
  \item{style}{The default \code{tcltk}, opens a full-featured GUI for MTurkR. \code{simple} opens a simpler, text-based wizard (provided by \code{wizard.simple}) for performing some functions; \code{simpleGUI} opens the same simpler wizard, with graphical rather than text-based menus.}
  \item{graphics}{Optionally use graphical menus, if available, for the simple wizard. See \code{\link{menu}}. Default is \code{FALSE}.}
  \item{sandbox}{Optionally execute all requests in the MTurk sandbox rather than the live server.  Default (in \code{MTurkR.Wizard}) is FALSE; the default in \code{wizard.simple} is NULL (with the wizard prompting for a value on load.}
%  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
}
\details{
An interactive, menu-based wizard (with optionally graphical menus) to perform most MTurkR operations. It is intended as a way for MTurk (and MTurkR) beginners to quickly create and monitor HITs; approve and reject assignments; notify, bonus, and block/unblock workers; manage Qualifications; monitor MTurk statistics; and interact with the MTurk Requester User Interface (RUI). All functionality accepts basic inputs interactively and executes requests without programming individual commands.

Two particularly helpful features are worth highlighting. The wizard provides a point-and-click interface for approving and rejecting individual assignments, that interactively displays assignment content and executes approval/rejection decisions with ease. The wizard also provides analogous functionality for granting and rejecting qualification requests.

The wizard remains under active development and detailed documentation will hopefully be available under a subsequent release.

\code{mturkr.wizard()} is an alias for \code{MTurkR.Wizard}.
}
\value{Currently returns nothing.}
%\references{}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
\dontrun{
MTurkR.Wizard()
}
}
