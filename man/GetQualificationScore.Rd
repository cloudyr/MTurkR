\name{GetQualificationScore}
\alias{GetQualificationScore}
\alias{qualscore}
\title{Get a Worker's Qualification Score}
\description{Get a Worker's score for a specific Qualification. You can only retrieve scores for custom QualificationTypes. Scores for built-in QualificationTypes should be retrieved with \code{\link{GetWorkerStatistic}}.}
\usage{
GetQualificationScore(qual, workers, verbose = getOption('MTurkR.verbose'), ...)
}
\arguments{
  \item{qual}{A character string containing a QualificationTypeId for a custom QualificationType.}
  \item{workers}{A character string containing a WorkerId, or a vector of character strings containing multiple WorkerIds, whose Qualification Scores you want to retrieve.}
  \item{verbose}{Optionally print the results of the API request to the standard output. Default is taken from \code{getOption('MturkR.verbose')}.}
  \item{...}{Additional arguments passed to \code{\link{request}}.}
}
\details{
A function to retrive one or more scores for a specified QualificationType. To retrieve all Qualifications of a given QualificationType, use \code{\link{GetQualifications}} instead.

\code{qualscore()} is an alias.
}
\value{A dataframe containing the WorkerId, QualificationTypeId, and the Qualification score for each request.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_GetQualificationScoreOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{UpdateQualificationScore}}

\code{\link{GetQualifications}}
}
\examples{
\dontrun{
GetQualificationScore("2YCIA0RYNJ9262B1D82MPTUEXAMPLE","A1RO9UJNWXMU65")
}
}
\keyword{Qualifications}