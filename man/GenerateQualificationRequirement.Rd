\name{GenerateQualificationRequirement}
\alias{GenerateQualificationRequirement}
\title{Generate QualificationRequirement}
\description{Generate a QualificationRequirement data structure for use with \code{\link{CreateHIT}} or \code{\link{RegisterHITType}}.}
\usage{
GenerateQualificationRequirement(qual, 
                                 comparator, 
                                 value, 
                                 preview = NULL, 
                                 qual.number = NULL)
}
\arguments{
  \item{qual}{A character string containing a QualificationTypeId, or a vector of QualificationTypeIds. This parameter also accepts shorthand labels for built-in QualificationTypes: \dQuote{Approved} (percent of assignments approved), \dQuote{NumberApproved} (number of assignments approved), \dQuote{Locale}, \dQuote{Adult}, and MTurk \dQuote{masters} QualificationTypes (\dQuote{Masters}).}
  \item{comparator}{A character string containing a comparator, or a vector of comparators, by which a worker's score of a qualification is compared to the specified \code{value}. One of \code{<},\code{<=},\code{>},\code{>=},\code{==},\code{!=},\dQuote{Exists},\dQuote{DoesNotExist},\dQuote{In},\dQuote{NotIn}. For \dQuote{Masters}-type qualifications, only \dQuote{Exists} and \dQuote{DoesNotExist} are available.}
  \item{value}{A numeric or character string value (or vector of such) against which workers scores will be compared. Must be a non-negative integer, except when \code{qualification="Locale"} (when it must be a two-digit country code, or a five-character \href{http://en.wikipedia.org/wiki/ISO_3166-2:US}{ISO 3166-2} identifier for a U.S. state of the form \code{US-MN} using the two-letter abbreviation for Minnesota, etc.) or when \code{comparator} is \dQuote{Exists} or \dQuote{DoesNotExist} (when it must be an empty character string). When using the \dQuote{In} or \dQuote{NotIn} comparators, each element in \code{value} can be a comma-separated string of up to 15 values (e.g., 15 discrete scores or 15 discrete locales to use for that Qualification).}
  \item{preview}{An optional logical specifying whether a worker must have the Qualification in order to preview the HIT on the MTurk worker site. The default is \code{FALSE}.}
  \item{qual.number}{An argument for primarily internal use.}
}
\details{
A convenience function to translate the details of a QualificationRequirement into the necessary structure for use in the \code{qual.req} parameter of \code{\link{CreateHIT}} or \code{\link{RegisterHITType}}. The function accepts three required parameters: \code{qual}, \code{comparator}, and \code{value}. \code{qual} must be a valid QualificationTypeId for either a built-in QualificaitonType (see \code{\link{ListQualificationTypes}}) or a custom QualificationType (e.g., one created with \code{\link{CreateQualificationType}}). Multiple QualificationRequirements can be generated in one call --- that is, if a requester intends to impose multiple QualificationRequirements on a single HITType, those requirements must be specified in a single call to \code{GenerateQualificationRequirements}. Once attached to a HITType, only workers who meet all of the specified QualificationRequirements can complete assignments for a HIT of that HITType.
}
\value{Returns a character string (of class \dQuote{QualificationRequirement}) containing URL-encoded QualificationRequirements.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_QualificationRequirementDataStructureArticle.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateHIT}}

\code{\link{RegisterHITType}}
}
\examples{
a <- ListQualificationTypes()[2,2] # Number of HITs Approved
# one QualificationRequirement
q1 <- GenerateQualificationRequirement(a,">","90")
# two QualificationRequirements
q2 <- GenerateQualificationRequirement(c("Locale","Approved"), 
                                       c("==",">"), 
                                       c("US",90), 
                                       preview = TRUE)
# one QualificationRequirement using the 'In' comparator
q3 <- GenerateQualificationRequirement("Locale","In","US,GB")
# two QualificationRequirements using the 'In' comparator
q4 <- GenerateQualificationRequirement(c("Locale","Approved"), 
                                       c("==",">"), 
                                       c("US,GB,DK",90), 
                                       preview = c(TRUE,FALSE))
# `Exists` comparator
q5 <- GenerateQualificationRequirement("Approved", "Exists", "")

# Masters `DoesNotExist` comparator
q6 <- GenerateQualificationRequirement("Masters", "DoesNotExist", "")

# U.S. state locale value
q7a <- GenerateQualificationRequirement("Locale","==","US-MN")
## with multiple states and an 'In' operator
q7b <- GenerateQualificationRequirement("Locale","In","US-MN,US-IL")

# Complex locale values
q8 <- GenerateQualificationRequirement("Locale","In","US-NY,US-WA,CA")
}
\keyword{Qualifications}
