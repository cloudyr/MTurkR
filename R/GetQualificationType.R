GetQualificationType <-
qualtype <-
function(qual, verbose = getOption('MTurkR.verbose', TRUE), ...) {
    operation <- "GetQualificationType"
    if (is.null(qual)) {
        stop("Must specify QualificationTypeId")
    }
    GETparameters <- paste("&QualificationTypeId=", as.character(qual), sep = "") 
    request <- request(operation, GETparameters = GETparameters, ...)
    if (is.null(request$valid)) {
        return(request)
    }
    if (request$valid) {
        Qualifications <- as.data.frame.QualificationTypes(xml.parsed = xmlParse(request$xml))
        if (verbose) {
            message("QualificationType Retrieved: ", qual)
        }
    } else if (!request$valid) {
        Qualifications <-
        emptydf(nrow=0, ncol=13, c("QualificationTypeId", "CreationTime", "Name", "Description",
                                   "Keywords", "QualificationTypeStatus", "AutoGranted", "AutoGrantedValue",
                                   "IsRequestable", "RetryDelayInSeconds", "TestDurationInSeconds",
                                   "Test","AnswerKey"))
        if (verbose) {
            warning("Invalid Request")
        }
    }
    return(Qualifications)
}
