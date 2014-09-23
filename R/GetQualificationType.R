GetQualificationType <-
qualtype <-
function(qual, verbose = getOption('MTurkR.verbose', TRUE), ...) {
    # temporary check for `print` argument (remove after v1.0)
    if('print' %in% names(list(...)) && is.null(verbose))
        verbose <- list(...)$print
    operation <- "GetQualificationType"
    if(is.null(qual)) 
        stop("Must specify QualificationTypeId")
    GETparameters <- paste("&QualificationTypeId=", as.character(qual), sep = "") 
    request <- request(operation, GETparameters = GETparameters, ...)
    if(is.null(request$valid))
        return(request)
    if(request$valid) {
        s <- getOption("stringsAsFactors")
        options("stringsAsFactors" = FALSE)
        Qualifications <- as.data.frame.QualificationTypes(xml.parsed = xmlParse(request$xml))
        options("stringsAsFactors" = s)
        if(verbose) 
            message("QualificationType Retrieved: ", qual)
    } else if(!request$valid) {
        Qualifications <-
        setNames(data.frame(matrix(nrow=0, ncol=13)),
                 c("QualificationTypeId",
                   "CreationTime",
                   "Name",
                   "Description",
                   "Keywords",
                   "QualificationTypeStatus",
                   "AutoGranted",
                   "AutoGrantedValue",
                   "IsRequestable",
                   "RetryDelayInSeconds",
                   "TestDurationInSeconds",
                   "Test",
                   "AnswerKey"))
        if(verbose)
            warning("Invalid Request")
    }
    return(Qualifications)
}
