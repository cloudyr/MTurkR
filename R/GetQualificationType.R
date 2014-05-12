GetQualificationType <-
qualtype <-
function (qual, keypair = getOption('MTurkR.keypair'), print = getOption('MTurkR.print'),
    log.requests = getOption('MTurkR.log'),
    sandbox = getOption('MTurkR.sandbox'), return.qual.dataframe = TRUE,
    validation.test = getOption('MTurkR.test')) {
    if(is.null(keypair))
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "GetQualificationType"
    if(is.null(qual)) 
        stop("Must specify QualificationTypeId")
    GETparameters <- paste("&QualificationTypeId=", as.character(qual), sep = "") 
    request <- request(keypair[1], operation, secret=keypair[2],
        GETparameters = GETparameters, log.requests = log.requests, 
        sandbox = sandbox, validation.test = validation.test)
    if(validation.test)
        return(invisible(request))
    if(request$valid == TRUE) {
        Qualifications <- QualificationTypesToDataFrame(xml = request$xml)
        if(print == TRUE) 
            message("QualificationType Retrieved: ", qual)
    }
    else if(request$valid == FALSE & print == TRUE)
        warning("Invalid Request")
    return(Qualifications)
}
