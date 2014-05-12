DisposeQualificationType <-
disposequal <-
function (qual, keypair = getOption('MTurkR.keypair'), print = getOption('MTurkR.print'),
    log.requests = getOption('MTurkR.log'),
    sandbox = getOption('MTurkR.sandbox'), validation.test = getOption('MTurkR.test')) {
    if(is.null(keypair))
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "DisposeQualificationType"
    if(is.null(qual)) 
        stop("Must specify QualificationTypeId")
    else {
        if(is.factor(qual))
            qual <- as.character(qual)
        GETparameters <- paste("&QualificationTypeId=", qual, sep = "")
    }
    QualificationTypes <- setNames(data.frame(matrix(ncol = 2)),
                            c("QualificationTypeId", "Valid"))
    request <- request(keypair[1], operation, secret=keypair[2],
        GETparameters = GETparameters, log.requests = log.requests, 
        sandbox = sandbox, validation.test = validation.test)
    if(validation.test)
        return(invisible(request))
    if(request$valid == TRUE) {
        QualificationTypes[1, ] <- c(qual, request$valid)
        if(print == TRUE)
            message("QualificationType ", qual, " Disposed")
        QualificationTypes$Valid <-
            factor(QualificationTypes$Valid, levels=c('TRUE','FALSE'))
        return(QualificationTypes)
    }
    else if(request$valid == FALSE) {
        if(print == TRUE) 
            warning("Invalid Request\n")
        return(NULL)
    }
}
