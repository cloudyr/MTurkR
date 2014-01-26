DisposeQualificationType <-
disposequal <-
function (qual, keypair = credentials(), print = getOption('MTurkR.print'),
    browser = getOption('MTurkR.browser'), log.requests = getOption('MTurkR.log'),
    sandbox = getOption('MTurkR.sandbox'), validation.test = getOption('MTurkR.test')) {
    if(!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "DisposeQualificationType"
    if(is.null(qual)) 
        stop("Must specify QualificationTypeId")
    else {
        if(is.factor(qual))
            qual <- as.character(qual)
        GETparameters <- paste("&QualificationTypeId=", qual, sep = "")
    }
    auth <- authenticate(operation, secret)
    if(browser == TRUE) {
        request <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, browser = browser, 
            sandbox = sandbox, validation.test = validation.test)
		if(validation.test)
			return(invisible(request))
    }
    else {
        QualificationTypes <- setNames(data.frame(matrix(ncol = 2)),
                                c("QualificationTypeId", "Valid"))
        request <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, log.requests = log.requests, 
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
}
