GetQualificationType <-
qualtype <-
function (qual, keypair = credentials(), print = getOption('MTurkR.print'),
    browser = getOption('MTurkR.browser'), log.requests = getOption('MTurkR.log'),
    sandbox = getOption('MTurkR.sandbox'), return.qual.dataframe = TRUE,
    validation.test = getOption('MTurkR.test')) {
    if(!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "GetQualificationType"
    if(is.null(qual)) 
        stop("Must specify QualificationTypeId")
    else
		GETparameters <- paste("&QualificationTypeId=", qual, sep = "")
    auth <- authenticate(operation, secret)
    if(browser == TRUE) {
        request <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, browser = browser, 
            sandbox = sandbox, validation.test = validation.test)
		if(validation.test)
			invisible(request)
    }
    else {
        request <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, log.requests = log.requests, 
            sandbox = sandbox, validation.test = validation.test)
		if(validation.test)
			invisible(request)
        if(request$valid == TRUE) {
            Qualifications <- QualificationTypesToDataFrame(xml = request$xml)
            if(print == TRUE) 
                message("QualificationType Retrieved: ", qual)
        }
        else if(request$valid == FALSE) {
            if(print == TRUE) 
                warning("Invalid Request")
        }
    }
    if(print == TRUE) 
        return(Qualifications)
    else
		invisible(Qualifications)
}
