genericmturkr <-
function (operation, parameters = NULL, keypair = credentials(), 
    print = getOption('MTurkR.print'), browser = getOption('MTurkR.browser'),
    log.requests = getOption('MTurkR.log'), sandbox = getOption('MTurkR.sandbox'), 
    xml.parse = TRUE, validation.test = getOption('MTurkR.test')){
    if(!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else
        stop("No keypair provided or 'credentials' object not stored")
    operation <- operation
    auth <- authenticate(operation, secret)
    GETparameters <- parameters
    if(browser == TRUE) {
		request <- request(keyid, auth$operation, auth$signature, 
			auth$timestamp, GETparameters, browser = browser, log.requests = log.requests, 
			sandbox = sandbox, xml.parse = xml.parse, validation.test = validation.test)
		if(validation.test)
			return(invisible(request))
	}
	else{
		request <- request(keyid, auth$operation, auth$signature, 
			auth$timestamp, GETparameters, browser = browser, log.requests = log.requests, 
			sandbox = sandbox, xml.parse = xml.parse, validation.test = validation.test)
		if(validation.test)
			return(invisible(request))
		if(request$valid == TRUE & print == TRUE)
			message("Operation (", operation, ") Successful")
		else if(request$valid == FALSE & print==TRUE)
            warning("Invalid Request")
	    return(request)
	}
}
