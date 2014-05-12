genericmturkr <-
function (operation, parameters = NULL, keypair = getOption('MTurkR.keypair'), 
    print = getOption('MTurkR.print'), browser = getOption('MTurkR.browser'),
    log.requests = getOption('MTurkR.log'), sandbox = getOption('MTurkR.sandbox'), 
    validation.test = getOption('MTurkR.test')){
    if(is.null(keypair))
        stop("No keypair provided or 'credentials' object not stored")
    operation <- operation
    GETparameters <- parameters
    if(browser == TRUE) {
        request <- request(keypair[1], operation, secret=keypair[2],
            GETparameters = GETparameters, browser = browser, log.requests = log.requests, 
            sandbox = sandbox, validation.test = validation.test)
        if(validation.test)
            return(invisible(request))
    }
    else{
        request <- request(keypair[1], operation, secret=keypair[2],
            GETparameters = GETparameters, browser = browser, log.requests = log.requests, 
            sandbox = sandbox, validation.test = validation.test)
        if(validation.test)
            return(invisible(request))
        if(request$valid == TRUE & print == TRUE)
            message("Operation (", operation, ") Successful")
        else if(request$valid == FALSE & print==TRUE)
            warning("Invalid Request")
        return(request)
    }
}
