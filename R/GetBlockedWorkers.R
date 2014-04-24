GetBlockedWorkers <-
blockedworkers <-
function (pagenumber = NULL, pagesize = NULL, keypair = getOption('MTurkR.keypair'), 
    print = getOption('MTurkR.print'), browser = getOption('MTurkR.browser'),
    log.requests = getOption('MTurkR.log'), sandbox = getOption('MTurkR.sandbox'),
    validation.test = getOption('MTurkR.test')) {
    if(!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    } else
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "GetBlockedWorkers"
    GETparameters <- ""
    if(!is.null(pagesize)) {
        if(as.numeric(pagesize) > 65535) 
            stop("'pagesize' must be <=65535")
        else
            GETparameters <- paste(GETparameters, "&PageSize=", pagesize, sep = "")
    }
    else
        GETparameters <- paste(GETparameters, "&PageSize=65535", sep = "")
    if(!is.null(pagenumber)) {
        if(as.numeric(pagenumber) < 1) 
            stop("'pagenumber' must be > 1")
        else 
            GETparameters <- paste(GETparameters, "&PageNumber=", pagenumber, sep = "")
    }
    Workers <- NA
    auth <- authenticate(operation, secret)
    if(browser == TRUE) {
        request <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, browser = browser, 
            sandbox = sandbox, validation.test = validation.test)
        if(validation.test)
            return(invisible(request))
    }
    else {
        request <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, log.requests = log.requests, 
            sandbox = sandbox, validation.test = validation.test)
        if(validation.test)
            return(invisible(request))
        if(request$valid == TRUE) {
            Workers <- WorkerBlockToDataFrame(xml = request$xml)
            if(print == TRUE) {
                if(!is.null(Workers) && dim(Workers)[1] > 0) 
                    message(dim(Workers)[1], " Blocked Workers Retrieved")
                else
                    message("No Blocked Workers Retrieved")
            }
            return(Workers)
        }
        else
            return(NULL)
    }
}
