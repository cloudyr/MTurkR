UnblockWorker <-
UnblockWorkers <-
unblock <-
function (workers, reasons = NULL, keypair = getOption('MTurkR.keypair'),
    print = getOption('MTurkR.print'), 
    browser = getOption('MTurkR.browser'), log.requests = getOption('MTurkR.log'),
    sandbox = getOption('MTurkR.sandbox'), validation.test = getOption('MTurkR.test')) 
{
    if(is.null(keypair))
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "UnblockWorker"
    if(is.factor(workers))
        workers <- as.character(workers)
    if (length(workers) > 1) {
        if (!is.null(reasons)) {
            if(is.factor(reasons))
                reasons <- as.character(reasons)
            if (length(reasons) == 1) 
                reasons <- rep(reasons, length(workers))
            else if (!length(workers) == length(reasons)) 
                stop("length(reason) must equal length(workers) or 1")
        }
    }
    Workers <- data.frame(matrix(ncol = 3))
    names(Workers) <- c("WorkerId", "Reason", "Valid")
    for (i in 1:length(workers)) {
        GETparameters <- paste("&WorkerId=", workers[i], sep = "")
        if (!is.null(reasons[i])) 
            GETparameters <- paste(GETparameters, "&Reason=", 
                curlEscape(reasons[i]), sep = "")
        if (browser == TRUE) {
            request <- request(keypair[1], operation, secret=keypair[2],
                GETparameters = GETparameters, browser = browser, 
                sandbox = sandbox, validation.test = validation.test)
            if(validation.test)
                return(invisible(request))
        }
        else {
            request <- request(keypair[1], operation, secret=keypair[2],
                GETparameters = GETparameters, log.requests = log.requests, 
                sandbox = sandbox, validation.test = validation.test)
            if(validation.test)
                return(invisible(request))
            if (request$valid == TRUE) {
                if (print == TRUE) 
                    message(i, ": Worker ", workers[i], " Unblocked")
                if (is.null(reasons)) 
                    Workers[i, ] = c(workers[i], NA, request$valid)
                else
                    Workers[i, ] = c(workers[i], reasons[i], request$valid)
            }
            else if (request$valid == FALSE & print == TRUE)
                warning(i, ": Invalid Request for worker ", workers[i])
        }
    }
    Workers$Valid <- factor(Workers$Valid, levels=c('TRUE','FALSE'))
    return(Workers)
}
