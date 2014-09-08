UnblockWorker <-
UnblockWorkers <-
unblock <-
function (workers, reasons = NULL, verbose = getOption('MTurkR.verbose'), ...){
    # temporary check for `print` argument (remove after v1.0)
    if('print' %in% names(list(...)) && is.null(verbose))
        verbose <- list(...)$print
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
        
        request <- request(operation, GETparameters = GETparameters, ...)
        if(is.null(request$valid))
            return(request)
        if (request$valid) {
            if(verbose) 
                message(i, ": Worker ", workers[i], " Unblocked")
            if(is.null(reasons)) 
                Workers[i, ] = c(workers[i], NA, request$valid)
            else
                Workers[i, ] = c(workers[i], reasons[i], request$valid)
        } else if (!request$valid & verbose)
            warning(i, ": Invalid Request for worker ", workers[i])
    }
    Workers$Valid <- factor(Workers$Valid, levels=c('TRUE','FALSE'))
    return(Workers)
}
