GetBlockedWorkers <-
blockedworkers <-
function(pagenumber = NULL, pagesize = NULL,
         verbose = getOption('MTurkR.verbose', TRUE), ...) {
    operation <- "GetBlockedWorkers"
    GETparameters <- ""
    if (!is.null(pagesize)) {
        if (as.numeric(pagesize) > 65535) {
            stop("'pagesize' must be <=65535")
        } else {
            GETparameters <- paste(GETparameters, "&PageSize=", pagesize, sep = "")
        }
    } else {
        GETparameters <- paste(GETparameters, "&PageSize=65535", sep = "")
    }
    if (!is.null(pagenumber)) {
        if (as.numeric(pagenumber) < 1) {
            stop("'pagenumber' must be > 1")
        } else {
            GETparameters <- paste(GETparameters, "&PageNumber=", pagenumber, sep = "")
        }
    }
    request <- request(operation, GETparameters = GETparameters, verbose = verbose, ...)
    if (is.null(request$valid) || !request$valid) {
        return(request)
    }
    Workers <- as.data.frame.WorkerBlock(xml.parsed = xmlParse(request$xml))
    if (verbose) {
       if (!is.null(Workers) && dim(Workers)[1] > 0) {
           message(dim(Workers)[1], " Blocked Workers Retrieved")
       } else {
           message("No Blocked Workers Retrieved")
        }
    }
    return(Workers)
}
