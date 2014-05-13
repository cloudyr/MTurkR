GetBlockedWorkers <-
blockedworkers <-
function(pagenumber = NULL, pagesize = NULL,
         verbose = getOption('MTurkR.verbose'), ...) {
    # temporary check for `print` argument (remove after v1.0)
    if('print' %in% names(list(...)) && is.null(verbose))
        verbose <- list(...)$print
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
    request <- request(operation, GETparameters = GETparameters, ...)
    if(is.null(request$valid))
        return(request)
    if(request$valid == TRUE) {
        Workers <- as.data.frame.WorkerBlock(xml.pased = xmlParse(request$xml))
        if(verbose) {
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
