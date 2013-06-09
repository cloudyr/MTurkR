UnblockWorker <-
UnblockWorkers <-
unblock <-
function (workers, reasons = NULL, keypair = credentials(), print = TRUE, 
    browser = FALSE, log.requests = TRUE, sandbox = FALSE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "UnblockWorker"
    if (length(workers) > 1) {
        if (!is.null(reasons)) {
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
        auth <- authenticate(operation, secret)
        if (browser == TRUE) {
            request <- request(keyid, auth$operation, auth$signature, 
                auth$timestamp, GETparameters, browser = browser, 
                sandbox = sandbox)
        }
        else {
            request <- request(keyid, auth$operation, auth$signature, 
                auth$timestamp, GETparameters, log.requests = log.requests, 
                sandbox = sandbox)
            if (request$valid == TRUE) {
                if (print == TRUE) 
                  cat(i, ": Worker ", workers[i], " Unblocked\n", 
                    sep = "")
                if (is.null(reasons)) 
                  Workers[i, ] = c(workers[i], NA, request$valid)
                else Workers[i, ] = c(workers[i], reasons[i], 
                  request$valid)
            }
            else if (request$valid == FALSE) {
                if (print == TRUE) 
                  cat(i, ": Invalid Request for worker ", workers[i], 
                    "\n", sep = "")
            }
        }
    }
    if (print == TRUE) 
        return(Workers)
    else invisible(Workers)
}
