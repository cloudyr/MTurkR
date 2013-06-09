block <-
BlockWorker <-
BlockWorkers <-
function (workers, reasons, keypair = credentials(), print = TRUE, 
    browser = FALSE, log.requests = TRUE, sandbox = FALSE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "BlockWorker"
    if (is.null(reasons)) 
        stop("Must specify one reason for block for all workers or one reason per worker")
    if (length(workers) > 1) {
        if (length(reasons) == 1) 
            reasons <- rep(reasons, length(workers))
        else if (!length(workers) == length(reasons)) 
            stop("length(reasons) must equal length(workers) or 1")
    }
    Workers <- data.frame(matrix(ncol = 3))
    names(Workers) <- c("WorkerId", "Reason", "Valid")
    for (i in 1:length(workers)) {
        GETparameters <- paste("&WorkerId=", workers[i], "&Reason=", 
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
            Workers[i, ] = c(workers[i], reasons[i], request$valid)
            if (request$valid == TRUE & print == TRUE) {
                cat(i, ": Worker ", workers[i], " Blocked\n", 
                  sep = "")
            }
            else if (request$valid == FALSE & print == TRUE) {
                cat(i, ": Invalid Request for worker ", workers[i], 
                  "\n", sep = "")
            }
        }
    }
    invisible(Workers)
}
