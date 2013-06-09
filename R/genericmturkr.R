genericmturkr <-
function (operation, parameters = NULL, keypair = credentials(), 
    print = TRUE, browser = FALSE, log.requests = TRUE, sandbox = FALSE, 
    xml.parse = TRUE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- operation
    auth <- authenticate(operation, secret)
    GETparameters <- parameters
    request <- request(keyid, auth$operation, auth$signature, 
        auth$timestamp, GETparameters, browser = browser, log.requests = log.requests, 
        sandbox = sandbox, xml.parse = xml.parse)
    if (request$valid == TRUE & print == TRUE) {
        cat("Operation (", operation, ") Successful\n", sep = "")
        return(request)
    }
    else if (request$valid == FALSE & print == TRUE) {
        cat("Invalid Request\n")
        return(request)
    }
    else invisible(request)
}
