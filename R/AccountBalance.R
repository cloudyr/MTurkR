AccountBalance <-
accountbalance <-
getbalance <-
function (keypair = credentials(), print = TRUE, browser = FALSE, 
    log.requests = TRUE, sandbox = FALSE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "GetAccountBalance"
    GETparameters = ""
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
            balance <- strsplit(strsplit(request$xml, "<Amount>")[[1]][2], 
                "</Amount>")[[1]][1]
            balanceformatted <- strsplit(strsplit(request$xml, 
                "<FormattedPrice>")[[1]][2], "</FormattedPrice>")[[1]][1]
            if (print == TRUE)
                message(paste("Balance: ", balanceformatted, "\n", sep = ""))
            invisible(balance)
        }
        else if (request$valid == FALSE) {
            invisible(NULL)
        }
    }
}
