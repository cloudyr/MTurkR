AccountBalance <-
accountbalance <-
getbalance <-
function (keypair = getOption('MTurkR.keypair'), print = getOption('MTurkR.print'),
    log.requests = getOption('MTurkR.log'),
    sandbox = getOption('MTurkR.sandbox'), validation.test = getOption('MTurkR.test')) 
{
    if (is.null(keypair))
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "GetAccountBalance"
    GETparameters = ""
    request <- request(keypair[1], operation, secret=keypair[2],
        GETparameters = GETparameters, log.requests = log.requests, 
        sandbox = sandbox, validation.test = validation.test)
    if(validation.test)
        return(invisible(request))
    if (request$valid == TRUE) {
        balance <- strsplit(strsplit(request$xml, "<Amount>")[[1]][2], "</Amount>")[[1]][1]
        balanceformatted <- strsplit(strsplit(request$xml, 
            "<FormattedPrice>")[[1]][2], "</FormattedPrice>")[[1]][1]
        if (print == TRUE)
            message(paste("Balance: ", balanceformatted, "\n", sep = ""))
        return(invisible(balance))
    }
    else if (request$valid == FALSE) {
        return(NULL)
    }
}
