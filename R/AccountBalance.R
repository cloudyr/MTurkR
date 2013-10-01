AccountBalance <-
accountbalance <-
getbalance <-
function (keypair = credentials(), print = getOption('MTurkR.print'),
    browser = getOption('MTurkR.browser'), log.requests = getOption('MTurkR.log'),
    sandbox = getOption('MTurkR.sandbox'), validation.test = getOption('MTurkR.test')) 
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
            sandbox = sandbox, validation.test = validation.test)
		if(validation.test)
			invisible(request)
    }
    else {
        request <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, log.requests = log.requests, 
            sandbox = sandbox, validation.test = validation.test)
		if(validation.test)
			invisible(request)
        if (request$valid == TRUE) {
            balance <- strsplit(strsplit(request$xml, "<Amount>")[[1]][2], "</Amount>")[[1]][1]
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
