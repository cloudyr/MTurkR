AccountBalance <-
accountbalance <-
getbalance <-
function(verbose = getOption('MTurkR.verbose', TRUE), ...) 
{
    operation <- "GetAccountBalance"
    request <- request(operation, verbose = verbose, ...)
    if (is.null(request$valid) || !request$valid) {
        return(request)
    } else if(request$valid) {
        balance <- strsplit(strsplit(request$xml, "<Amount>")[[1]][2], "</Amount>")[[1]][1]
        balanceformatted <- strsplit(strsplit(request$xml, 
            "<FormattedPrice>")[[1]][2], "</FormattedPrice>")[[1]][1]
        request$balance <- balance
        if (verbose) {
            message(paste("Balance: ", balanceformatted, "\n", sep = ""))
        }
        return(invisible(request))
    }
}
