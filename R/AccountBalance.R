AccountBalance <-
accountbalance <-
getbalance <-
function(verbose = getOption('MTurkR.verbose'), ...) 
{
    # temporary check for `print` argument (remove after v1.0)
    if('print' %in% names(list(...)) && is.null(verbose))
        verbose <- list(...)$print
    operation <- "GetAccountBalance"
    request <- request(operation, verbose = verbose, ...)
    if(is.null(request$valid) || !request$valid) {
        return(request)
    } else if(request$valid) {
        balance <- strsplit(strsplit(request$xml, "<Amount>")[[1]][2], "</Amount>")[[1]][1]
        balanceformatted <- strsplit(strsplit(request$xml, 
            "<FormattedPrice>")[[1]][2], "</FormattedPrice>")[[1]][1]
        request$balance <- balance
        if(verbose)
            message(paste("Balance: ", balanceformatted, "\n", sep = ""))
        return(invisible(request))
    }
}
