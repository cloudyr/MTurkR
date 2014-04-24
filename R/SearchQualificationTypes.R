SearchQualificationTypes <-
searchquals <-
function (query = NULL, only.mine = TRUE, only.requestable = FALSE, 
    return.all = FALSE, pagenumber = "1", pagesize = "10", sortproperty = "Name", 
    sortdirection = "Ascending", keypair = getOption('MTurkR.keypair'),
    print = getOption('MTurkR.print'), 
    log.requests = getOption('MTurkR.log'), sandbox = getOption('MTurkR.sandbox'),
    return.qual.dataframe = TRUE, validation.test = getOption('MTurkR.test')) {
    if(!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "SearchQualificationTypes"
    if(!sortproperty %in% c("Name")) 
        stop("'sortproperty' must be 'Name'")
    if(!sortdirection %in% c("Ascending", "Descending")) 
        stop("'sortdirection' must be 'Ascending' | 'Descending'")
    if(as.numeric(pagesize) < 1 || as.numeric(pagesize) > 100) 
        stop("'pagesize' must be in range (1,100)")
    if(as.numeric(pagenumber) < 1) 
        stop("'pagenumber' must be > 1")
    GETparameters <- ""
    if(!is.null(query)) 
        GETparameters <- paste(GETparameters, "&Query=", curlEscape(query), sep = "")
    if(only.mine == TRUE) 
        GETparameters <- paste(GETparameters, "&MustBeOwnedByCaller=", "true", sep = "")
    else if(only.mine == FALSE) 
        GETparameters <- paste(GETparameters, "&MustBeOwnedByCaller=", "false", sep = "")
    if(only.requestable == TRUE) 
        GETparameters <- paste(GETparameters, "&MustBeRequestable=", "true", sep = "")
    else if(only.requestable == FALSE) 
        GETparameters <- paste(GETparameters, "&MustBeRequestable=", "false", sep = "")
    batch <- function(operation, keyid, secret, GETparameters, 
        pagenumber, pagesize, sandbox = sandbox, validation.test = validation.test) {
        GETparameters <- paste(GETparameters, "&PageNumber=", 
            pagenumber, "&PageSize=", pagesize, "&SortProperty=", 
            sortproperty, "&SortDirection=", sortdirection, sep = "")
        auth <- authenticate(operation, secret)
        batch <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, log.requests = log.requests, 
            sandbox = sandbox, validation.test = validation.test)
        if(validation.test)
            return(invisible(batch))
        batch$total <- as.numeric(strsplit(strsplit(batch$xml, 
                            "<TotalNumResults>")[[1]][2], "</TotalNumResults>")[[1]][1])
        batch$batch.total <- length(xpathApply(xmlParse(batch$xml), "//QualificationTypeId"))
        if(return.qual.dataframe == TRUE) {
            if(batch$total > 0) 
                batch$quals <- QualificationTypesToDataFrame(xml = batch$xml)
        }
        return(batch)
    }
    request <- batch(operation, keyid, secret, GETparameters, 
        pagenumber, pagesize, sandbox = sandbox, validation.test = validation.test)
    if(validation.test)
        return(invisible(request))
    runningtotal <- request$batch.total
    pagenumber = 2
    if(return.all == TRUE) {
        sortproperty <- "Name"
        sortdirection <- "Ascending"
        pagesize <- "100"
        pagenumber <- "1"
        while(request$total > runningtotal) {
            nextbatch <- batch(operation, keyid, secret, GETparameters, 
                pagenumber, pagesize, sandbox = sandbox, validation.test = validation.test)
            if(validation.test)
                return(invisible(nextbatch))
            request$request.id <- c(request$request.id, nextbatch$request.id)
            request$valid <- c(request$valid, nextbatch$valid)
            request$xml.response <- c(request$xml, nextbatch$xml)
            if (return.qual.dataframe == TRUE) 
                request$quals <- rbind(request$quals, nextbatch$quals)
            request$pages.returned <- pagesize
            runningtotal <- runningtotal + request$batch.total
            pagenumber <- pagenumber + 1
        }
    }
    request$batch.total <- NULL
    if(request$valid[1] == TRUE & print == TRUE)
        message(dim(request$quals)[1], " of ", request$total, " QualificationTypes Retrieved")
    else if(request$valid == FALSE & print == TRUE)
        warning("Invalid Request")
    return(request$quals)
}
