GetQualificationRequests <-
qualrequests <-
function (qual = NULL, return.all = TRUE, pagenumber = "1", pagesize = "10", 
    sortproperty = "SubmitTime", sortdirection = "Ascending", 
    keypair = credentials(), print = getOption('MTurkR.print'),
    log.requests = getOption('MTurkR.log'), sandbox = getOption('MTurkR.sandbox'),
    return.qual.dataframe = TRUE, validation.test = getOption('MTurkR.test')) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "GetQualificationRequests"
    if (!sortproperty %in% c("SubmitTime", "QualificationTypeId")) 
        stop("'sortproperty' must be 'SubmitTime' | 'QualificationTypeId'")
    if (!sortdirection %in% c("Ascending", "Descending")) 
        stop("'sortdirection' must be 'Ascending' | 'Descending'")
    if (as.numeric(pagesize) < 1 || as.numeric(pagesize) > 100) 
        stop("'pagesize' must be in range (1,100)")
    if (as.numeric(pagenumber) < 1) 
        stop("'pagenumber' must be > 1")
    if (return.all == TRUE) {
        sortproperty <- "SubmitTime"
        sortdirection <- "Ascending"
        pagesize <- "100"
        pagenumber <- "1"
    }
    GETparameters <- ""
    if (!is.null(qual)) 
        GETparameters = paste("&QualificationTypeId=", qual, 
            sep = "")
    else qual <- ""
    batch <- function(qual, pagenumber, pagesize, sortproperty, 
        sortdirection, sandbox = sandbox) {
        GETiteration <- paste(GETparameters, "&PageNumber=", 
            pagenumber, "&PageSize=", pagesize, "&SortProperty=", 
            sortproperty, "&SortDirection=", sortdirection, sep = "")
        auth <- authenticate(operation, secret)
        batch <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETiteration, log.requests = log.requests, 
            sandbox = sandbox, validation.test = validation.test)
		if(validation.test)
			invisible(batch)
        batch$QualificationRequests <- NA
        batch$total <- as.numeric(strsplit(strsplit(batch$xml, 
            "<TotalNumResults>")[[1]][2], "</TotalNumResults>")[[1]][1])
        batch$batch.total <- length(xpathApply(xmlParse(batch$xml), 
            "//QualificationRequestId"))
        if (batch$batch.total > 0) {
            if (return.qual.dataframe == TRUE) 
                batch$QualificationRequests <- QualificationRequestsToDataFrame(xml = batch$xml)
        }
        return(batch)
    }
    request <- batch(qual, pagenumber, pagesize, sortproperty, 
        sortdirection, sandbox = sandbox)
	if(validation.test)
		invisible(request)
    runningtotal <- request$batch.total
    pagenumber = 2
    while (request$total > runningtotal) {
        nextbatch <- batch(qual, pagenumber, pagesize, sortproperty, 
            sortdirection, sandbox = sandbox)
        request$request.id <- c(request$request.id, nextbatch$request.id)
        request$valid <- c(request$valid, nextbatch$valid)
        request$xml.response <- c(request$xml, nextbatch$xml)
        if (return.qual.dataframe == TRUE) 
            request$QualificationRequests <- rbind(request$QualificationRequests, 
                nextbatch$QualificationRequests)
        request$pages.returned <- pagenumber
        runningtotal <- runningtotal + nextbatch$batch.total
        pagenumber <- pagenumber + 1
    }
    request$batch.total <- NULL
    if (print == TRUE) {
        message(request$total, " Requests Retrieved")
        if (request$total > 0) 
            return(request$QualificationRequests)
    }
    else {
        if (request$total > 0) 
            invisible(request$QualificationRequests)
    }
}
