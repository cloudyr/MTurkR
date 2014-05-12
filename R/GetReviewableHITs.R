GetReviewableHITs <-
reviewable <-
function (hit.type = NULL, status = NULL, response.group = "Minimal", 
    return.all = TRUE, pagenumber = "1", pagesize = "10", sortproperty = "Enumeration", 
    sortdirection = "Ascending", keypair = getOption('MTurkR.keypair'), print = getOption('MTurkR.print'), 
    log.requests = getOption('MTurkR.log'), sandbox = getOption('MTurkR.sandbox'),
    validation.test = getOption('MTurkR.test')) {
    if(is.null(keypair))
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "GetReviewableHITs"
    if(!sortproperty %in% c("Title", "Reward", "Expiration", 
        "CreationTime", "Enumeration")) 
        stop("'sortproperty' must be 'Title' | 'Reward' | 'Expiration' | 'CreationTime' | 'Enumeration'")
    if(!sortdirection %in% c("Ascending", "Descending")) 
        stop("'sortdirection' must be 'Ascending' | 'Descending'")
    if(as.numeric(pagesize) < 1 || as.numeric(pagesize) > 100) 
        stop("'pagesize' must be in range (1,100)")
    if(as.numeric(pagenumber) < 1) 
        stop("'pagenumber' must be > 1")
    if(!is.null(response.group) && !response.group == "Minimal") 
        warning("ResponseGroup must be 'Minimal'; Minimal used as default")
    if(!is.null(status) && !status %in% c("Reviewable", "Reviewing")) 
        stop("Status must be 'Reviewable' or 'Reviewing' or NULL")
    if(return.all == TRUE) {
        sortproperty <- "Enumeration"
        sortdirection <- "Ascending"
        pagesize <- "100"
        pagenumber <- "1"
    }
    if(is.factor(hit.type))
        hit.type <- as.character(hit.type)
    batch <- function(operation, pagenumber, pagesize) {
        GETparameters <- paste("&PageNumber=", pagenumber, "&PageSize=", 
            pagesize, "&SortProperty=", sortproperty, "&SortDirection=", 
            sortdirection, sep = "")
        if(!is.null(hit.type))
            GETparameters <- paste(GETparameters, "&HITTypeId=", hit.type, sep = "")
        if(!is.null(status) && (status %in% c("Reviewable", "Reviewing"))) 
            GETparameters <- paste(GETparameters, "&Status=", status, sep = "")
        batch <- request(keypair[1], operation, secret=keypair[2],
            GETparameters = GETparameters, log.requests = log.requests, 
            sandbox = sandbox, validation.test = validation.test)
        if(validation.test)
            return(invisible(batch))
        batch$HITs <- NA
        batch$total <- as.numeric(strsplit(strsplit(batch$xml, 
            "<TotalNumResults>")[[1]][2], "</TotalNumResults>")[[1]][1])
        batch$batch.total <- length(xpathApply(xmlParse(batch$xml), "//HITId"))
        if(batch$total > 0) {
            for(i in 1:batch$batch.total) {
                batch$HITs[i] <- strsplit(strsplit(batch$xml, 
                  "<HITId>")[[1]][i+1], "</HITId>")[[1]][1]
            }
        }
        return(batch)
    }
    request <- batch(operation, pagenumber, pagesize)
    if(validation.test)
        invisible(request)
    runningtotal <- request$batch.total
    pagenumber <- 2
    while(request$total > runningtotal) {
        nextbatch <- batch(operation, pagenumber, pagesize)
        if(validation.test)
            invisible(nextbatch)
        request$request.id <- c(request$request.id, nextbatch$request.id)
        request$valid <- c(request$valid, nextbatch$valid)
        request$xml.response <- c(request$xml, nextbatch$xml)
        request$HITs <- c(request$HITs, nextbatch$HITs)
        request$pages.returned <- pagenumber
        runningtotal <- runningtotal + nextbatch$batch.total
        pagenumber <- pagenumber + 1
    }
    request$batch.total <- NULL
    if(print == TRUE) 
        message(request$total, " HITs Retrieved")
    return(data.frame(HITId = request$HITs))
}
