SearchQualificationTypes <-
searchquals <-
function (query = NULL, only.mine = TRUE, only.requestable = FALSE, 
    return.all = FALSE, pagenumber = "1", pagesize = "10", sortproperty = "Name", 
    sortdirection = "Ascending", return.qual.dataframe = TRUE, 
    verbose = getOption('MTurkR.verbose'), ...) {
    # temporary check for `print` argument (remove after v1.0)
    if('print' %in% names(list(...)) && is.null(verbose))
        verbose <- list(...)$print
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
    batch <- function(GETparameters, pagenumber, pagesize) {
        GETparameters <- paste(GETparameters, "&PageNumber=", 
            pagenumber, "&PageSize=", pagesize, "&SortProperty=", 
            sortproperty, "&SortDirection=", sortdirection, sep = "")
        batch <- request(operation, GETparameters = GETparameters, ...)
        if(is.null(batch$valid))
            return(batch)
        batch$total <- as.numeric(strsplit(strsplit(batch$xml, 
                            "<TotalNumResults>")[[1]][2], "</TotalNumResults>")[[1]][1])
        batch$batch.total <- length(xpathApply(xmlParse(batch$xml), "//QualificationTypeId"))
        if(return.qual.dataframe == TRUE) {
            if(batch$total > 0) 
                batch$quals <- as.data.frame.QualificationTypes(xml.parsed = xmlParse(batch$xml))
        }
        return(batch)
    }
    request <- batch(GETparameters, pagenumber, pagesize)
    if(is.null(request$valid))
        return(request)
    runningtotal <- request$batch.total
    pagenumber <- 2
    if(return.all == TRUE) {
        sortproperty <- "Name"
        sortdirection <- "Ascending"
        pagesize <- "100"
        pagenumber <- "1"
        while(request$total > runningtotal) {
            nextbatch <- batch(GETparameters, pagenumber, pagesize)
            if(is.null(nextbatch$valid))
                return(nextbatch)
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
