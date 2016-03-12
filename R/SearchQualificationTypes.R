SearchQualificationTypes <-
searchquals <-
function (query = NULL, only.mine = TRUE, only.requestable = FALSE, 
    return.all = FALSE, pagenumber = "1", pagesize = "10", sortproperty = "Name", 
    sortdirection = "Ascending", return.qual.dataframe = TRUE, 
    verbose = getOption('MTurkR.verbose', TRUE), ...) {
    operation <- "SearchQualificationTypes"
    if (!sortproperty %in% c("Name")) {
        stop("'sortproperty' must be 'Name'")
    }
    if (!sortdirection %in% c("Ascending", "Descending")) {
        stop("'sortdirection' must be 'Ascending' | 'Descending'")
    }
    if (as.numeric(pagesize) < 1 || as.numeric(pagesize) > 100) {
        stop("'pagesize' must be in range (1,100)")
    }
    if (as.numeric(pagenumber) < 1) {
        stop("'pagenumber' must be > 1")
    }
    GETparameters <- ""
    if (!is.null(query)) {
        GETparameters <- paste(GETparameters, "&Query=", curl_escape(query), sep = "")
    }
    if (only.mine) {
        GETparameters <- paste(GETparameters, "&MustBeOwnedByCaller=", "true", sep = "")
    } else {
        GETparameters <- paste(GETparameters, "&MustBeOwnedByCaller=", "false", sep = "")
    }
    if (only.requestable) {
        GETparameters <- paste(GETparameters, "&MustBeRequestable=", "true", sep = "")
    } else {
        GETparameters <- paste(GETparameters, "&MustBeRequestable=", "false", sep = "")
    }
    batch <- function(GETparameters, pagenumber, pagesize) {
        GETparameters <- paste(GETparameters, "&PageNumber=", 
                               pagenumber, "&PageSize=", 
                               pagesize, "&SortProperty=", 
                               sortproperty, "&SortDirection=", 
                               sortdirection, sep = "")
        batch <- request(operation, GETparameters = GETparameters, ...)
        if (is.null(batch$valid)) {
            return(batch)
        }
        batch$total <- as.numeric(strsplit(strsplit(batch$xml, 
                            "<TotalNumResults>")[[1]][2], "</TotalNumResults>")[[1]][1])
        batch$batch.total <- length(xpathApply(xmlParse(batch$xml), "//QualificationTypeId"))
        if (return.qual.dataframe) {
            if (batch$total > 0) {
                batch$quals <- as.data.frame.QualificationTypes(xml.parsed = xmlParse(batch$xml))
            }
        }
        return(batch)
    }
    if (return.all) {
        sortproperty <- "Name"
        sortdirection <- "Ascending"
        pagesize <- "100"
        pagenumber <- "1"
        request <- batch(GETparameters, pagenumber, pagesize)
        if (is.null(request$valid)) {
            return(request)
        }
        runningtotal <- request$batch.total
        while (request$total > runningtotal) {
            nextbatch <- batch(GETparameters, pagenumber, pagesize)
            if (is.null(nextbatch$valid)) {
                return(nextbatch)
            }
            request$request.id <- c(request$request.id, nextbatch$request.id)
            request$valid <- c(request$valid, nextbatch$valid)
            request$xml.response <- c(request$xml, nextbatch$xml)
            if (return.qual.dataframe) {
                request$quals <- rbind(request$quals, nextbatch$quals)
            }
            request$pages.returned <- pagesize
            runningtotal <- runningtotal + request$batch.total
            pagenumber <- as.numeric(pagenumber) + 1
        }
    } else {
        request <- batch(GETparameters, pagenumber, pagesize)
        if (is.null(request$valid)) {
            return(request)
        }
    }
    request$batch.total <- NULL
    if (request$valid[1] & verbose) {
        message(dim(request$quals)[1], " of ", request$total, " QualificationTypes Retrieved")
    } else if (!request$valid[1] & verbose) {
        warning("Invalid Request")
    }
    return(setRownames(request$quals))
}
