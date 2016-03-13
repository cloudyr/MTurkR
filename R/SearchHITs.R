SearchHITs <-
searchhits <-
function (response.group = NULL, return.all = TRUE, pagenumber = "1", 
    pagesize = "10", sortproperty = "Enumeration", sortdirection = "Ascending", 
    return.hit.dataframe = TRUE, return.qual.dataframe = TRUE,
    verbose = getOption('MTurkR.verbose', TRUE), ...) {
    if ('sandbox' %in% names(list(...))) {
        sandbox <- list(...)$sandbox
    } else {
        sandbox <- getOption('MTurkR.verbose', TRUE)
    }
    operation <- "SearchHITs"
    if (!sortproperty %in% c("Title", "Reward", "Expiration", "CreationTime", "Enumeration")) {
        stop("'sortproperty' must be 'Title' | 'Reward' | 'Expiration' | 'CreationTime' | 'Enumeration'")
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
    if (return.all == TRUE) {
        sortproperty <- "Enumeration"
        sortdirection <- "Ascending"
        pagesize <- "100"
        pagenumber <- "1"
    }
    GETparameters <- ""
    if (!is.null(response.group)) {
        if (any(!response.group %in% c("Request", "Minimal", "HITDetail", "HITQuestion", "HITAssignmentSummary"))) {
            stop("ResponseGroup must be in c(Request,Minimal,HITDetail,HITQuestion,HITAssignmentSummary)")
        }
        if (length(response.group) == 1) {
            GETparameters <- paste(GETparameters, "&ResponseGroup=", response.group, sep = "")
        } else {
            for (i in 1:length(response.group)) {
                GETparameters <- paste(GETparameters, "&ResponseGroup", 
                i - 1, "=", response.group[i], sep = "")
            }
        }
    }
    batch <- function(pagenumber) {
        GETiteration <- paste(GETparameters, "&PageNumber=", 
                        pagenumber, "&PageSize=", pagesize, "&SortProperty=", 
                        sortproperty, "&SortDirection=", sortdirection, sep = "")
        batch <- request(operation, GETparameters = GETiteration, ...)
        if (is.null(batch$valid)) {
            return(batch)
        }
        batch$total <- as.numeric(strsplit(strsplit(batch$xml, "<TotalNumResults>")[[1]][2], "</TotalNumResults>")[[1]][1])
        batch$batch.total <- length(xpathApply(xmlParse(batch$xml), "//HIT"))
        if (return.hit.dataframe == TRUE) {
            if (!batch$valid) {
                stop("SearchHITs() request failed!")
            }
            if (batch$total > 0) {
                hitlist <- as.data.frame.HITs(xml.parsed = xmlParse(batch$xml),
                                              return.qual.list = return.qual.dataframe,
                                              sandbox = sandbox)
                batch$HITs <- hitlist$HITs
                if (return.qual.dataframe == TRUE) {
                    batch$QualificationRequirements <- hitlist$QualificationRequirements
                }
            }
        }
        return(batch)
    }
    request <- batch(pagenumber)
    if (is.null(request$valid)) {
        return(request)
    }
    runningtotal <- request$batch.total
    if (return.all) {
        pagenumber <- 2
        while (request$total > runningtotal) {
            nextbatch <- batch(pagenumber)
            if (is.null(nextbatch$valid)) {
                return(nextbatch)
            }
            request$request.id <- c(request$request.id, nextbatch$request.id)
            request$valid <- c(request$valid, nextbatch$valid)
            request$xml.response <- c(request$xml, nextbatch$xml)
            if (return.hit.dataframe == TRUE) {
                request$HITs <- rbind(request$HITs, nextbatch$HITs)
            }
            if (return.qual.dataframe == TRUE) {
                request$QualificationRequirements <- c(request$QualificationRequirements, 
                                                    nextbatch$QualificationRequirements)
            }
            request$pages.returned <- pagesize
            runningtotal <- runningtotal + request$batch.total
            pagenumber <- pagenumber + 1
        }
        request$batch.total <- NULL
    }
    if (!is.null(response.group)) {
        request$ResponseGroup <- c("Minimal", "HITDetail", "HITQuestion")
        if (response.group == "Minimal") {
            request$HITs <- request$HITs[, c("HITId", "HITTypeId")]
            return.qual.dataframe <- TRUE
        }
    } else {
        request$ResponseGroup <- response.group
    }
    if (return.qual.dataframe==TRUE) {
        if (!is.null(request$QualificationRequirements) && length(request$QualificationRequirements)>0) {
            tmpdf <- do.call(rbind,request$QualificationRequirements)
            allQuals <- unique(tmpdf$QualificationTypeId)
            allQuals <- allQuals[!allQuals %in% ListQualificationTypes()$QualificationTypeId]
            if (length(allQuals)>0) {
                allNames <- character(length=length(allQuals))
                for (i in 1:length(allQuals)) {
                    allNames[i] <- GetQualificationType(allQuals[i], verbose = verbose, ...)$Name
                }
                for (i in 1:nrow(tmpdf)) {
                    if (is.na(tmpdf$Name[i])) {
                        tmpdf$Name[i] <- allNames[allQuals==tmpdf$QualificationTypeId[i]]
                    }
                }
                request$QualificationRequirements <- split(tmpdf,tmpdf$HITId)
            }
        }
        if(return.hit.dataframe==TRUE){
            return.list <- list(HITs = setRownames(request$HITs),
                                QualificationRequirements = request$QualificationRequirements)
        } else {
            return.list <- list(QualificationRequirements = request$QualificationRequirements)
        }
    } else {
        if (return.hit.dataframe == TRUE) {
            return.list <- list(HITs = setRownames(request$HITs))
        } else {
            return.list <- list()
        }
    }
    if (verbose) {
        message(runningtotal, " HITs Retrieved")
    }
    return(return.list)
}
