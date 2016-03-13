GetHITsForQualificationType <-
gethitsbyqual <-
function (qual, response.group = NULL, return.all = TRUE, pagenumber = 1, 
    pagesize = 100, verbose = getOption('MTurkR.verbose', TRUE), ...) {
    operation <- "GetHITsForQualificationType"
    if (return.all == TRUE) {
        pagesize <- "100"
        pagenumber <- "1"
    }
    if (as.numeric(pagesize) < 1 || as.numeric(pagesize) > 100) {
        stop("'pagesize' must be in range (1,100)")
    }
    if (as.numeric(pagenumber) < 1) {
        stop("'pagenumber' must be > 1")
    }
    GETparameters <- paste("&QualificationTypeId=", .AliasToQualificationType(qual), 
                           "&PageSize=", 
                           pagesize, sep = "")
    if (!is.null(response.group)) {
        if (any(!response.group %in% c("Minimal", "Request"))) {
            stop("ResponseGroup must be in c(Minimal,Request)")
        }
        if (length(response.group) == 1) {
            GETparameters <- paste(GETparameters, "&ResponseGroup=", 
                                   response.group, sep = "")
        } else {
            for (i in 1:length(response.group)) {
                GETparameters <- paste(GETparameters, "&ResponseGroup", i - 1,
                                       "=", response.group[i], sep = "")
            }
        }
    }
    batch <- function(pagenumber) {
        GETiteration <- paste(GETparameters, "&PageNumber=", 
                              pagenumber, sep = "")
        batch <- request(operation, GETparameters = GETiteration, 
                         verbose = verbose, ...)
        if (is.null(batch$valid)) {
            return(batch)
        }
        batch$total <- as.numeric(strsplit(strsplit(batch$xml, 
            "<TotalNumResults>")[[1]][2], "</TotalNumResults>")[[1]][1])
        batch$batch.total <- length(xpathApply(xmlParse(batch$xml), "//HIT"))
        if (batch$total > 0) {
            hitlist <- as.data.frame.HITs(xml.parsed = xmlParse(batch$xml))
            batch$HITs <- hitlist$HITs
            batch$QualificationRequirements <- hitlist$QualificationRequirements
        } else {
            batch$HITs <- emptydf(0, 19, c("HITId", "HITTypeId", "CreationTime", 
                                     "Title", "Description", "Keywords", "HITStatus", 
                                     "MaxAssignments", "Amount", 
                                     "AutoApprovalDelayInSeconds", "Expiration", 
                                     "AssignmentDurationInSeconds", "NumberOfSimilarHITs", 
                                     "HITReviewStatus", "RequesterAnnotation", 
                                     "NumberOfAssignmentsPending",
                                     "NumberOfAssignmentsAvailable", 
                                     "NumberOfAssignmentsCompleted", "Question"))
            batch$QualificationRequirements <- list()
        }
        return(batch)
    }
    request <- batch(pagenumber)
    if (is.null(request$valid)) {
        return(request)
    }
    runningtotal <- request$batch.total
    pagenumber <- 2
    while (request$total > runningtotal) {
        nextbatch <- batch(pagenumber)
        request$request.id <- c(request$request.id, nextbatch$request.id)
        request$valid <- c(request$valid, nextbatch$valid)
        request$xml.response <- c(request$xml, nextbatch$xml)
        if (request$valid) {
            request$HITs <- rbind(request$HITs, nextbatch$HITs)
            request$QualificationRequirements <- rbind(request$QualificationRequirements, 
                nextbatch$QualificationRequirements)
        }
        request$pages.returned <- pagesize
        runningtotal <- runningtotal + request$batch.total
        pagenumber <- pagenumber + 1
    }
    request$batch.total <- NULL
    if (verbose) {
        message(request$total, " HITs Retrieved")
    }
    return(list(HITs = setRownames(request$HITs),
                QualificationRequirements = request$QualificationRequirements))
}
