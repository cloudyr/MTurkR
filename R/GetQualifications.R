GetQualifications <-
getquals <-
function (qual, status = NULL, return.all = TRUE, pagenumber = 1, 
    pagesize = 100, verbose = getOption('MTurkR.verbose', TRUE), ...) {
    operation <- "GetQualificationsForQualificationType"
    if (return.all == TRUE) {
        pagesize <- "100"
        pagenumber <- "1"
    }
    if (as.numeric(pagesize) < 1 || as.numeric(pagesize) > 100) {
        stop("'pagesize' must be range (1,100)")
    }
    if (as.numeric(pagenumber) < 1) {
        stop("'pagenumber' must be > 1")
    }
    if (is.factor(qual)) {
        qual <- as.character(qual)
    }
    GETparameters <- paste("&QualificationTypeId=", qual, sep = "")
    if(!is.null(status)) {
        if (!status %in% c("Granted", "Revoked")) {
            warning("Status parameter ignored because it is not 'Granted' or 'Revoked'")
        } else {
            GETparameters <- paste(GETparameters, "&Status=", status, sep = "")
        }
    }
    batch <- function(qual, pagenumber) {
        GETiteration <- paste(GETparameters, "&PageNumber=", 
                              pagenumber, "&PageSize=", pagesize, sep = "")
        batch <- request(operation, GETparameters = GETiteration, ...)
        if (is.null(batch$valid)) {
            return(batch)
        }
        batch$Qualifications <- NA_character_
        batch$total <- as.numeric(strsplit(strsplit(batch$xml, 
            "<TotalNumResults>")[[1]][2], "</TotalNumResults>")[[1]][1])
        batch$batch.total <- length(xpathApply(xmlParse(batch$xml), "//QualificationTypeId"))
        if (batch$batch.total > 0) {
            batch$Qualifications <- as.data.frame.Qualifications(xml.parsed = xmlParse(batch$xml))
        }
        return(batch)
    }
    request <- batch(qual, pagenumber)
    if (is.null(request$valid)) {
        return(request)
    }
    if (return.all) {
        runningtotal <- request$batch.total
        pagenumber <- 2
        while (request$total > runningtotal) {
            nextbatch <- batch(qual, pagenumber)
            request$request.id <- c(request$request.id, nextbatch$request.id)
            request$valid <- c(request$valid, nextbatch$valid)
            request$xml.response <- c(request$xml, nextbatch$xml)
            request$Qualifications <- rbind(request$Qualifications, nextbatch$Qualifications)
            request$pages.returned <- pagenumber
            runningtotal <- runningtotal + nextbatch$batch.total
            pagenumber <- pagenumber + 1
        }
        request$batch.total <- NULL
    }
    if (verbose) {
        message(request$total, " Qualifications Retrieved")
    }
    if (request$total > 0) {
        return(setRownames(request$Qualifications))
    } else {
        return(emptydf(0, 5, c("QualificationTypeId", "WorkerId", "GrantTime", "Value", "Status")))
    }
}
