GetQualifications <-
getquals <-
function (qual, status = NULL, return.all = TRUE, pagenumber = 1, 
    pagesize = 100, keypair = credentials(), print = getOption('MTurkR.print'),
    log.requests = getOption('MTurkR.log'), sandbox = getOption('MTurkR.sandbox'),
    return.qual.dataframe = TRUE, validation.test = getOption('MTurkR.test')) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "GetQualificationsForQualificationType"
    if (return.all == TRUE) {
        pagesize <- "100"
        pagenumber <- "1"
    }
    if (as.numeric(pagesize) < 1 || as.numeric(pagesize) > 100) 
        stop("'pagesize' must be range (1,100)")
    if (as.numeric(pagenumber) < 1) 
        stop("'pagenumber' must be > 1")
    GETparameters <- paste("&QualificationTypeId=", qual, sep = "")
    if (!is.null(status)) {
        if (!status %in% c("Granted", "Revoked")) 
            warning("Status parameter ignored because it is not 'Granted' or 'Revoked'")
        else GETparameters <- paste(GETparameters, "&Status=", status, sep = "")
    }
    batch <- function(qual, pagenumber) {
        GETiteration <- paste(GETparameters, "&PageNumber=", 
            pagenumber, "&PageSize=", pagesize, sep = "")
        auth <- authenticate(operation, secret)
        batch <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETiteration, log.requests = log.requests, 
            sandbox = sandbox, validation.test = validation.test)
		if(validation.test)
			invisible(batch)
        batch$Qualifications <- NA
        batch$total <- as.numeric(strsplit(strsplit(batch$xml, 
            "<TotalNumResults>")[[1]][2], "</TotalNumResults>")[[1]][1])
        batch$batch.total <- length(xpathApply(xmlParse(batch$xml), 
            "//QualificationTypeId"))
        if (batch$batch.total > 0) {
            if (return.qual.dataframe == TRUE) 
                batch$Qualifications <- QualificationsToDataFrame(xml = batch$xml)
        }
        return(batch)
    }
    request <- batch(qual, pagenumber)
	if(validation.test)
		invisible(request)
    runningtotal <- request$batch.total
    pagenumber = 2
    while (request$total > runningtotal) {
        nextbatch <- batch(qual, pagenumber)
        request$request.id <- c(request$request.id, nextbatch$request.id)
        request$valid <- c(request$valid, nextbatch$valid)
        request$xml.response <- c(request$xml, nextbatch$xml)
        if (return.qual.dataframe == TRUE) 
            request$Qualifications <- rbind(request$Qualifications, 
                nextbatch$Qualifications)
        request$pages.returned <- pagenumber
        runningtotal <- runningtotal + nextbatch$batch.total
        pagenumber <- pagenumber + 1
    }
    request$batch.total <- NULL
    if (print == TRUE) {
        message(request$total, " Qualifications Retrieved")
        if (request$total > 0)
            return(request$Qualifications)
    }
    else {
        if (request$total > 0) 
            invisible(request$Qualifications)
    }
}
