GetQualificationType <-
qualtype <-
function (qual, keypair = credentials(), print = TRUE, browser = FALSE, 
    log.requests = TRUE, sandbox = FALSE, return.qual.dataframe = TRUE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "GetQualificationType"
    if (is.null(qual)) 
        stop("Must specify QualificationTypeId")
    else GETparameters <- paste("&QualificationTypeId=", qual, 
        sep = "")
    auth <- authenticate(operation, secret)
    if (browser == TRUE) {
        request <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, browser = browser, 
            sandbox = sandbox)
    }
    else {
        request <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, log.requests = log.requests, 
            sandbox = sandbox)
        if (request$valid == TRUE) {
            Qualifications <- QualificationTypesToDataFrame(xml = request$xml)
            if (print == TRUE) 
                cat("QualificationType Retrieved: ", qual, "\n", 
                  sep = "")
        }
        else if (request$valid == FALSE) {
            if (print == TRUE) 
                cat("Invalid Request\n")
        }
    }
    if (print == TRUE) 
        return(Qualifications)
    else invisible(Qualifications)
}
