GetQualificationScore <-
qualscore <-
function (qual, workers, keypair = credentials(), print = TRUE, 
    browser = FALSE, log.requests = TRUE, sandbox = FALSE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "GetQualificationScore"
    Qualifications <- NA
    for (i in 1:length(workers)) {
        GETparameters <- paste("&QualificationTypeId=", qual, 
            "&SubjectId=", workers[i], sep = "")
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
                x <- QualificationsToDataFrame(xml = request$xml)
                x$WorkerId <- workers[i]
                if (i == 1) 
                  Qualifications <- x
                else Qualifications <- rbind(Qualifications, 
                  x)
                if (print == TRUE) {
                  message("Qualification (", qual, ") Score for ", 
                    workers[i], ": ", Qualifications$Value[i])
                }
            }
            else if (request$valid == FALSE) {
                if (print == TRUE) 
                  warning("Invalid Request for worker ", workers[i])
            }
        }
    }
    if (browser == FALSE) {
        if (print == TRUE) 
            return(Qualifications)
        else invisible(Qualifications)
    }
}
