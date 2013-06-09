RejectQualification <-
RejectQualifications <-
rejectrequest <-
function (qual.request, reason = NULL, keypair = credentials(), 
    print = TRUE, browser = FALSE, log.requests = TRUE, sandbox = FALSE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "RejectQualificationRequest"
    if (!is.null(reason)) {
        if (!length(qual.request) == length(reason)) {
            if (length(reason) == 1) 
                reason <- rep(reason[1], length(qual.request))
            else stop("Number of QualificationRequests is not 1 or number of Reasons")
        }
    }
    QualificationRequests <- data.frame(matrix(ncol = 3))
    names(QualificationRequests) <- c("QualificationRequestId", 
        "Reason", "Valid")
    for (i in 1:length(qual.request)) {
        GETparameters <- paste("&QualificationRequestId=", qual.request[i], 
            "&Reason=", curlEscape(reason[i]), sep = "")
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
                if (is.null(reason[i])) 
                  reason[i] <- NA
                QualificationRequests[1, ] <- c(qual.request[i], 
                  reason[i], request$valid)
                if (print == TRUE) 
                  cat(i, ": Qualification (", qual.request[i], 
                    ") Rejected\n", sep = "")
                invisible(QualificationRequests)
            }
            else if (request$valid == FALSE) {
                if (print == TRUE) {
                  cat(i, ": Invalid Request for QualificationRequestId ", 
                    qual.request, "\n", sep = "")
                  return(QualificationRequests)
                }
            }
        }
    }
}
