DisposeQualificationType <-
disposequal <-
function (qual, keypair = credentials(), print = TRUE, browser = FALSE, 
    log.requests = TRUE, sandbox = FALSE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "DisposeQualificationType"
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
        QualificationTypes <- data.frame(matrix(ncol = 2))
        names(QualificationTypes) <- c("QualificationTypeId", 
            "Valid")
        request <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, log.requests = log.requests, 
            sandbox = sandbox)
        if (request$valid == TRUE) {
            QualificationTypes[1, ] <- c(qual, request$valid)
            if (print == TRUE) {
                cat("QualificationType ", qual, " Disposed\n", 
                  sep = "")
                return(QualificationTypes)
            }
            else invisible(QualificationTypes)
        }
        else if (request$valid == FALSE) {
            if (print == TRUE) 
                cat("Invalid Request\n")
            invisible(NULL)
        }
    }
}
