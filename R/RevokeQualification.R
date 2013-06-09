RevokeQualification <-
RevokeQualifications <-
revokequal <-
function (qual, worker, reason = NULL, keypair = credentials(), 
    print = TRUE, browser = FALSE, log.requests = TRUE, sandbox = FALSE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "RevokeQualification"
    if (!is.null(reason) && length(reason) > 1) 
        stop("Reason must be NULL or length==1; other configurations not currently supported")
    batch <- function(qualbatch, workerbatch, reasonbatch) {
        GETparameters <- paste("&QualificationTypeId=", qualbatch, 
            "&SubjectId=", workerbatch, sep = "")
        if (!is.null(reason)) 
            GETparameters <- paste(GETparameters, "&SendNotification=", 
                curlEscape(reasonbatch), sep = "")
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
                if (print == TRUE) 
                  cat(i, ": Qualification (", qualbatch, ") for worker ", 
                    workerbatch, " Revoked\n", sep = "")
            }
            else if (request$valid == FALSE) {
                if (print == TRUE) 
                  cat(i, ": Invalid Request for worker ", workerbatch, 
                    "\n", sep = "")
            }
            invisible(request)
        }
    }
    Qualifications <- data.frame(matrix(ncol = 4))
    names(Qualifications) <- c("WorkerId", "QualificationTypeId", 
        "Reason", "Valid")
    if (length(qual) == 1 & length(worker) == 1) {
        x <- batch(qual[1], worker[1], reason)
        Qualifications[1, ] = c(worker[1], qual[1], reason, x$valid)
    }
    else if (length(qual) > 1 & length(worker) == 1) {
        for (i in 1:length(qual)) {
            x <- batch(qual[i], worker[1], reason)
            Qualifications[i, ] = c(worker[1], qual[i], reason, 
                x$valid)
        }
    }
    else if (length(qual) == 1 & length(worker) > 1) {
        for (i in 1:length(worker)) {
            x <- batch(qual[1], worker[i], reason)
            Qualifications[i, ] = c(worker[i], qual[1], reason, 
                x$valid)
        }
    }
    else if (length(qual) > 1 & length(worker) > 1) {
        for (i in 1:length(worker)) {
            for (j in 1:length(qual)) {
                x <- batch(qual[j], worker[i], reason)
                Qualifications[i, ] = c(worker[i], qual[j], reason, 
                  x$valid)
            }
        }
    }
    if (print == TRUE) 
        return(Qualifications)
    else invisible(Qualifications)
}
