RejectQualification <-
RejectQualifications <-
rejectrequest <-
function (qual.request, reason = NULL, keypair = getOption('MTurkR.keypair'), 
    print = getOption('MTurkR.print'), browser = getOption('MTurkR.browser'),
    log.requests = getOption('MTurkR.log'), sandbox = getOption('MTurkR.sandbox'),
    validation.test = getOption('MTurkR.test')) {
    if(!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "RejectQualificationRequest"
    if(is.factor(qual.request))
        qual.request <- as.character(qual.request)
    if(!is.null(reason)) {
        if(is.factor(reason))
            reason <- as.character(reason)
        if(!length(qual.request) == length(reason)) {
            if(length(reason) == 1) 
                reason <- rep(reason[1], length(qual.request))
            else
                stop("Number of QualificationRequests is not 1 or number of Reasons")
        }
    }
    QualificationRequests <- setNames(data.frame(matrix(ncol=3, nrow=length(qual.request))),
                                c("QualificationRequestId", "Reason", "Valid"))
    for(i in 1:length(qual.request)) {
        GETparameters <- paste("&QualificationRequestId=", qual.request[i], 
            "&Reason=", curlEscape(reason[i]), sep = "")
        auth <- authenticate(operation, secret)
        if(browser == TRUE) {
            request <- request(keyid, auth$operation, auth$signature, 
                auth$timestamp, GETparameters, browser = browser, 
                sandbox = sandbox, validation.test = validation.test)
            if(validation.test)
                return(invisible(request))
        }
        else {
            request <- request(keyid, auth$operation, auth$signature, 
                auth$timestamp, GETparameters, log.requests = log.requests, 
                sandbox = sandbox, validation.test = validation.test)
            if(validation.test)
                return(invisible(request))
            if(request$valid == TRUE) {
                if(is.null(reason[i])) 
                    reason[i] <- NA
                QualificationRequests[1, ] <- c(qual.request[i], 
                    reason[i], request$valid)
                if(print == TRUE) 
                    message(i, ": Qualification (", qual.request[i],") Rejected")
            }
            else if(request$valid == FALSE) {
                if(print == TRUE)
                    warning(i, ": Invalid Request for QualificationRequestId ", qual.request)
            }
            QualificationRequests$Valid <-
                factor(QualificationRequests$Valid, levels=c('TRUE','FALSE'))
            return(QualificationRequests)
        }
    }
}
