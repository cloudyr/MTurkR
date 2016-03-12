RejectQualification <-
RejectQualifications <-
rejectrequest <-
function (qual.requests, reason = NULL, verbose = getOption('MTurkR.verbose', TRUE), ...){
    operation <- "RejectQualificationRequest"
    if (is.factor(qual.requests)) {
        qual.requests <- as.character(qual.requests)
    }
    if (!is.null(reason)) {
        if (is.factor(reason)) {
            reason <- as.character(reason)
        }
        if (!length(qual.requests) == length(reason)) {
            if (length(reason) == 1) {
                reason <- rep(reason[1], length(qual.requests))
            } else {
                stop("Number of QualificationRequests is not 1 or number of Reasons")
            }
        }
    }
    QualificationRequests <- emptydf(length(qual.requests), 3, c("QualificationRequestId", "Reason", "Valid"))
    for (i in 1:length(qual.requests)) {
        GETparameters <- paste("&QualificationRequestId=", qual.requests[i], 
                               "&Reason=", curl_escape(reason[i]), sep = "")        
        request <- request(operation, GETparameters = GETparameters, ...)
        if (is.null(request$valid)) {
            return(request)
        }
        if (request$valid == TRUE) {
            if (is.null(reason[i])) {
                reason[i] <- NA_character_
            }
            QualificationRequests[1, ] <- c(qual.requests[i], reason[i], request$valid)
            if (verbose) {
                message(i, ": Qualification (", qual.requests[i],") Rejected")
            }
        } else if (request$valid == FALSE) {
            if (verbose) {
                warning(i, ": Invalid Request for QualificationRequestId ", qual.requests)
            }
        }
        QualificationRequests$Valid <- factor(QualificationRequests$Valid, levels=c('TRUE','FALSE'))
        return(QualificationRequests)
    }
}
