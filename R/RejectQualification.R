RejectQualification <-
RejectQualifications <-
rejectrequest <-
function (qual.requests, reason = NULL, verbose = getOption('MTurkR.verbose', TRUE), ...){
    # temporary check for `print` argument (remove after v1.0)
    if('print' %in% names(list(...)) && is.null(verbose))
        verbose <- list(...)$print
    operation <- "RejectQualificationRequest"
    if(is.factor(qual.requests))
        qual.requests <- as.character(qual.requests)
    if(!is.null(reason)) {
        if(is.factor(reason))
            reason <- as.character(reason)
        if(!length(qual.requests) == length(reason)) {
            if(length(reason) == 1) 
                reason <- rep(reason[1], length(qual.requests))
            else
                stop("Number of QualificationRequests is not 1 or number of Reasons")
        }
    }
    QualificationRequests <- setNames(data.frame(matrix(ncol=3, nrow=length(qual.requests))),
                                c("QualificationRequestId", "Reason", "Valid"))
    for(i in 1:length(qual.requests)) {
        GETparameters <- paste("&QualificationRequestId=", qual.requests[i], 
            "&Reason=", curlEscape(reason[i]), sep = "")        
        request <- request(operation, GETparameters = GETparameters, ...)
        if(is.null(request$valid))
            return(request)
        if(request$valid == TRUE) {
            if(is.null(reason[i])) 
                reason[i] <- NA
            QualificationRequests[1, ] <- c(qual.requests[i], 
                reason[i], request$valid)
            if(verbose) 
                message(i, ": Qualification (", qual.requests[i],") Rejected")
        }
        else if(request$valid == FALSE) {
            if(verbose)
                warning(i, ": Invalid Request for QualificationRequestId ", qual.requests)
        }
        QualificationRequests$Valid <-
            factor(QualificationRequests$Valid, levels=c('TRUE','FALSE'))
        return(QualificationRequests)
    }
}
