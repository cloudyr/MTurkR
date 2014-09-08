RejectQualification <-
RejectQualifications <-
rejectrequest <-
function (qual.request, reason = NULL, verbose = getOption('MTurkR.verbose'), ...){
    # temporary check for `print` argument (remove after v1.0)
    if('print' %in% names(list(...)) && is.null(verbose))
        verbose <- list(...)$print
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
        request <- request(operation, GETparameters = GETparameters, ...)
        if(is.null(request$valid))
            return(request)
        if(request$valid == TRUE) {
            if(is.null(reason[i])) 
                reason[i] <- NA
            QualificationRequests[1, ] <- c(qual.request[i], 
                reason[i], request$valid)
            if(verbose) 
                message(i, ": Qualification (", qual.request[i],") Rejected")
        }
        else if(request$valid == FALSE) {
            if(verbose)
                warning(i, ": Invalid Request for QualificationRequestId ", qual.request)
        }
        QualificationRequests$Valid <-
            factor(QualificationRequests$Valid, levels=c('TRUE','FALSE'))
        return(QualificationRequests)
    }
}
