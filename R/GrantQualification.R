GrantQualification <-
GrantQualifications <-
grantqual <-
function (qual.requests, values, keypair = getOption('MTurkR.keypair'),
    print = getOption('MTurkR.print'), 
    log.requests = getOption('MTurkR.log'),
    sandbox = getOption('MTurkR.sandbox'), validation.test = getOption('MTurkR.test')) {
    if(is.null(keypair))
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "GrantQualification"
    if(is.factor(qual.requests))
        qual.requests <- as.character(qual.requests)
    if(is.factor(values))
        values <- as.character(values)
    if(!length(qual.requests) == length(values)) {
        if(length(values) == 1) 
            values <- rep(values[1], length(qual.requests))
        else
            stop("Number of QualificationRequests is not 1 or number of Values")
    }
    for(i in 1:length(values)) {
        if(!is.numeric(as.numeric(values))) 
            warning("Non-numeric Qualification Value requested for request ", 
                qual.requests[i], "\n", sep = "")
    }
    QualificationRequests <- setNames(data.frame(matrix(ncol=3, nrow=length(qual.requests))),
                                c("QualificationRequestId", "Value", "Valid"))
    for (i in 1:length(qual.requests)) {
        GETparameters <- paste("&QualificationRequestId=", qual.requests[i], 
            "&IntegerValue=", values[i], sep = "")
        request <- request(keypair[1], operation, secret=keypair[2],
            GETparameters = GETparameters, log.requests = log.requests, 
            sandbox = sandbox, validation.test = validation.test)
        if(validation.test)
            return(invisible(request))
        QualificationRequests[i, ] <- c(qual.requests[i], 
            values[i], request$valid)
        if(request$valid == TRUE) {
            if(print == TRUE) 
                message(i, ": Qualification (", qual.requests[i],") Granted")
        }
        else if (request$valid == FALSE) {
            if(print == TRUE) 
                warning(i, ": Invalid Request for QualificationRequest ", 
                qual.requests[i])
        }
    }
    QualificationRequests$Valid <-
        factor(QualificationRequests$Valid, levels=c('TRUE','FALSE'))
    return(QualificationRequests)
}
