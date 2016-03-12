GrantQualification <-
GrantQualifications <-
grantqual <-
function(qual.requests, values, verbose = getOption('MTurkR.verbose', TRUE), ...) {
    operation <- "GrantQualification"
    if (is.factor(qual.requests)) {
        qual.requests <- as.character(qual.requests)
    }
    if (is.factor(values)) {
        values <- as.character(values)
    }
    if (!length(qual.requests) == length(values)) {
        if (length(values) == 1) {
            values <- rep(values[1], length(qual.requests))
        } else {
            stop("Number of QualificationRequests is not 1 or number of Values")
        }
    }
    for (i in 1:length(values)) {
        if (!is.numeric(as.numeric(values))) {
            warning("Non-numeric Qualification Value requested for request ", 
                    qual.requests[i], "\n", sep = "")
        }
    }
    QualificationRequests <- emptydf(length(qual.requests), 3, c("QualificationRequestId", "Value", "Valid"))
    for (i in 1:length(qual.requests)) {
        GETparameters <- paste("&QualificationRequestId=", qual.requests[i], 
                               "&IntegerValue=", values[i], sep = "")
        request <- request(operation, GETparameters = GETparameters, ...)
        if (is.null(request$valid)) {
            return(request)
        }
        QualificationRequests[i, ] <- c(qual.requests[i], values[i], request$valid)
        if (request$valid == TRUE) {
            if (verbose) {
                message(i, ": Qualification (", qual.requests[i],") Granted")
            }
        } else if (request$valid == FALSE) {
            if (verbose) {
                warning(i, ": Invalid Request for QualificationRequest ", qual.requests[i])
            }
        }
    }
    QualificationRequests$Valid <- factor(QualificationRequests$Valid, levels=c('TRUE','FALSE'))
    return(QualificationRequests)
}
