DisposeQualificationType <-
disposequal <-
function (qual, verbose = getOption('MTurkR.verbose', TRUE), ...) {
    operation <- "DisposeQualificationType"
    if (is.null(qual)) {
        stop("Must specify QualificationTypeId")
    } else {
        if (is.factor(qual)) {
            qual <- as.character(qual)
        }
        GETparameters <- paste("&QualificationTypeId=", qual, sep = "")
    }
    QualificationTypes <- emptydf(0, 2, c("QualificationTypeId", "Valid"))
    request <- request(operation, GETparameters = GETparameters, ...)
    if (is.null(request$valid)) {
        return(request)
    }
    if (request$valid == TRUE) {
        QualificationTypes[1, ] <- c(qual, request$valid)
        if (verbose) {
            message("QualificationType ", qual, " Disposed")
        }
        QualificationTypes$Valid <- factor(QualificationTypes$Valid, levels=c('TRUE','FALSE'))
        return(QualificationTypes)
    } else if (request$valid == FALSE) {
        if (verbose) {
            warning("Invalid Request\n")
        }
        return(emptydf(0, 2, c("QualificationTypeId", "Valid")))
    }
}
