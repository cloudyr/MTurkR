DisposeQualificationType <-
disposequal <-
function (qual, verbose = getOption('MTurkR.verbose'), ...) {
    # temporary check for `print` argument (remove after v1.0)
    if('print' %in% names(list(...)) && is.null(verbose))
        verbose <- list(...)$print
    operation <- "DisposeQualificationType"
    if(is.null(qual)) 
        stop("Must specify QualificationTypeId")
    else {
        if(is.factor(qual))
            qual <- as.character(qual)
        GETparameters <- paste("&QualificationTypeId=", qual, sep = "")
    }
    QualificationTypes <- setNames(data.frame(matrix(ncol = 2)),
                            c("QualificationTypeId", "Valid"))
    request <- request(operation, GETparameters = GETparameters, ...)
    if(is.null(request$valid))
        return(request)
    if(request$valid == TRUE) {
        QualificationTypes[1, ] <- c(qual, request$valid)
        if(verbose)
            message("QualificationType ", qual, " Disposed")
        QualificationTypes$Valid <-
            factor(QualificationTypes$Valid, levels=c('TRUE','FALSE'))
        return(QualificationTypes)
    }
    else if(request$valid == FALSE) {
        if(verbose) 
            warning("Invalid Request\n")
        return(NULL)
    }
}
