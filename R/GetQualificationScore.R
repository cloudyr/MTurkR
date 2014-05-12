GetQualificationScore <-
qualscore <-
function (qual, workers, verbose = getOption('MTurkR.verbose'), ...) {
    # temporary check for `print` argument (remove after v1.0)
    if('print' %in% names(list(...)) && is.null(verbose))
        verbose <- list(...)$print
    operation <- "GetQualificationScore"
    if(is.factor(qual))
        qual <- as.character(qual)
    if(is.factor(workers))
        workers <- as.character(workers)
    Qualifications <- NA
    for(i in 1:length(workers)) {
        GETparameters <- paste("&QualificationTypeId=", qual, 
            "&SubjectId=", workers[i], sep = "")
        request <- request(operation, GETparameters = GETparameters, ...)
        if(is.null(request$valid))
            return(request)
        if(request$valid == TRUE) {
            x <- QualificationsToDataFrame(xml = request$xml)
            x$WorkerId <- workers[i]
            if(i == 1) 
                Qualifications <- x
            else Qualifications <- rbind(Qualifications, x)
            if(verbose) {
                message("Qualification (", qual, ") Score for ", 
                        workers[i], ": ", Qualifications$Value[i])
            }
        }
        else if(request$valid == FALSE & print == TRUE)
              warning("Invalid Request for worker ", workers[i])
    }
    return(Qualifications)
}
