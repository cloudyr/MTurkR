GetQualificationScore <-
qualscore <-
function (qual, workers, verbose = getOption('MTurkR.verbose', TRUE), ...) {
    # temporary check for `print` argument (remove after v1.0)
    if('print' %in% names(list(...)) && is.null(verbose))
        verbose <- list(...)$print
    operation <- "GetQualificationScore"
    if(is.factor(qual))
        qual <- as.character(qual)
    if(length(qual)==1)
        qual <- rep(qual, length(workers))
    else if(length(qual) != length(workers))
        stop("length(qual) != length(workers)")
    if(is.factor(workers))
        workers <- as.character(workers)
    Qualifications <- 
        setNames(data.frame(matrix(nrow = length(workers), ncol = 5)),
                 c("QualificationTypeId", "WorkerId", "GrantTime", "Value", "Status"))
    for(i in 1:length(workers)) {
        GETparameters <- paste("&QualificationTypeId=", qual[i], 
            "&SubjectId=", workers[i], sep = "")
        request <- request(operation, GETparameters = GETparameters, ...)
        if(is.null(request$valid))
            return(request)
        if(request$valid) {
            x <- as.data.frame.Qualifications(xml.parsed = xmlParse(request$xml))
            x$WorkerId <- workers[i]
            Qualifications[i,] <- x
            if(verbose) {
                message("Qualification (", qual[i], ") Score for ", 
                        workers[i], ": ", Qualifications$Value[i])
            }
        } else {
            Qualifications[i,] <- 
                c(QualificationTypeId = qual[i], 
                  WorkerId = workers[i],
                  GrantTime = NA,
                  Value = NA,
                  Status = NA)
            if(verbose)
                warning("Invalid Request for worker ", workers[i])
        }
    }
    return(Qualifications)
}
