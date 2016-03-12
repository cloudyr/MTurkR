GetQualificationScore <-
qualscore <-
function (qual, workers, verbose = getOption('MTurkR.verbose', TRUE), ...) {
    operation <- "GetQualificationScore"
    if (is.factor(qual)) {
        qual <- as.character(qual)
    }
    if (length(qual)==1) {
        qual <- rep(qual, length(workers))
    } else if(length(qual) != length(workers)) {
        stop("length(qual) != length(workers)")
    }
    if (is.factor(workers)) {
        workers <- as.character(workers)
    }
    Qualifications <- emptydf(length(workers), 6, c("QualificationTypeId", "WorkerId", "GrantTime", "Value", "Status", "Valid"))
    for (i in 1:length(workers)) {
        GETparameters <- paste("&QualificationTypeId=", qual[i], "&SubjectId=", workers[i], sep = "")
        request <- request(operation, GETparameters = GETparameters, ...)
        if (is.null(request$valid)) {
            return(request)
        }
        if (request$valid) {
            x <- as.data.frame.Qualifications(xml.parsed = xmlParse(request$xml))
            x$WorkerId <- workers[i]
            x$Valid <- TRUE
            Qualifications[i,] <- x
            if (verbose) {
                message("Qualification (", qual[i], ") Score for ", workers[i], ": ", Qualifications$Value[i])
            }
        } else {
            Qualifications[i,] <- 
                c(QualificationTypeId = qual[i], 
                  WorkerId = workers[i],
                  GrantTime = NA,
                  Value = NA,
                  Status = NA,
                  Valid = FALSE)
            if (verbose) {
                warning("Invalid Request for worker ", workers[i])
            }
        }
    }
    Qualifications$Valid <- factor(Qualifications$Valid, levels=c('TRUE','FALSE'))
    return(Qualifications)
}
