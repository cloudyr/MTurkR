GetQualificationScore <-
qualscore <-
function (qual, workers, keypair = getOption('MTurkR.keypair'),
    print = getOption('MTurkR.print'), 
    log.requests = getOption('MTurkR.log'),
    sandbox = getOption('MTurkR.sandbox'), validation.test = getOption('MTurkR.test')) {
    if(is.null(keypair))
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "GetQualificationScore"
    if(is.factor(qual))
        qual <- as.character(qual)
    if(is.factor(workers))
        workers <- as.character(workers)
    Qualifications <- NA
    for(i in 1:length(workers)) {
        GETparameters <- paste("&QualificationTypeId=", qual, 
            "&SubjectId=", workers[i], sep = "")
        
        request <- request(keypair[1], operation, secret=keypair[2],
        GETparameters = GETparameters, log.requests = log.requests, 
            sandbox = sandbox, validation.test = validation.test)
        if(validation.test)
            return(invisible(request))
        if(request$valid == TRUE) {
            x <- QualificationsToDataFrame(xml = request$xml)
            x$WorkerId <- workers[i]
            if(i == 1) 
                Qualifications <- x
            else Qualifications <- rbind(Qualifications, x)
            if(print == TRUE) {
                message("Qualification (", qual, ") Score for ", 
                        workers[i], ": ", Qualifications$Value[i])
            }
        }
        else if(request$valid == FALSE & print == TRUE)
              warning("Invalid Request for worker ", workers[i])
    }
    return(Qualifications)
}
