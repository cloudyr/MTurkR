UpdateQualificationScore <-
updatequalscore <-
function (qual, workers, values = NULL, increment = NULL,
    keypair = getOption('MTurkR.keypair'),
    print = getOption('MTurkR.print'),
    log.requests = getOption('MTurkR.log'), sandbox = getOption('MTurkR.sandbox'),
    validation.test = getOption('MTurkR.test')) {
    if(is.null(keypair))
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "UpdateQualificationScore"
    if(is.factor(qual))
        qual <- as.character(qual)
    if(is.factor(workers))
        workers <- as.character(workers)
    if(!is.null(increment)) {
        values <- NA
        score <- NA
        for(i in 1:length(workers)) {
            score[i] <- GetQualificationScore(qual, workers[i], 
                keypair = keypair, log.requests = log.requests, 
                sandbox = sandbox)$Value[1]
            if(is.null(score[i]) || is.na(score[i])) 
                score[i] <- 0
        }
        values <- as.numeric(score) + as.numeric(increment)
    }
    if(!is.null(values)) {
        for(i in 1:length(values)) {
            if(!is.numeric(as.numeric(values[i]))) 
                stop("Value is non-numeric or not coercible to numeric")
        }
        if(length(values) == 1) 
            values <- rep(values[1], length(workers))
        if(!length(workers) == length(values)) 
            stop("!length(workers)==length(values)")
    }
    else
        stop("Value(s) is/are missing")
    Qualifications <- data.frame(matrix(nrow = length(workers), ncol = 4))
    names(Qualifications) <- c("QualificationTypeId", "WorkerId", "Value", "Valid")
    for(i in 1:length(workers)) {
        GETparameters <- paste("&QualificationTypeId=", qual, 
            "&SubjectId=", workers[i], "&IntegerValue=", values[i], sep = "")
        
        request <- request(keypair[1], operation, secret=keypair[2],
            GETparameters = GETparameters, log.requests = log.requests, 
            sandbox = sandbox, validation.test = validation.test)
        if(validation.test)
            return(invisible(request))
        Qualifications[i, ] <- c(qual, workers[i], values[i], request$valid)
        if(request$valid == TRUE & print == TRUE) {
            message(i, ": Qualification Score for Worker ", 
                workers[i], " updated to ", values[i])
        }
        else if(request$valid == FALSE & print == TRUE)
            warning(i, ": Invalid Request for worker ", workers[i])
    }
    if(print == TRUE) 
        message(i, " Qualification Scores Updated")
    Qualifications$Valid <- factor(Qualifications$Valid, levels=c('TRUE','FALSE'))
    return(Qualifications)
}
