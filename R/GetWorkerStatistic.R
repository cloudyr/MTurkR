GetWorkerStatistic <-
workerstatistic <-
function (worker, statistic, period = "LifeToDate", count = NULL, 
    response.group = NULL, keypair = getOption('MTurkR.keypair'),
    print = getOption('MTurkR.print'), 
    log.requests = getOption('MTurkR.log'),
    sandbox = getOption('MTurkR.sandbox'), validation.test = getOption('MTurkR.test')) {
    if(is.null(keypair))
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "GetRequesterWorkerStatistic"
    value.integer <- c("NumberAssignmentsApproved", "NumberAssignmentsRejected", 
        "NumberKnownAnswersCorrect", "NumberKnownAnswersIncorrect", 
        "NumberKnownAnswersEvaluated", "NumberPluralityAnswersCorrect", 
        "NumberPluralityAnswersIncorrect", "NumberPluralityAnswersEvaluated")
    value.double <- c("PercentAssignmentsApproved", "PercentAssignmentsRejected", 
        "PercentKnownAnswersCorrect", "PercentPluralityAnswersCorrect")
    if(!statistic %in% value.integer & !statistic %in% value.double) 
        stop("Statistic not valid")
    if(!period %in% c("OneDay", "SevenDays", "ThirtyDays", "LifeToDate")) 
        stop("Period not valid")
    GETparameters <- paste("&WorkerId=", worker, "&Statistic=", 
        statistic, "&TimePeriod=", period, sep = "")
    if(!is.null(count)) {
        if(is.na(as.numeric(count))) 
            stop("Count must be numeric or coercible to numeric")
        else
            GETparameters <- paste(GETparameters, "&Count=", count, sep = "")
    }
    
    request <- request(keypair[1], operation, secret=keypair[2],
        GETparameters = GETparameters, log.requests = log.requests, 
        sandbox = sandbox, validation.test = validation.test)
    if(validation.test)
        return(invisible(request))
    request$statistic <- statistic
    request$period <- period
    if(request$valid == TRUE) {
        if(!is.null(count) & period == "OneDay") {
            request$value <- setNames(data.frame(matrix(nrow = count, ncol = 2)),
                                c("Date", "Value"))
            for(i in 1:count) {
                request$value[i, 1] <- strsplit(strsplit(request$xml, 
                    "<Date>")[[1]][2], "</Date>")[[1]][1]
                if(statistic %in% value.integer) 
                    request$value[i, 2] <- strsplit(strsplit(request$xml, 
                    "<LongValue>")[[1]][2], "</LongValue>")[[1]][1]
                else if(statistic %in% value.double) 
                    request$value[i, 2] <- strsplit(strsplit(request$xml, 
                    "<DoubleValue>")[[1]][2], "</DoubleValue>")[[1]][1]
                else
                    warning("Cannot print statistic value")
            }
            if(print == TRUE) 
                message("Statistic (", statistic, ", past ", count, 
                    " days) for ", worker, " Retrieved: ", request$value)
            return(invisible(request$value))
        }
        else {
            request$date <- strsplit(strsplit(request$xml, 
                "<Date>")[[1]][2], "</Date>")[[1]][1]
            if(statistic %in% value.integer) 
                request$value <- strsplit(strsplit(request$xml, 
                "<LongValue>")[[1]][2], "</LongValue>")[[1]][1]
            else if(statistic %in% value.double) 
                request$value <- strsplit(strsplit(request$xml, 
                "<DoubleValue>")[[1]][2], "</DoubleValue>")[[1]][1]
            else
                warning("Cannot print statistic value")
            if(print == TRUE)
              message("Statistic (", statistic, ",", period, 
                    ") for ", worker, " Retrieved: ", request$value)
            return(invisible(request$value))
        }
    }
    else if(request$valid == FALSE) {
        if(print == TRUE) 
            warning("Invalid Request")
        return(NULL)
    }
}
