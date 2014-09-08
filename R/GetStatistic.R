GetStatistic <-
statistic <-
function (statistic, period = "LifeToDate", count = NULL, response.group = NULL, 
    verbose = getOption('MTurkR.verbose'), ...) {
    # temporary check for `print` argument (remove after v1.0)
    if('print' %in% names(list(...)) && is.null(verbose))
        verbose <- list(...)$print
    operation <- "GetRequesterStatistic"
    value.long <- c("NumberAssignmentsAvailable", "NumberAssignmentsAccepted", 
        "NumberAssignmentsPending", "NumberAssignmentsApproved", 
        "NumberAssignmentsRejected", "NumberAssignmentsReturned", 
        "NumberAssignmentsAbandoned", "NumberHITsCreated", "NumberHITsCompleted", 
        "NumberHITsAssignable", "NumberHITsReviewable")
    value.double <- c("PercentAssignmentsApproved", "PercentAssignmentsRejected", 
        "TotalRewardPayout", "AverageRewardAmount", "TotalRewardFeePayout", 
        "TotalFeePayout", "TotalRewardAndFeePayout", "TotalBonusPayout", 
        "TotalBonusFeePayout", "EstimatedRewardLiability", "EstimatedFeeLiability", 
        "EstimatedTotalLiability")
    if(!statistic %in% value.long & !statistic %in% value.double) 
        stop("Statistic not valid")
    if(!period %in% c("OneDay", "SevenDays", "ThirtyDays", "LifeToDate")) 
        stop("Period not valid")
    GETparameters <- paste("&Statistic=", statistic, "&TimePeriod=", 
        period, sep = "")
    if(!is.null(count)) {
        if(is.na(as.numeric(count))) 
            stop("Count must be numeric or coercible to numeric")
        else
            GETparameters <- paste(GETparameters, "&Count=", count, sep = "")
    }
    
    request <- request(operation, GETparameters = GETparameters, ...)
    if(is.null(request$valid))
        return(request)
    request$statistic <- statistic
    request$period <- period
    if(request$valid == TRUE) {
        if(!is.null(count) & period == "OneDay") {
            request$value <- setNames(data.frame(matrix(nrow = count, ncol = 2)),
                                    c("Date", "Value"))
            for (i in 1:count) {
                request$value[i, 1] <- strsplit(strsplit(request$xml, 
                    "<Date>")[[1]][2], "</Date>")[[1]][1]
                if(statistic %in% value.long) 
                    request$value[i, 2] <- strsplit(strsplit(request$xml, 
                    "<LongValue>")[[1]][2], "</LongValue>")[[1]][1]
                else if(statistic %in% value.double) 
                    request$value[i, 2] <- strsplit(strsplit(request$xml, 
                    "<DoubleValue>")[[1]][2], "</DoubleValue>")[[1]][1]
                else
                    warning("Cannot print statistic value")
            }
            if(verbose) {
                message("Statistic (", statistic, ", past ", count, 
                    " days) Retrieved: ", request$value)
            }
        }
        else {
            request$date <- strsplit(strsplit(request$xml, 
                "<Date>")[[1]][2], "</Date>")[[1]][1]
            if(statistic %in% value.long) 
                request$value <- strsplit(strsplit(request$xml, 
                "<LongValue>")[[1]][2], "</LongValue>")[[1]][1]
            else if(statistic %in% value.double) 
                request$value <- strsplit(strsplit(request$xml, 
                "<DoubleValue>")[[1]][2], "</DoubleValue>")[[1]][1]
            if(verbose) 
                message(statistic, " (", period, "): ", request$value)
        }
        return(request$value)
    }
    else if(request$valid == FALSE){
        if(verbose)
            warning("Invalid Request")
        return(NULL)
    }
}
