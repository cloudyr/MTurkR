GetStatistic <-
statistic <-
function (statistic, period = "LifeToDate", count = NULL, response.group = NULL, 
    verbose = getOption('MTurkR.verbose', TRUE), ...) {
    operation <- "GetRequesterStatistic"
    value.long <- c("NumberAssignmentsPending", "NumberAssignmentsApproved", 
                    "NumberAssignmentsRejected", "NumberAssignmentsAbandoned", 
                    "NumberHITsCreated", "NumberHITsCompleted", 
                    "NumberHITsAssignable", "NumberHITsReviewable")
    value.double <- c("PercentAssignmentsApproved", "PercentAssignmentsRejected", 
                      "TotalRewardPayout", "AverageRewardAmount", "TotalRewardFeePayout", 
                      "TotalBonusPayout", "TotalBonusFeePayout", 
                      "EstimatedRewardLiability", "EstimatedFeeLiability", 
                      "EstimatedTotalLiability")
    if (!statistic %in% value.long & !statistic %in% value.double) {
        stop("Statistic not valid")
    }
    if (!period %in% c("OneDay", "SevenDays", "ThirtyDays", "LifeToDate")) {
        stop("Period not valid")
    }
    GETparameters <- paste("&Statistic=", statistic, "&TimePeriod=", period, sep = "")
    if (!is.null(count)) {
        if (is.na(as.numeric(count))) {
            stop("Count must be numeric or coercible to numeric")
        } else {
            GETparameters <- paste(GETparameters, "&Count=", count, sep = "")
        }
    }
    request <- request(operation, GETparameters = GETparameters, ...)
    if (is.null(request$valid)) {
        return(request)
    }
    request$statistic <- statistic
    request$period <- period
    if (request$valid == TRUE) {
        if (period == "OneDay" && !is.null(count)) {
            request$value <- emptydf(count, 3, c("Date", "Value", "Statistic"))
            request$value$Statistic <- statistic
            for (i in 1:count) {
                request$value[i, 1] <- strsplit(strsplit(request$xml, "<Date>")[[1]][2], "</Date>")[[1]][1]
                if (statistic %in% value.long) {
                    request$value[i, 2] <- strsplit(strsplit(request$xml, "<LongValue>")[[1]][2], "</LongValue>")[[1]][1]
                } else if (statistic %in% value.double) {
                    request$value[i, 2] <- strsplit(strsplit(request$xml, "<DoubleValue>")[[1]][2], "</DoubleValue>")[[1]][1]
                } else {
                    warning("Cannot print statistic value")
                }
            }
            if (verbose) {
                if (as.numeric(count) == 1) {
                    message("Statistic (", statistic, ", past ", count, " days) Retrieved: ", request$value$Value[1])
                } else {
                    message("Statistic (", statistic, ", past ", count, " days) Retrieved")
                }
            }
        } else {
            request$value <- emptydf(1, 3, c("Date", "Value", "Statistic"))
            request$value$Statistic <- statistic
            request$value$Date <- strsplit(strsplit(request$xml, "<Date>")[[1]][2], "</Date>")[[1]][1]
            if(statistic %in% value.long) {
                request$value$Value <- strsplit(strsplit(request$xml, "<LongValue>")[[1]][2], "</LongValue>")[[1]][1]
            } else if(statistic %in% value.double) {
                request$value$Value <- strsplit(strsplit(request$xml, "<DoubleValue>")[[1]][2], "</DoubleValue>")[[1]][1]
            }
            if (verbose) {
                message(statistic, " (", period, "): ", request$value$Value[1])
            }
        }
        return(request$value)
    } else {
        if (verbose) {
            warning("Invalid Request")
        }
        return(emptydf(0, 3, c("Date", "Value", "Statistic")))
    }
}
