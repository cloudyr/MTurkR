GrantBonus <-
bonus <-
paybonus <-
function(workers, assignments, amounts, reasons,
         unique.request.token = NULL, 
         verbose = getOption('MTurkR.verbose', TRUE), ...) {
    operation <- "GrantBonus"
    if (!length(workers) == length(assignments)) {
        stop("Number of workers does not match number of assignments")
    }
    if (length(amounts) == 1) {
        amounts <- rep(amounts[1], length(workers))
    }
    if (!length(amounts) == length(workers)) {
        stop("Number of amounts is not 1 nor length(workers)")
    }
    if (length(reasons) == 1) {
        reasons <- rep(reasons[1], length(workers))
    }
    if (!length(reasons) == length(workers)) {
        stop("Number of reasons is not 1 nor length(workers)")
    }
    if (is.factor(workers)) {
        workers <- as.character(workers)
    }
    if (is.factor(assignments)) {
        assignments <- as.character(assignments)
    }
    if (is.factor(reasons)) {
        reasons <- as.character(reasons)
    }
    if (is.factor(amounts)) {
        amounts <- as.character(amounts)
    }
    for (i in 1:length(amounts)) {
        if (!is.numeric(as.numeric(amounts))) {
            stop(paste("Non-numeric bonus amount requested for bonus ", i, sep = ""))
        }
    }
    if (!is.null(unique.request.token)) {
        if (any(nchar(curl_escape(unique.request.token)) > 64)) {
            stop("'unique.request.token' values must be <= 64 characters")
        }
        if (length(unique.request.token) < length(workers)) {
            stop("'unique.request.token' must be same length as 'workers'")
        }
        if (any(duplicated(unique.request.token))) {
            stop("'unique.request.token' values must be unique")
        }
    }
    Bonuses <- emptydf(length(workers), 5, c("WorkerId", "AssignmentId", "Amount", "Reason", "Valid"))
    for (i in 1:length(workers)) {
        GETparameters <- paste("&WorkerId=", workers[i], 
                               "&AssignmentId=", assignments[i], 
                               "&BonusAmount.1.Amount=", amounts[i], 
                               "&BonusAmount.1.CurrencyCode=USD", 
                               "&Reason=", curl_escape(reasons[i]), sep = "")
        if (!is.null(unique.request.token)) {
            GETparameters <- paste0(GETparameters, "&UniqueRequestToken=", 
                                    curl_escape(unique.request.token[i]))
        }
        request <- request(operation, GETparameters = GETparameters, ...)
        if (is.null(request$valid)) {
            return(request)
        }
        Bonuses[i, ] <- c(workers[i], assignments[i], amounts[i], reasons[i], request$valid)
        if (request$valid == TRUE) {
            if (verbose) {
                message(i, ": Bonus of ", amounts[i], " granted to ", 
                        workers[i], " for assignment ", assignments[i])
            }
        } else if (request$valid == FALSE) {
            if (verbose) {
                warning("Invalid Request for worker ", workers[i])
            }
        }
    }
    Bonuses$Valid <- factor(Bonuses$Valid, levels=c('TRUE','FALSE'))
    return(Bonuses)
}
