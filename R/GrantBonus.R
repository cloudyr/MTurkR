GrantBonus <-
bonus <-
paybonus <-
function(workers, assignments, amounts, reasons,
         verbose = getOption('MTurkR.verbose', TRUE), ...) {
    # temporary check for `print` argument (remove after v1.0)
    if('print' %in% names(list(...)) && is.null(verbose))
        verbose <- list(...)$print
    operation <- "GrantBonus"
    if(!length(workers) == length(assignments)) 
        stop("Number of workers does not match number of assignments")
    if(length(amounts) == 1) 
        amounts <- rep(amounts[1], length(workers))
    if(!length(amounts) == length(workers)) 
        stop("Number of amounts is not 1 nor length(workers)")
    if(length(reasons) == 1) 
        reasons <- rep(reasons[1], length(workers))
    if(!length(reasons) == length(workers)) 
        stop("Number of reasons is not 1 nor length(workers)")
    if(is.factor(workers))
        workers <- as.character(workers)
    if(is.factor(assignments))
        assignments <- as.character(assignments)
    if(is.factor(reasons))
        reasons <- as.character(reasons)
    if(is.factor(amounts))
        amounts <- as.character(amounts)
    for(i in 1:length(amounts)) {
        if(!is.numeric(as.numeric(amounts))) 
            stop(paste("Non-numeric bonus amount requested for bonus ", i, sep = ""))
    }
    Bonuses <- setNames(data.frame(matrix(nrow = length(workers), ncol = 5)),
                    c("WorkerId", "AssignmentId", "Amount", "Reason", "Valid"))
    for(i in 1:length(workers)) {
        GETparameters <- paste("&WorkerId=", workers[i], "&AssignmentId=", 
            assignments[i], "&BonusAmount.1.Amount=", amounts[i], 
            "&BonusAmount.1.CurrencyCode=USD", "&Reason=", curlEscape(reasons[i]), 
            sep = "")
        
        request <- request(operation, GETparameters = GETparameters, ...)
        if(is.null(request$valid))
            return(request)
        Bonuses[i, ] <- c(workers[i], assignments[i], amounts[i], 
            reasons[i], request$valid)
        if(request$valid == TRUE) {
            if(verbose) 
                message(i, ": Bonus of ", amounts[i], " granted to ", 
                workers[i], " for assignment ", assignments[i])
        }
        else if(request$valid == FALSE) {
            if(verbose) 
                warning("Invalid Request for worker ", workers[i])
        }
    }
    Bonuses$Valid <- factor(Bonuses$Valid, levels=c('TRUE','FALSE'))
    return(Bonuses)
}
