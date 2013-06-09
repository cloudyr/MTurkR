GrantBonus <-
bonus <-
paybonus <-
function (workers, assignments, amounts, reasons, keypair = credentials(), 
    print = FALSE, browser = FALSE, log.requests = TRUE, sandbox = FALSE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "GrantBonus"
    if (!length(workers) == length(assignments)) 
        stop("Number of workers does not match number of assignments")
    if (length(amounts) == 1) 
        amounts <- rep(amounts[1], length(workers))
    if (!length(amounts) == length(workers)) 
        stop("Number of amounts is not 1 nor length(workers)")
    if (length(reasons) == 1) 
        reasons <- rep(reasons[1], length(workers))
    if (!length(reasons) == length(workers)) 
        stop("Number of reasons is not 1 nor length(workers)")
    for (i in 1:length(amounts)) {
        if (!is.numeric(as.numeric(amounts))) 
            stop(paste("Non-numeric bonus amount requested for bonus ", 
                i, sep = ""))
    }
    Bonuses <- data.frame(matrix(nrow = length(workers), ncol = 5))
    names(Bonuses) <- c("WorkerId", "AssignmentId", "Amount", 
        "Reason", "Valid")
    for (i in 1:length(workers)) {
        GETparameters <- paste("&WorkerId=", workers[i], "&AssignmentId=", 
            assignments[i], "&BonusAmount.1.Amount=", amounts[i], 
            "&BonusAmount.1.CurrencyCode=USD", "&Reason=", curlEscape(reasons[i]), 
            sep = "")
        auth <- authenticate(operation, secret)
        if (browser == TRUE) {
            request <- request(keyid, auth$operation, auth$signature, 
                auth$timestamp, GETparameters, browser = browser, 
                sandbox = sandbox)
        }
        else {
            request <- request(keyid, auth$operation, auth$signature, 
                auth$timestamp, GETparameters, log.requests = log.requests, 
                sandbox = sandbox)
            Bonuses[i, ] <- c(workers[i], assignments[i], amounts[i], 
                reasons[i], request$valid)
            if (request$valid == TRUE) {
                if (print == TRUE) 
                  cat(i, ": Bonus of ", amounts[i], " granted to ", 
                    workers[i], " for assignment ", assignments[i], 
                    "\n", sep = "")
            }
            else if (request$valid == FALSE) {
                if (print == TRUE) 
                  cat("Invalid Request for worker ", workers[i], 
                    "\n")
            }
        }
    }
    if (print == TRUE) 
        return(Bonuses)
    else invisible(Bonuses)
}
