SufficientFunds <-
function (amount, assignments = NULL, hits = NULL, bonus.ct = NULL, 
    bonus.amount = NULL, masters = FALSE, turkfee = 0.1, turkmin = 0.005, 
    mastersfee = 0.2, keypair = credentials(), print = TRUE, 
    log.requests = TRUE, sandbox = FALSE) 
{
    total <- 0
    payments <- 0
    payment.fee <- 0
    masters.fee <- 0
    bonuses <- 0
    bonus.fee <- 0
    amount <- as.numeric(amount)
    if (!is.null(assignments)) 
        assignments <- as.numeric(assignments)
    if (!is.null(hits)) 
        hits <- as.numeric(hits)
    if (!is.null(bonus.ct)) 
        bonus.ct <- as.numeric(bonus.ct)
    if (!is.null(bonus.amount)) 
        bonus.amount <- as.numeric(bonus.amount)
    if (!is.null(assignments)) {
        if (is.null(hits)) 
            hits <- 1
        assign.total <- hits * assignments
        payments <- assign.total * amount
        if ((turkfee * amount) < turkmin) 
            payment.fee <- turkmin * assign.total
        else payment.fee <- turkfee * payments
        total <- payments + payment.fee
        if (!is.null(masters) && masters == TRUE) {
            masters.fee <- mastersfee * payments
            total <- total + masters.fee
        }
    }
    if (!is.null(bonus.amount)) {
        if (is.null(bonus.ct)) 
            stop("Must supply both 'bonus.amount' and 'bonus.ct'")
        bonuses <- bonus.ct * bonus.amount
        if ((turkfee * bonus.amount) < turkmin) 
            bonus.fee <- turkmin * bonus.ct
        else bonus.fee <- turkfee * bonuses
        total <- total + bonuses + bonus.fee
    }
    balchar <- AccountBalance(print = FALSE)
    oldbalance <- as.numeric(substring(balchar, 1, nchar(balchar)))
    newbalance <- oldbalance - total
    if (newbalance >= 0) 
        sufficient <- TRUE
    else sufficient <- FALSE
    if (print == TRUE) {
        cat("Total Payments:  $", round(payments, 2), "\n", sep = "")
        cat("Payment Fee:     $", round(payment.fee, 2), "\n", 
            sep = "")
        cat("Masters Fee:     $", round(masters.fee, 2), "\n", 
            sep = "")
        cat("Bonuses:         $", round(bonuses, 2), "\n", sep = "")
        cat("Bonus Fee:       $", round(bonus.fee, 2), "\n", 
            sep = "")
        cat("-------------------------\n")
        cat("  Old Balance:   $", round(oldbalance, 2), "\n", 
            sep = "")
        cat("  Total Cost:    $", round(total, 2), "\n", sep = "")
        if (sufficient == TRUE) 
            cat("  New Balance:   $", round(newbalance, 2), " < SUFFICIENT\n", 
                sep = "")
        else cat("  New Balance:   $", round(newbalance, 2), 
            " < INSUFFICIENT\n", sep = "")
        cat("\n")
    }
    invisible(list(Total = round(total, 3), OldBalance = round(oldbalance, 
        3), NewBalance = round(newbalance, 3), SufficientFunds = sufficient))
}
