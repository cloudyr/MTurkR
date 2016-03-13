SufficientFunds <-
function (amount = NULL, assignments = NULL, hits = NULL, bonus.ct = NULL, 
    bonus.amount = NULL, masters = FALSE, turkfee = 0.2, turkmin = 0.01, 
    mastersfee = 0.05, ...)
{
    total <- 0
    payments <- 0
    payment.fee <- 0
    masters.fee <- 0
    bonuses <- 0
    bonus.fee <- 0
    amount <- as.numeric(amount)
    if (!is.null(assignments)) {
        assignments <- as.numeric(assignments)
        if (assignments >= 10) {
            turkfee <- .40
        }
    }
    if (!is.null(hits)) {
        hits <- as.numeric(hits)
    }
    if (!is.null(bonus.ct)) {
        bonus.ct <- as.numeric(bonus.ct)
    }
    if (!is.null(bonus.amount)) {
        bonus.amount <- as.numeric(bonus.amount)
    }
    if (!is.null(assignments)) {
        if (is.null(hits)) {
            hits <- 1
        }
        assign.total <- hits * assignments
        payments <- assign.total * amount
        if ((turkfee * amount) < turkmin) {
            payment.fee <- turkmin * assign.total
        } else {
            payment.fee <- turkfee * payments
        }
        total <- payments + payment.fee
        if (!is.null(masters) && masters == TRUE) {
            masters.fee <- mastersfee * payments
            total <- total + masters.fee
        }
    }
    if (!is.null(bonus.amount)) {
        if (is.null(bonus.ct)) {
            stop("Must supply both 'bonus.amount' and 'bonus.ct'")
        }
        bonuses <- bonus.ct * bonus.amount
        if ((turkfee * bonus.amount) < turkmin) {
            bonus.fee <- turkmin * bonus.ct
        } else {
            bonus.fee <- turkfee * bonuses
        }
        total <- total + bonuses + bonus.fee
    }
    request <- AccountBalance(verbose = FALSE, ...)
    oldbalance <- as.numeric(request$balance)
    newbalance <- oldbalance - total
    structure(list(Total = round(total, 3),
                   OldBalance = round(oldbalance, 3),
                   NewBalance = round(newbalance, 3), 
                   Total = total,
                   Payments = payments,
                   PaymentFee = payment.fee,
                   MastersFee = masters.fee,
                   Bonuses = bonuses,
                   BonusFee = bonus.fee),
              class = 'SufficientFunds')
}

print.SufficientFunds <- function(x, ...){
    cat("Total Payments:  ", sprintf('$%0.2f', x$Payments), '\n')
    cat("Payment Fee:     ", sprintf('$%0.2f', x$PaymentFee), '\n')
    cat("Masters Fee:     ", sprintf('$%0.2f', x$MastersFee), '\n')
    cat("Bonuses:         ", sprintf('$%0.2f', x$Bonuses), '\n')
    cat("Bonus Fee:       ", sprintf('$%0.2f', x$BonusFee), '\n')
    cat("-------------------------\n")
    cat("  Old Balance:   ", sprintf('$%0.2f', x$OldBalance), '\n')
    cat("  Total Cost:    ", sprintf('$%0.2f', x$Total), '\n')
    if (x$NewBalance >= 0) {
        cat("  New Balance:   ", sprintf('$%0.2f', x$NewBalance), " < SUFFICIENT FUNDS\n\n")
    } else {
        cat("  New Balance:   ", sprintf('$%0.2f', x$NewBalance), " < INSUFFICIENT FUNDS!\n\n")
    }
    invisible(x)
}
