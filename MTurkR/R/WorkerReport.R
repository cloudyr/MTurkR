WorkerReport <-
function (worker, period = "LifeToDate", keypair = credentials(), 
    log.requests = TRUE, sandbox = FALSE, print = TRUE) 
{
    if (!period %in% c("OneDay", "SevenDays", "ThirtyDays", "LifeToDate")) 
        stop("Period not valid")
    statistics <- c("NumberAssignmentsApproved", "NumberAssignmentsRejected", 
        "NumberKnownAnswersCorrect", "NumberKnownAnswersIncorrect", 
        "NumberKnownAnswersEvaluated", "NumberPluralityAnswersCorrect", 
        "NumberPluralityAnswersIncorrect", "NumberPluralityAnswersEvaluated", 
        "PercentAssignmentsApproved", "PercentAssignmentsRejected", 
        "PercentKnownAnswersCorrect", "PercentPluralityAnswersCorrect")
    z <- data.frame(matrix(nrow = length(statistics), ncol = 2))
    names(z) <- c("Statistic", "Value")
    z[, 1] <- statistics
    for (i in 1:length(statistics)) {
        z[i, 2] <- GetWorkerStatistic(worker, statistics[i], 
            period = period, keypair = keypair, print = FALSE, 
            log.requests = log.requests, sandbox = sandbox)
    }
    if (print == TRUE) 
        print(z)
    invisible(z)
}
