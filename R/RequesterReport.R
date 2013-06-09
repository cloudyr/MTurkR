RequesterReport <-
function (period = "LifeToDate", keypair = credentials(), log.requests = TRUE, 
    sandbox = FALSE, print = TRUE) 
{
    if (!period %in% c("OneDay", "SevenDays", "ThirtyDays", "LifeToDate")) 
        stop("Period not valid")
    statistics <- c("NumberAssignmentsAvailable", "NumberAssignmentsAccepted", 
        "NumberAssignmentsPending", "NumberAssignmentsApproved", 
        "NumberAssignmentsRejected", "NumberAssignmentsReturned", 
        "NumberAssignmentsAbandoned", "NumberHITsCreated", "NumberHITsCompleted", 
        "NumberHITsAssignable", "NumberHITsReviewable", "PercentAssignmentsApproved", 
        "PercentAssignmentsRejected", "TotalRewardPayout", "AverageRewardAmount", 
        "TotalRewardFeePayout", "TotalFeePayout", "TotalRewardAndFeePayout", 
        "TotalBonusPayout", "TotalBonusFeePayout", "EstimatedRewardLiability", 
        "EstimatedFeeLiability", "EstimatedTotalLiability")
    z <- data.frame(matrix(nrow = length(statistics), ncol = 2))
    names(z) <- c("Statistic", "Value")
    z[, 1] <- statistics
    for (i in 1:20) {
        z[i, 2] <- GetStatistic(statistics[i], period = period, 
            keypair = keypair, print = FALSE, log.requests = log.requests, 
            sandbox = sandbox)
    }
    for (i in 21:23) {
        z[i, 2] <- GetStatistic(statistics[i], period = "LifeToDate", 
            keypair = keypair, print = FALSE, log.requests = log.requests, 
            sandbox = sandbox)
    }
    if (print == TRUE) 
        print(z)
    invisible(z)
}
