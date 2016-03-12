RequesterReport <-
function (period = "LifeToDate", verbose = getOption('MTurkR.verbose', TRUE), ...) {
    if(!period %in% c("OneDay", "SevenDays", "ThirtyDays", "LifeToDate")) 
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
    z <- emptydf(length(statistics), 2, c("Statistic", "Value"))
    z[, 1] <- statistics
    for(i in 1:20)
        z[i, 2] <- GetStatistic(statistics[i], period = period, ...)
    for(i in 21:23)
        z[i, 2] <- GetStatistic(statistics[i], period = "LifeToDate", ...)
    return(z)
}
