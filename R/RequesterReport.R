RequesterReport <-
function (period = "LifeToDate", verbose = getOption('MTurkR.verbose', TRUE), ...) {
    if (!period %in% c("OneDay", "SevenDays", "ThirtyDays", "LifeToDate")) {
        stop("Period not valid")
    }
    statistics <- c(
        "NumberAssignmentsPending", "NumberAssignmentsApproved", "NumberAssignmentsRejected", 
        "NumberAssignmentsAbandoned", "NumberHITsCreated", "NumberHITsCompleted", 
        "NumberHITsAssignable", "NumberHITsReviewable", "PercentAssignmentsApproved", 
        "PercentAssignmentsRejected", "TotalRewardPayout", "AverageRewardAmount", 
        "TotalRewardFeePayout", "TotalBonusPayout", 
        "TotalBonusFeePayout", "EstimatedRewardLiability", 
        "EstimatedFeeLiability", "EstimatedTotalLiability")
    z <- emptydf(length(statistics), 2, c("Statistic", "Value"))
    z[["Statistic"]] <- statistics
    only_ltd <- c("NumberHITsAssignable", "EstimatedRewardLiability", "EstimatedFeeLiability", "EstimatedTotalLiability")
    for (i in seq_along(statistics)) {
        if (statistics[i] %in% only_ltd) {
            if (statistics[i] == "NumberHITsAssignable" && period != "LifeToDate") {
                message("'NumberHITsAssignable' is being retrieved for period 'LifeToDate'")
            }
            z[i, 2] <- GetStatistic(statistics[i], period = "LifeToDate", verbose = verbose, ...)[["Value"]]
        } else {
            z[i, 2] <- GetStatistic(statistics[i], period = period, verbose = verbose, ...)[["Value"]]
        }
    } 
    return(z)
}
