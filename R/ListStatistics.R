ListStatistics <-
function (stat = NULL, value.type = NULL, type = NULL) {
    stats <- setNames(as.data.frame(rbind(
        c("NumberAssignmentsPending", "Long", "GetRequesterStatistic"),
        c("NumberAssignmentsApproved", "Long", "GetRequesterStatistic"),
        c("NumberAssignmentsRejected", "Long", "GetRequesterStatistic"),
        c("NumberAssignmentsAbandoned", "Long", "GetRequesterStatistic"),
        c("NumberHITsCreated", "Long", "GetRequesterStatistic"),
        c("NumberHITsCompleted", "Long", "GetRequesterStatistic"),
        c("NumberHITsAssignable", "Long", "GetRequesterStatistic"),
        c("NumberHITsReviewable", "Long", "GetRequesterStatistic"),
        c("PercentAssignmentsApproved", "Double", "GetRequesterStatistic"),
        c("PercentAssignmentsRejected", "Double", "GetRequesterStatistic"),
        c("TotalRewardPayout", "Double", "GetRequesterStatistic"),
        c("AverageRewardAmount", "Double", "GetRequesterStatistic"),
        c("TotalRewardFeePayout", "Double", "GetRequesterStatistic"),
        c("TotalBonusPayout", "Double", "GetRequesterStatistic"), 
        c("TotalBonusFeePayout", "Double", "GetRequesterStatistic"), 
        c("EstimatedRewardLiability", "Double", "GetRequesterStatistic"), 
        c("EstimatedFeeLiability", "Double", "GetRequesterStatistic"), 
        c("EstimatedTotalLiability", "Double", "GetRequesterStatistic"), 
        c("NumberAssignmentsApproved", "Long", "GetRequesterWorkerStatistic"), 
        c("NumberAssignmentsRejected", "Long", "GetRequesterWorkerStatistic"), 
        c("NumberKnownAnswersCorrect", "Long", "GetRequesterWorkerStatistic"), 
        c("NumberKnownAnswersIncorrect", "Long", "GetRequesterWorkerStatistic"), 
        c("NumberKnownAnswersEvaluated", "Long", "GetRequesterWorkerStatistic"), 
        c("NumberPluralityAnswersCorrect", "Long", "GetRequesterWorkerStatistic"), 
        c("NumberPluralityAnswersIncorrect", "Long", "GetRequesterWorkerStatistic"), 
        c("NumberPluralityAnswersEvaluated", "Long", "GetRequesterWorkerStatistic"), 
        c("PercentAssignmentsApproved", "Double", "GetRequesterWorkerStatistic"), 
        c("PercentAssignmentsRejected", "Double", "GetRequesterWorkerStatistic"), 
        c("PercentKnownAnswersCorrect", "Double", "GetRequesterWorkerStatistic"), 
        c("PercentPluralityAnswersCorrect", "Double", "GetRequesterWorkerStatistic")),
            stringsAsFactors = FALSE), c("Statistic", "ValueType", "Type"))
    if (!is.null(stat) & is.null(type) & is.null(value.type)) {
        return(stats[stat, ])
    }
    if (is.null(stat) & !is.null(type) & is.null(value.type)) {
        return(stats[stats$Type == type,])
    }
    if (is.null(stat) & is.null(type) & !is.null(value.type)) {
        return(stats[stats$ValueType == value.type,])
    }
    if (is.null(stat) & (!is.null(type) & !is.null(value.type))) {
        return(stats[stats$Type == type & stats$ValueType == value.type,])
    } else {
        return(stats)
    }
}
