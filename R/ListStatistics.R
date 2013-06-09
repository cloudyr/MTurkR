ListStatistics <-
function (stat = NULL, value.type = NULL, type = NULL) 
{
    stats <- as.data.frame(rbind(c("NumberAssignmentsAvailable", 
        "Long", "GetRequesterStatistic"), c("NumberAssignmentsAccepted", 
        "Long", "GetRequesterStatistic"), c("NumberAssignmentsPending", 
        "Long", "GetRequesterStatistic"), c("NumberAssignmentsApproved", 
        "Long", "GetRequesterStatistic"), c("NumberAssignmentsRejected", 
        "Long", "GetRequesterStatistic"), c("NumberAssignmentsReturned", 
        "Long", "GetRequesterStatistic"), c("NumberAssignmentsAbandoned", 
        "Long", "GetRequesterStatistic"), c("NumberHITsCreated", 
        "Long", "GetRequesterStatistic"), c("NumberHITsCompleted", 
        "Long", "GetRequesterStatistic"), c("NumberHITsAssignable", 
        "Long", "GetRequesterStatistic"), c("NumberHITsReviewable", 
        "Long", "GetRequesterStatistic"), c("PercentAssignmentsApproved", 
        "Double", "GetRequesterStatistic"), c("PercentAssignmentsRejected", 
        "Double", "GetRequesterStatistic"), c("TotalRewardPayout", 
        "Double", "GetRequesterStatistic"), c("AverageRewardAmount", 
        "Double", "GetRequesterStatistic"), c("TotalRewardFeePayout", 
        "Double", "GetRequesterStatistic"), c("TotalFeePayout", 
        "Double", "GetRequesterStatistic"), c("TotalRewardAndFeePayout", 
        "Double", "GetRequesterStatistic"), c("TotalBonusPayout", 
        "Double", "GetRequesterStatistic"), c("TotalBonusFeePayout", 
        "Double", "GetRequesterStatistic"), c("EstimatedRewardLiability", 
        "Double", "GetRequesterStatistic"), c("EstimatedFeeLiability", 
        "Double", "GetRequesterStatistic"), c("EstimatedTotalLiability", 
        "Double", "GetRequesterStatistic"), c("NumberAssignmentsApproved", 
        "Long", "GetRequesterWorkerStatistic"), c("NumberAssignmentsRejected", 
        "Long", "GetRequesterWorkerStatistic"), c("NumberKnownAnswersCorrect", 
        "Long", "GetRequesterWorkerStatistic"), c("NumberKnownAnswersIncorrect", 
        "Long", "GetRequesterWorkerStatistic"), c("NumberKnownAnswersEvaluated", 
        "Long", "GetRequesterWorkerStatistic"), c("NumberPluralityAnswersCorrect", 
        "Long", "GetRequesterWorkerStatistic"), c("NumberPluralityAnswersIncorrect", 
        "Long", "GetRequesterWorkerStatistic"), c("NumberPluralityAnswersEvaluated", 
        "Long", "GetRequesterWorkerStatistic"), c("PercentAssignmentsApproved", 
        "Double", "GetRequesterWorkerStatistic"), c("PercentAssignmentsRejected", 
        "Double", "GetRequesterWorkerStatistic"), c("PercentKnownAnswersCorrect", 
        "Double", "GetRequesterWorkerStatistic"), c("PercentPluralityAnswersCorrect", 
        "Double", "GetRequesterWorkerStatistic")), stringsAsFactors = FALSE)
    names(stats) <- c("Statistic", "ValueType", "Type")
    Type <- NULL
    ValueType <- NULL
    if (!is.null(stat) & is.null(type) & is.null(value.type)) 
        return(stats[stat, ])
    if (is.null(stat) & !is.null(type) & is.null(value.type)) 
        return(subset(stats, Type == type))
    if (is.null(stat) & is.null(type) & !is.null(value.type)) 
        return(subset(stats, ValueType == value.type))
    if (is.null(stat) & (!is.null(type) & !is.null(value.type))) 
        return(subset(stats, Type == type & ValueType == value.type))
    else return(stats)
}
