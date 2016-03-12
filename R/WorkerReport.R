WorkerReport <-
function (worker, period = "LifeToDate", verbose = getOption('MTurkR.verbose', TRUE), ...){
    if (is.na(pmatch(period,c("OneDay", "SevenDays", "ThirtyDays", "LifeToDate"))))  {
        stop("Period not valid. Must be 'OneDay', 'SevenDays', 'ThirtyDays', or 'LifeToDate'.")
    }
    statistics <- c("NumberAssignmentsApproved", "NumberAssignmentsRejected", 
                    "NumberKnownAnswersCorrect", "NumberKnownAnswersIncorrect", 
                    "NumberKnownAnswersEvaluated", "NumberPluralityAnswersCorrect", 
                    "NumberPluralityAnswersIncorrect", "NumberPluralityAnswersEvaluated", 
                    "PercentAssignmentsApproved", "PercentAssignmentsRejected", 
                    "PercentKnownAnswersCorrect", "PercentPluralityAnswersCorrect")
    z <- data.frame(Statistic = statistics, 
                    Value = sapply(statistics, function(i) {
                        GetWorkerStatistic(worker, i, period = period, verbose = verbose, ...)
                        }), 
                    row.names = seq_along(statistics) )
    return(z)
}
