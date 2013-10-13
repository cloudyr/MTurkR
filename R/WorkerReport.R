WorkerReport <-
function (worker, period = "LifeToDate", keypair = credentials(), 
    log.requests = getOption('MTurkR.log'), sandbox = getOption('MTurkR.sandbox'),
    print = getOption('MTurkR.print')) 
{
    if(is.na(pmatch(period,c("OneDay", "SevenDays", "ThirtyDays", "LifeToDate")))) 
        stop("Period not valid. Must be 'OneDay', 'SevenDays', 'ThirtyDays', or 'LifeToDate'.   ")
    statistics <- c("NumberAssignmentsApproved", "NumberAssignmentsRejected", 
        "NumberKnownAnswersCorrect", "NumberKnownAnswersIncorrect", 
        "NumberKnownAnswersEvaluated", "NumberPluralityAnswersCorrect", 
        "NumberPluralityAnswersIncorrect", "NumberPluralityAnswersEvaluated", 
        "PercentAssignmentsApproved", "PercentAssignmentsRejected", 
        "PercentKnownAnswersCorrect", "PercentPluralityAnswersCorrect")
    z <- data.frame(Statistic=statistics, Value =
        sapply(statistics, function(i) GetWorkerStatistic(worker, i, 
                period = period, keypair = keypair, print = FALSE, 
                log.requests = log.requests, sandbox = sandbox)), row.names=1:length(statistics) )
    if (print == TRUE) 
        print(z)
    invisible(z)
}
