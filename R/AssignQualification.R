assignqual <-
AssignQualification <-
AssignQualifications <-
function (qual, workers, value = "1", notify = FALSE, name = NULL, 
    description = NULL, keywords = NULL, status = NULL, retry.delay = NULL, 
    test = NULL, answerkey = NULL, test.duration = NULL, auto = NULL, 
    auto.value = NULL, conditional.statistic = NULL, conditional.comparator = NULL, 
    conditional.value = NULL, conditional.period = NULL, set.statistic.as.value = FALSE, 
    keypair = getOption('MTurkR.keypair'), print = getOption('MTurkR.print'),
    browser = getOption('MTurkR.browser'), log.requests = getOption('MTurkR.log'),
    sandbox = getOption('MTurkR.sandbox'), validation.test = getOption('MTurkR.test')) {
    if(!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    } else
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "AssignQualification"
    if(is.factor(qual))
        qual <- as.character(qual)
    if(is.factor(workers))
        workers <- as.character(workers)
    if(is.factor(value))
        value <- as.character(value)
    for(i in 1:length(value)) {
        if(is.null(value[i]) || is.na(value[i]) || value[i]=='') {
            warning("Value ",i," not assigned; value assumed to be 1")
            value[i] <- "1"
        }
        else if(is.na(as.numeric(value[i])))
            stop("value ",i," is not or cannot be coerced to numeric")
    }
    worker <- NULL
    batch <- function(worker, value) {
        GETparameters <- paste(    "&QualificationTypeId=", qual, 
                                "&WorkerId=", worker,
                                "&IntegerValue=", value,
                                "&SendNotification=", tolower(notify), sep = "")
        auth <- authenticate(operation, secret)
        if(browser == TRUE) {
            request <- request(keyid, auth$operation, auth$signature, 
                auth$timestamp, GETparameters, browser = browser, 
                sandbox = sandbox, validation.test = validation.test)
            if(validation.test)
                return(invisible(request))
        }
        else {
            request = request(keyid, auth$operation, auth$signature, 
                auth$timestamp, GETparameters, log.requests = log.requests, 
                sandbox = sandbox, validation.test = validation.test)
            if(validation.test)
                return(invisible(request))
            if(print == TRUE) {
                if(request$valid == TRUE) {
                    message("Qualification (", qual, ") Assigned to worker ", worker)
                    return(invisible(request))
                }
                else if(request$valid == FALSE) {
                    warning("Invalid Request for worker ",worker)
                    return(request)
                }
            }
            else
                return(invisible(request))
        }
    }
    if (!is.null(name)) {
        if (!is.null(qual)) 
            stop("Cannot specify QualificationTypeId and properties of new QualificationType")
        if (is.null(description)) 
            stop("No Description provided for QualificationType")
        if (is.null(status) || !status == "Active") {
            warning("QualificationTypeStatus set to 'Active'")
            status <- "active"
        }
        type <- CreateQualificationType(keypair, name = name, 
            description = description, keywords = keywords, status = status, 
            retry.delay = retry.delay, test = test, answerkey = answerkey, 
            test.duration = test.duration, auto = auto, auto.value = auto.value, 
            log.requests = log.requests, sandbox = sandbox)
        if (type$valid == TRUE) 
            qual <- type$QualificationTypeId
        else
            stop("Could not create QualificationType")
    }
    qual.value <- value
    Qualifications <- setNames(data.frame(matrix(ncol=5, nrow=length(workers))),
        c("WorkerId", "QualificationTypeId", "Value", "Notified", "Valid"))
    if(is.null(conditional.statistic)) {
        for(i in 1:length(workers)) {
            x <- batch(workers[i], value)
            if(validation.test)
                return(invisible(x))
            Qualifications[i, ] = c(workers[i], value, qual, notify, x$valid)
        }
    }
    else {
        if(validation.test){
            warning("validation.test not available for conditional qualification assignment")
            return(invisible(NULL))
        }
        if(is.null(conditional.comparator)) 
            stop("No comparator specified for conditional")
        value.integer <- c("NumberAssignmentsApproved", "NumberAssignmentsRejected", 
            "NumberKnownAnswersCorrect", "NumberKnownAnswersIncorrect", 
            "NumberKnownAnswersEvaluated", "NumberPluralityAnswersCorrect", 
            "NumberPluralityAnswersIncorrect", "NumberPluralityAnswersEvaluated", 
            "NumberHITsCompleted", "NumberHITsAssignable", "NumberHITsReviewable")
        value.double <- c("PercentAssignmentsApproved", "PercentAssignmentsRejected", 
            "PercentKnownAnswersCorrect", "PercentPluralityAnswersCorrect")
        if(!conditional.statistic %in% value.integer & !conditional.statistic %in% 
            value.double) 
            stop("Conditional Statistic not valid")
        else {
            if(conditional.comparator == "<") 
                conditional.comparator = "LessThan"
            else if(conditional.comparator == "<=") 
                conditional.comparator = "LessThanOrEqualTo"
            else if(conditional.comparator == ">") 
                conditional.comparator = "GreaterThan"
            else if(conditional.comparator == ">=") 
                conditional.comparator = "GreaterThanOrEqualTo"
            else if(conditional.comparator == "==") 
                conditional.comparator = "EqualTo"
            else if(conditional.comparator == "!=") 
                conditional.comparator = "NotEqualTo"
            if(!conditional.comparator %in% c(    "LessThan", "LessThanOrEqualTo", "GreaterThan",
                                                "GreaterThanOrEqualTo", "EqualTo", "NotEqualTo")) 
                stop("Inappropriate comparator specified for conditional")
        }
        if(is.null(conditional.period) || !conditional.period %in% 
            c("LifeTime", "ThirtyDays", "SevenDays", "OneDay")) 
            stop("Inappropriate or no period specified for conditional")
        if(is.null(conditional.value)) 
            stop("Statistic value not specified for conditional")
        conditional.value <- as.numeric(conditional.value)
        if(is.na(conditional.value)) 
            stop("Conditional value is non-numeric")
        for(i in 1:length(workers)) {
            x <- GetWorkerStatistic(keypair, worker, conditional.statistic, 
                conditional.period, log.requests = log.requests, 
                sandbox = sandbox)
            if(set.statistic.as.value == TRUE) 
                value <- x$value
            if(conditional.comparator == "LessThan") {
                if(as.numeric(x$value) < conditional.value) 
                    temp <- batch(workers[i], value)
            }
            else if(conditional.comparator == "LessThanOrEqualTo") {
                if(as.numeric(x$value) <= conditional.value) 
                    temp <- batch(workers[i], value)
            }
            else if(conditional.comparator == "GreaterThan") {
                if(as.numeric(x$value) > conditional.value) 
                    temp <- batch(workers[i], value)
            }
            else if(conditional.comparator == "GreaterThanOrEqualTo") {
                if(as.numeric(x$value) >= conditional.value) 
                    temp <- batch(workers[i], value)
            }
            else if(conditional.comparator == "EqualTo") {
                if(as.numeric(x$value) == conditional.value) 
                    temp <- batch(workers[i], value)
            }
            else if(conditional.comparator == "NotEqualTo") {
                if(as.numeric(x$value) != conditional.value) 
                    temp <- batch(workers[i], value)
            }
            Qualifications[i, ] = c(workers[i], value, qual, notify, x$valid)
            value <- qual.value
        }
    }
    Qualifications$Valid <- factor(Qualifications$Valid, levels=c('TRUE','FALSE'))
    return(Qualifications)
}
