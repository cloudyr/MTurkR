assignqual <-
AssignQualification <-
AssignQualifications <-
function (qual = NULL, workers, value = "1", notify = FALSE, name = NULL, 
    description = NULL, keywords = NULL, status = NULL, retry.delay = NULL, 
    test = NULL, answerkey = NULL, test.duration = NULL, auto = NULL, 
    auto.value = NULL, conditional.statistic = NULL, conditional.comparator = NULL, 
    conditional.value = NULL, conditional.period = NULL, set.statistic.as.value = FALSE, 
    verbose = getOption('MTurkR.verbose', TRUE), ...) {
    operation <- "AssignQualification"
    if (!is.null(qual) & is.factor(qual)) {
        qual <- as.character(qual)
    }
    if (is.factor(workers)) {
        workers <- as.character(workers)
    }
    if (is.factor(value)) {
        value <- as.character(value)
    }
    for (i in 1:length(value)) {
        if (is.null(value[i]) || is.na(value[i]) || value[i]=='') {
            warning("Value ",i," not assigned; value assumed to be 1")
            value[i] <- "1"
        } else if(is.na(as.numeric(value[i]))) {
            stop("value ",i," is not or cannot be coerced to numeric")
        }
    }
    worker <- NULL
    batch <- function(worker, value) {
        GETparameters <- paste("&QualificationTypeId=", qual, 
                               "&WorkerId=", worker,
                               "&IntegerValue=", value,
                               "&SendNotification=", tolower(notify), sep = "")        
        request <- request(operation, GETparameters = GETparameters, ...)
        if (is.null(request$valid)) {
            return(request)
        }
        if (verbose) {
            if (request$valid == TRUE) {
                message("Qualification (", qual, ") Assigned to worker ", worker)
                return(invisible(request))
            } else if(request$valid == FALSE) {
                warning("Invalid Request for worker ",worker)
                return(request)
            }
        } else {
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
        type <- CreateQualificationType(name = name, description = description,
            keywords = keywords, status = status, retry.delay = retry.delay,
            test = test, answerkey = answerkey, test.duration = test.duration, 
            auto = auto, auto.value = auto.value, ...)
        qual <- as.character(type$QualificationTypeId)
    }
    qual.value <- value
    Qualifications <- emptydf(length(workers), 5, c("WorkerId", "QualificationTypeId", "Value", "Notified", "Valid"))
    if (is.null(conditional.statistic)) {
        for (i in 1:length(workers)) {
            x <- batch(workers[i], value)
            if (is.null(x$valid)) {
                return(request)
            }
            Qualifications[i, ] = c(workers[i], qual, value, notify, x$valid)
        }
    } else {
        if (is.null(conditional.comparator)) {
            stop("No comparator specified for conditional")
        }
        value.integer <- c("NumberAssignmentsApproved", "NumberAssignmentsRejected", 
            "NumberKnownAnswersCorrect", "NumberKnownAnswersIncorrect", 
            "NumberKnownAnswersEvaluated", "NumberPluralityAnswersCorrect", 
            "NumberPluralityAnswersIncorrect", "NumberPluralityAnswersEvaluated", 
            "NumberHITsCompleted", "NumberHITsAssignable", "NumberHITsReviewable")
        value.double <- c("PercentAssignmentsApproved", "PercentAssignmentsRejected", 
            "PercentKnownAnswersCorrect", "PercentPluralityAnswersCorrect")
        if (!conditional.statistic %in% value.integer & !conditional.statistic %in% value.double) {
            stop("Conditional Statistic not valid")
        } else {
            conditional.comparator <- 
              switch(conditional.comparator,
                     "<" = "LessThan",
                     "<=" = "LessThanOrEqualTo",
                     ">" = "GreaterThan",
                     ">=" = "GreaterThanOrEqualTo",
                     "==" = "EqualTo",
                     "!=" = "NotEqualTo")
            if (!conditional.comparator %in% c("LessThan", "LessThanOrEqualTo", "GreaterThan",
                                              "GreaterThanOrEqualTo", "EqualTo", "NotEqualTo")) {
                stop("Inappropriate comparator specified for conditional")
            }
        }
        if (is.null(conditional.period) || !conditional.period %in% 
            c("LifeToDate", "ThirtyDays", "SevenDays", "OneDay")) {
            stop("Inappropriate or no period specified for conditional")
        }
        if (is.null(conditional.value)) {
            stop("Statistic value not specified for conditional")
        }
        conditional.value <- as.numeric(conditional.value)
        if (is.na(conditional.value)) {
            stop("Conditional value is non-numeric")
        }
        for (i in 1:length(workers)) {
            x <- GetWorkerStatistic(workers[i], conditional.statistic, 
                                    conditional.period, ...)
            if (set.statistic.as.value == TRUE) {
                value <- x
            }
            if (conditional.comparator == "LessThan") {
                if (as.numeric(x$value) < conditional.value) {
                    temp <- batch(workers[i], value)
                }
            } else if (conditional.comparator == "LessThanOrEqualTo") {
                if (as.numeric(x$value) <= conditional.value) {
                    temp <- batch(workers[i], value)
                }
            } else if (conditional.comparator == "GreaterThan") {
                if (as.numeric(x$value) > conditional.value) {
                    temp <- batch(workers[i], value)
                }
            } else if (conditional.comparator == "GreaterThanOrEqualTo") {
                if (as.numeric(x$value) >= conditional.value) {
                    temp <- batch(workers[i], value)
                }
            } else if (conditional.comparator == "EqualTo") {
                if (as.numeric(x$value) == conditional.value) {
                    temp <- batch(workers[i], value)
                }
            } else if (conditional.comparator == "NotEqualTo") {
                if (as.numeric(x$value) != conditional.value) {
                    temp <- batch(workers[i], value)
                }
            }
            Qualifications[i, ] = c(workers[i], value, qual, notify, x$valid)
            value <- qual.value
        }
    }
    Qualifications$Valid <- factor(Qualifications$Valid, levels=c('TRUE','FALSE'))
    return(Qualifications)
}
