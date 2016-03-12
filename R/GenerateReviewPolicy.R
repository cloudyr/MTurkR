GenerateHITReviewPolicy <-
function(...) {
    l <- list(...)
    h <- list()
    
    if (("ExtendIfHITAgreementScoreIsLessThan" %in% names(l)) &&
       ((!"ExtendMaximumAssignments" %in% names(l)) | 
        (!"ExtendMinimumTimeInSeconds" %in% names(l)) ) ) {
        stop(paste0("ExtendMaximumAssignments and ExtendMinimumTimeInSeconds required",
             " if using ExtendIfHITAgreementScoreIsLessThan"))
    }
    if("ExtendIfHITAgreementScoreIsLessThan" %in% names(l) &&
       (as.numeric(l$ExtendIfHITAgreementScoreIsLessThan) > 100 | 
        as.numeric(l$ExtendIfHITAgreementScoreIsLessThan) < 1)) {
        stop("ExtendIfHITAgreementScoreIsLessThan must be between 0 and 100")
    }
    # if("ExtendMaximumAssignments" %in% names(l) &&
       # (as.numeric(l$ExtendMaximumAssignments) > 25 | 
        # as.numeric(l$ExtendMaximumAssignments) < 2)) {
        # stop("ExtendMaximumAssignments must be between 2 and 25")
    # }
    if ("ExtendMinimumTimeInSeconds" %in% names(l) &&
       (as.numeric(l$ExtendMinimumTimeInSeconds) > 31536000 | 
        as.numeric(l$ExtendMinimumTimeInSeconds) < 3600)) {
        stop("ExtendMinimumTimeInSeconds must be between one hour and one year")
    }
    if ("DisregardAssignmentIfRejected" %in% names(l) &&
       (!as.character(l$DisregardAssignmentIfRejected) %in% c("TRUE","FALSE"))) {
        stop("DisregardAssignmentIfRejected must be TRUE or FALSE")
    }
    
    h$PolicyName <- curl_escape("SimplePlurality/2011-09-01")
    
    if (length(l$QuestionIds) > 15) {
        stop("Max number of 'QuestionIds' is 15")
    }
    h[["Parameter.1.Key"]] <- "QuestionIds"
    for (i in 1:length(l$QuestionIds)) {
        h[[paste0("Parameter.1.Value.",i)]] <- l$QuestionIds[i]
    }
    l$QuestionIds <- NULL
    for (i in 1:length(l)) {        
        if (!names(l)[i] %in% c("QuestionAgreementThreshold", 
            "DisregardAssignmentIfRejected", "DisregardAssignmentIfKnownAnswerScoreIsLessThan", 
            "ExtendIfHITAgreementScoreIsLessThan", "ExtendMaximumAssignments", 
            "ExtendMinimumTimeInSeconds", "ApproveIfWorkerAgreementScoreIsAtLeast", 
            "RejectIfWorkerAgreementScoreIsLessThan", "RejectReason")) {
            stop(paste0("Inappropriate HIT ReviewPolicy Parameter: ",names(l)[i]))
        }
        h[[paste0("Parameter.", i+1, ".Key")]] <- names(l)[i]
        if (names(l)[i] == "DisregardAssignmentIfRejected") {
            h[[paste0("Parameter.", i+1, ".Value.1")]] <- unname(substring(as.character(l[i]), 1,1))
        } else {
            h[[paste0("Parameter.", i+1, ".Value.1")]] <- unname(l[i])
        }
    }    
    names(h) <- paste0("HITReviewPolicy.", names(h))
    hitstring <- paste0("&", paste(names(h), unlist(h), sep="=", collapse = "&"))
    return(hitstring)
}

GenerateAssignmentReviewPolicy <-
function(...) {
    l <- list(...)
    
    if ("ApproveIfKnownAnswerScoreIsAtLeast" %in% names(l) &&
       (as.numeric(l$ApproveIfKnownAnswerScoreIsAtLeast) > 101 | 
        as.numeric(l$ApproveIfKnownAnswerScoreIsAtLeast) < 1)) {
        stop("ApproveIfKnownAnswerScoreIsAtLeast must be between 0 and 101")
    }
    if ("RejectIfKnownAnswerScoreIsLessThan" %in% names(l) &&
       (as.numeric(l$RejectIfKnownAnswerScoreIsLessThan) > 101 | 
        as.numeric(l$RejectIfKnownAnswerScoreIsLessThan) < 1)) {
        stop("RejectIfKnownAnswerScoreIsLessThan must be between 0 and 101")
    }
    if ("ExtendIfKnownAnswerScoreIsLessThan" %in% names(l) &&
       (as.numeric(l$ExtendIfKnownAnswerScoreIsLessThan) > 101 | 
        as.numeric(l$ExtendIfKnownAnswerScoreIsLessThan) < 1)) {
        stop("ExtendIfKnownAnswerScoreIsLessThan must be between 0 and 101")
    }
    if ("ExtendMaximumAssignments" %in% names(l) &&
       (as.numeric(l$ExtendMaximumAssignments) > 25 | 
        as.numeric(l$ExtendMaximumAssignments) < 2)) {
        stop("ExtendMaximumAssignments must be between 2 and 25")
    }
    if ("ExtendMinimumTimeInSeconds" %in% names(l) &&
       (as.numeric(l$ExtendMinimumTimeInSeconds) > 31536000 | 
        as.numeric(l$ExtendMinimumTimeInSeconds) < 3600)) {
        stop("ExtendMinimumTimeInSeconds must be between one hour and one year")
    }
    
    a <- list()
    a$PolicyName <- curl_escape("ScoreMyKnownAnswers/2011-09-01")
    
    a[["Parameter.1.Key"]] = "AnswerKey"
    ak <- l$AnswerKey
    l$AnswerKey <- NULL
    for (i in 1:length(ak)) {
        a[[paste0("Parameter.1.MapEntry.", i, ".Key")]] <- names(ak)[i]
        a[[paste0("Parameter.1.MapEntry.", i, ".Value")]] <- ak[i]
    }
    for (i in 1:length(l)) {
        if (!names(l)[i] %in% c("ApproveIfKnownAnswerScoreIsAtLeast", 
            "ApproveReason", "RejectIfKnownAnswerScoreIsLessThan", 
            "RejectReason", "ExtendIfKnownAnswerScoreIsLessThan", 
            "ExtendMaximumAssignments", "ExtendMinimumTimeInSeconds")) {
            stop("Inappropriate Assignment ReviewPolicy Parameter: ", names(l)[i])
        }
        a[[paste0("Parameter.", i+1, ".Key")]] <- names(l)[i]
        a[[paste0("Parameter.", i+1, ".Value.1")]] <- unname(l[i])
    }
    names(a) <- paste0("AssignmentReviewPolicy.", names(a))
    assignstring <- paste0("&", paste(names(a), unlist(a), sep="=", collapse = "&"))
    return(assignstring)
}
