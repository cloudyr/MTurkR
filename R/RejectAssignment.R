RejectAssignment <-
RejectAssignments <-
reject <-
function (assignments, feedback = NULL, verbose = getOption('MTurkR.verbose', TRUE), ...){
    operation <- "RejectAssignment"
    if (is.factor(assignments)) {
        assignments <- as.character(assignments)
    }
    if (!is.null(feedback)) {
        if (is.factor(feedback)) {
            feedback <- as.character(feedback)
        }
        for (i in 1:length(feedback)) {
            if (!is.null(feedback[i]) && nchar(curl_escape(feedback[i])) > 1024) {
                warning("Feedback ", i, " is too long (1024 char max)")
            }
        }
        if (length(feedback) == 1) {
            feedback <- rep(feedback[1], length(assignments))
        } else if(!length(feedback) == length(assignments)) {
            stop("Number of feedback is not 1 nor length(assignments)")
        }
    }
    Assignments <- emptydf(length(assignments), 3, c("AssignmentId", "Feedback", "Valid"))
    for (i in 1:length(assignments)) {
        GETparameters <- paste("&AssignmentId=", assignments[i], sep = "")
        if (!is.null(feedback[i])) {
            GETparameters <- paste(GETparameters, "&RequesterFeedback=", 
                                   curl_escape(feedback[i]), sep = "")        
        }
        request <- request(operation, GETparameters = GETparameters, ...)
        if (is.null(request$valid)) {
            return(request)
        }
        if (!is.null(feedback)) {
            Assignments[i, ] <- c(assignments[i], feedback[i], request$valid)
        } else {
            Assignments[i, ] <- c(assignments[i], "", request$valid)
        }
        if (request$valid == TRUE) {
            if (verbose) {
                message(i, ": Assignment (", assignments[i], ") Rejected")
            }
        }
        if (request$valid == FALSE) {
            if (verbose) {
                warning(i, ": Invalid request for assignment ",assignments[i])
            }
        }
    }
    Assignments$Valid <- factor(Assignments$Valid, levels=c('TRUE','FALSE'))
    return(Assignments)
}
