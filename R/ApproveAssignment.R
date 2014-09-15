approve <-
ApproveAssignment <-
ApproveAssignments <-
function (assignments, feedback = NULL, rejected = FALSE,
    verbose = getOption('MTurkR.verbose', TRUE), ...) {
    # temporary check for `print` argument (remove after v1.0)
    if('print' %in% names(list(...)) && is.null(verbose))
        verbose <- list(...)$print
    if(is.factor(assignments))
        assignments <- as.character(assignments)
    if(rejected == TRUE) 
        operation <- "ApproveRejectedAssignment"
    else
        operation <- "ApproveAssignment"
    if(!is.null(feedback)) {
        if(is.factor(feedback))
            feedback <- as.character(feedback)
        for(i in 1:length(feedback)) {
            if (!is.null(feedback[i]) && nchar(curlEscape(feedback[i])) > 1024) 
                warning("Feedback ", i, " is too long (1024 char max)")
        }
        if(length(feedback) == 1) 
            feedback <- rep(feedback[1], length(assignments))
        else if(!length(feedback) == length(assignments)) 
            stop("Number of feedback is not 1 nor length(assignments)")
    }
    batch <- function(assignment, feedback.batch = NULL) {
        GETparameters <- paste("&AssignmentId=", assignment, sep = "")
        if (!is.null(feedback.batch)) {
            GETparameters <- paste(GETparameters, "&RequesterFeedback=", 
                curlEscape(feedback.batch), sep = "")
        }
        request <- request(operation, GETparameters = GETparameters, ...)
        if(is.null(request$valid))
            return(request)
        if(verbose) {
            if(request$valid) 
                message("Assignment ", assignment, " Approved", sep = "")
            else
                warning("Invalid Request for ", assignment)
        }
        return(request)
    }
    Assignments <- setNames(data.frame(matrix(nrow=length(assignments), ncol=3)),
                    c("AssignmentId", "Feedback", "Valid"))
    for(i in 1:length(assignments)) {
        x <- batch(assignments[i], feedback[i])
        if (!is.null(feedback)) 
            Assignments[i, ] <- c(assignments[i], feedback[i], x$valid)
        else
            Assignments[i, ] <- c(assignments[i], "", x$valid)
    }
    Assignments$Valid <- factor(Assignments$Valid, levels=c('TRUE','FALSE'))
    if(verbose) 
        message(sum(x$valid), " Assignments Approved")
    return(Assignments)
}
