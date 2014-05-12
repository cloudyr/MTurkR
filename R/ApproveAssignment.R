approve <-
ApproveAssignment <-
ApproveAssignments <-
function (assignments, feedback = NULL, rejected = FALSE,
    keypair = getOption('MTurkR.keypair'), 
    print = getOption('MTurkR.print'), browser = getOption('MTurkR.browser'),
    log.requests = getOption('MTurkR.log'), sandbox = getOption('MTurkR.sandbox'),
    validation.test = getOption('MTurkR.test')) {
    if(is.null(keypair))
        stop("No keypair provided or 'credentials' object not stored")
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
        if(browser == TRUE) {
            request <- request(keypair[1], operation, secret=keypair[2],
                GETparameters = GETparameters, browser = browser, 
                sandbox = sandbox, validation.test = validation.test)
            if(validation.test)
                invisible(request)
        }
        else {
            request <- request(keypair[1], operation, secret=keypair[2],
                GETparameters = GETparameters, log.requests = log.requests, 
                sandbox = sandbox, validation.test = validation.test)
            if(validation.test)
                return(invisible(request))
            if(print == TRUE) {
                if (request$valid == TRUE) 
                    message("Assignment ", assignment, " Approved", sep = "")
                else if (request$valid == FALSE) 
                    warning("Invalid Request for ", assignment)
            }
            return(request)
        }
    }
    Assignments <- setNames(data.frame(matrix(nrow=length(assignments), ncol=3)),
                    c("AssignmentId", "Feedback", "Valid"))
    for(i in 1:length(assignments)) {
        x <- batch(assignments[i], feedback[i])
        if(validation.test)
            return(invisible(x))
        if (!is.null(feedback)) 
            Assignments[i, ] <- c(assignments[i], feedback[i], x$valid)
        else
            Assignments[i, ] <- c(assignments[i], "", x$valid)
    }
    Assignments$Valid <- factor(Assignments$Valid, levels=c('TRUE','FALSE'))
    if(print == TRUE) 
        message(sum(x$valid), " Assignments Approved")
    return(Assignments)
}
