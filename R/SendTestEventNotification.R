SendTestEventNotification <-
notificationtest <-
function (notification, test.event.type = "HITExpired",
    verbose = getOption('MTurkR.verbose', TRUE), ...) {
    operation <- "SendTestEventNotification"
    validopts <- c("AssignmentAccepted", "AssignmentAbandoned", "AssignmentReturned", 
                   "AssignmentSubmitted", "HITReviewable", "HITExpired", "Ping")
    if (!test.event.type %in% validopts) {
        stop(paste0("Inappropriate TestEventType specified. Must be one of: ", paste(validopts, sep = ", ")))
    }
    GETparameters <- notification
    GETparameters <- paste(GETparameters, "&TestEventType=", test.event.type, sep = "")
    TestEvent <- emptydf(1, 3, c("TestEventType", "Notification", "Valid"))
    request <- request(operation, GETparameters = GETparameters, ...)
    if (is.null(request$valid)) {
        return(request)
    }
    TestEvent[1, ] <- c(test.event.type, notification, request$valid)
    if (request$valid & verbose) {
        message("TestEventNotification ", test.event.type," Sent")
    } else if(!request$valid & verbose) {
        warning("Invalid Request")
    }
    TestEvent$Valid <- factor(TestEvent$Valid, levels=c('TRUE','FALSE'))
    return(TestEvent)
}
