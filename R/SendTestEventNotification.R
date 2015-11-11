SendTestEventNotification <-
notificationtest <-
function (notification, test.event.type = "HITExpired",
    verbose = getOption('MTurkR.verbose', TRUE), ...) {
    operation <- "SendTestEventNotification"
    if(!test.event.type %in% c("AssignmentAccepted", "AssignmentAbandoned", 
        "AssignmentReturned", "AssignmentSubmitted", "HITReviewable", "HITExpired", "Ping")) 
        stop("Inappropriate TestEventType specified")
    GETparameters <- notification
    GETparameters <- paste(GETparameters, "&TestEventType=", test.event.type, sep = "")
    TestEvent <- setNames(data.frame(matrix(ncol=3, nrow=1)),
                    c("TestEventType", "Notification", "Valid"))
    request <- request(operation, GETparameters = GETparameters, ...)
    if(is.null(request$valid))
        return(request)
    TestEvent[1, ] <- c(test.event.type, notification, request$valid)
    if(request$valid & verbose)
            message("TestEventNotification ", test.event.type," Sent")
    else if(!request$valid & verbose)
        warning("Invalid Request")
    TestEvent$Valid <- factor(TestEvent$Valid, levels=c('TRUE','FALSE'))
    return(TestEvent)
}
