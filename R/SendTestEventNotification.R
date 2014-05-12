SendTestEventNotification <-
notificationtest <-
function (notification, test.event.type = "HITExpired",
    keypair = getOption('MTurkR.keypair'), 
    print = getOption('MTurkR.print'),
    log.requests = getOption('MTurkR.log'), sandbox = getOption('MTurkR.sandbox'),
    validation.test = getOption('MTurkR.test')) {
    if(is.null(keypair))
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "SendTestEventNotification"
    if(!test.event.type %in% c("AssignmentAccepted", "AssignmentAbandoned", 
        "AssignmentReturned", "AssignmentSubmitted", "HITReviewable", "HITExpired")) 
        stop("Inappropriate TestEventType specified")
    GETparameters <- notification
    GETparameters <- paste(GETparameters, "&TestEventType=", test.event.type, sep = "")
    TestEvent <- setNames(data.frame(matrix(ncol=3, nrow=1)),
                    c("TestEventType", "Notification", "Valid"))
    request <- request(keypair[1], operation, secret=keypair[2],
            GETparameters = GETparameters, log.requests = log.requests, 
        sandbox = sandbox, validation.test = validation.test)
    if(validation.test)
        return(invisible(request))
    TestEvent[1, ] <- c(test.event.type, notification, request$valid)
    if(request$valid == TRUE & print == TRUE)
            message("TestEventNotification ", test.event.type," Sent")
    else if(request$valid == FALSE & print == TRUE)
        warning("Invalid Request")
    TestEvent$Valid <- factor(TestEvent$Valid, levels=c('TRUE','FALSE'))
    return(TestEvent)
}
