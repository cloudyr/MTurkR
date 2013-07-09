SendTestEventNotification <-
notificationtest <-
function (notification, test.event.type = "HITExpired", keypair = credentials(), 
    print = TRUE, browser = FALSE, log.requests = TRUE, sandbox = FALSE, validation.test = FALSE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "SendTestEventNotificaiton"
    if (!test.event.type %in% c("AssignmentAccepted", "AssignmentAbandoned", 
        "AssignmentReturned", "AssignmentSubmitted", "HITReviewable", "HITExpired")) 
        stop("Inappropriate TestEventType specified")
    GETparameters <- notification
    GETparameters <- paste(GETparameters, "&TestEventType=", 
        test.event.type, sep = "")
    TestEvent <- data.frame(matrix(ncol = 3))
    names(TestEvent) <- c("TestEventType", "Notification", "Valid")
    auth <- authenticate(operation, secret)
    if (browser == TRUE) {
        request <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, browser = browser, 
            sandbox = sandbox, validation.test = validation.test)
		if(validation.test)
			invisible(request)
    }
    else {
        request <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, log.requests = log.requests, 
            sandbox = sandbox, validation.test = validation.test)
        if(validation.test)
			invisible(request)
		TestEvent[1, ] <- c(test.event.type, notification, request$valid)
        if (request$valid == TRUE) {
            if (print == TRUE) 
                message("TestEventNotification ", test.event.type," Sent")
            invisible(TestEvent)
        }
        else if (request$valid == FALSE) {
            if (print == TRUE) 
                warning("Invalid Request")
            invisible(TestEvent)
        }
    }
}
