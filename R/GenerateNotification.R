GenerateNotification <-
function (destination, transport = "Email", event.type, version = "2006-05-05", 
    event.number = "1") {
    if (is.null(destination)) {
        stop("No Destination specified; must be Email address or URL")
    }
    if (is.null(transport)) {
        stop("No Transport specified")
    } else if(!transport %in% c("Email", "SQS")) {
        stop("Transport must be 'Email' or 'SQS'")
    }
    EVENTS <- c("AssignmentAccepted", "AssignmentAbandoned", "AssignmentReturned",
                "AssignmentSubmitted", "HITReviewable", "HITExpired", "Ping")
    if (is.null(event.type)) {
        stop("No EventType specified")
    } else if(length(event.type) == 1) {
        if (!event.type %in% EVENTS) {
            stop("Inappropriate EventType specified")
        }
    } else if (length(event.type) > 1) {
        for (i in 1:length(event.type)) {
            if (!event.type[i] %in% EVENTS) {
                stop(paste("Inappropriate EventType specified for EventType ", i, sep = ""))
            }
        }
    }
    x <- paste("&Notification.", event.number, ".Destination=", 
               destination, "&Notification.", event.number, ".Transport=", 
               transport, "&Notification.", event.number, ".Version=", 
               version, sep = "")
    if (length(event.type) == 1) {
        x <- paste(x, "&Notification.", event.number, ".EventType=", event.type, sep = "")
    } else {
        for (i in 1:length(event.type)) {
            x <- paste(x, "&Notification.", event.number, ".EventType=", event.type[i], sep = "")
        }
    }
    return(x)
}
