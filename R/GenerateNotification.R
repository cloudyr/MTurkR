GenerateNotification <-
function (destination, transport = "Email", event.type, version = "2006-05-05", 
    event.number = "1", format = "REST") 
{
    if (is.null(destination)) 
        stop("No Destination specified; must be Email address or URL")
    if (is.null(transport)) 
        stop("No Transport specified")
    else if (!transport %in% c("Email", "REST", "SOAP")) 
        stop("Transport must be 'Email' | 'REST' (GET) | 'SOAP' (XML)")
    if (is.null(event.type)) 
        stop("No EventType specified")
    else if (length(event.type) == 1) {
        if (!event.type %in% c("AssignmentAccepted", "AssignmentAbandoned", 
            "AssignmentReturned", "AssignmentSubmitted", "HITReviewable", 
            "HITExpired")) 
            stop("Inappropriate EventType specified")
    }
    else if (length(event.type) > 1) {
        for (i in 1:length(event.type)) {
            if (!event.type[i] %in% c("AssignmentAccepted", "AssignmentAbandoned", 
                "AssignmentReturned", "AssignmentSubmitted", 
                "HITReviewable", "HITExpired")) 
                stop(paste("Inappropriate EventType specified for EventType ", 
                  i, sep = ""))
        }
    }
    if (format == "get" | format == "Get" | format == "GET" | 
        format == "rest" | format == "REST") {
        x <- paste("&Notification.", event.number, ".Destination=", 
            destination, "&Notification.", event.number, ".Transport=", 
            transport, "&Notification.", event.number, ".Version=", 
            version, sep = "")
        if (length(event.type) == 1) {
            x <- paste(x, "&Notification.", event.number, ".EventType=", 
                event.type, sep = "")
        }
        else {
            for (i in 1:length(event.type)) {
                x <- paste(x, "&Notification.", event.number, 
                  ".EventType=", event.type[i], sep = "")
            }
        }
    }
    else if (format == "xml" | format == "Xml" | format == "XML" | 
        format == "soap" | format == "SOAP") {
        stop("Only REST/GET requests currently supported")
    }
    else stop("Inappropriate format specified; Only REST/GET requests currently supported")
    return(x)
}
