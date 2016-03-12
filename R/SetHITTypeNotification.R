SetHITTypeNotification <-
setnotification <-
function (hit.type, notification = NULL, active = NULL, 
    verbose = getOption('MTurkR.verbose', TRUE), ...){
    if (is.null(notification) & is.null(active)) {
        stop("Must specify either 'notification' and/or 'active'")
    }
    operation <- "SetHITTypeNotification"
    GETparameters <- paste("&HITTypeId=", hit.type, sep = "")
    if (is.null(notification) & is.null(active)) {
        stop("Must specify 'notification' and/or 'active'")
    }
    if (!is.null(notification)) {
        GETparameters <- paste(GETparameters, notification, sep = "")
    }
    if (!is.null(active) && active == TRUE) {
        GETparameters <- paste(GETparameters, "&Active=true", sep = "")
    }
    if (!is.null(active) && active == FALSE) {
        GETparameters <- paste(GETparameters, "&Active=false", sep = "")
    }
    Notification <- emptydf(1, 4, c("HITTypeId", "Notification", "Active", "Valid"))
    request <- request(operation, GETparameters = GETparameters, ...)
    if (is.null(request$valid)) {
        return(request)
    }
    Notification[1, ] <- c(hit.type, notification, active, request$valid)
    if (request$valid) {
        if (verbose) {
            if (!is.null(notification) & is.null(active)) {
                message("HITTypeNotification for ", hit.type, " Created")
            } else if (!is.null(notification) & !is.null(active) && active == TRUE) {
                message("HITTypeNotification ", hit.type, " Created & Active")
            } else if (!is.null(notification) & !is.null(active) && active == FALSE) {
                message("HITTypeNotification ", hit.type, " Created & Inactive")
            } else if (is.null(notification) & !is.null(active) && active == TRUE) {
                message("HITTypeNotification ", hit.type, " Active")
            } else if (is.null(notification) & !is.null(active) && active == FALSE) {
                message("HITTypeNotification ", hit.type, " Inactive")
            } 
        }
    } else if (!request$valid & verbose) {
        warning("Invalid Request")
    }
    Notification$Valid <- factor(Notification$Valid, levels=c('TRUE','FALSE'))
    return(Notification)
}
