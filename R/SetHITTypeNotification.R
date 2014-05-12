SetHITTypeNotification <-
setnotification <-
function (hit.type, notification = NULL, active = NULL, 
    keypair = getOption('MTurkR.keypair'), 
    print = getOption('MTurkR.print'),
    log.requests = getOption('MTurkR.log'), sandbox = getOption('MTurkR.sandbox'),
    validation.test = getOption('MTurkR.test')){
    if(is.null(notification) & is.null(active)) 
        stop("Must specify either 'notification' and/or 'active'")
    if(is.null(keypair))
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "SetHITTypeNotification"
    GETparameters <- paste("&HITTypeId=", hit.type, sep = "")
    if(is.null(notification) & is.null(active)) 
        stop("Must specify 'notification' and/or 'active'")
    if(!is.null(notification)) 
        GETparameters <- paste(GETparameters, notification, sep = "")
    if(!is.null(active) && active == TRUE) 
        GETparameters <- paste(GETparameters, "&Active=true", sep = "")
    if(!is.null(active) && active == FALSE) 
        GETparameters <- paste(GETparameters, "&Active=false", sep = "")
    Notification <- setNames(data.frame(matrix(ncol=4, nrow=1)),
                    c("HITTypeId", "Notification", "Active", "Valid"))
    request <- request(keypair[1], operation, secret=keypair[2],
            GETparameters = GETparameters, log.requests = log.requests, 
        sandbox = sandbox, validation.test = validation.test)
    if(validation.test)
        return(invisible(request))
    Notification[1, ] <- c(hit.type, notification, active, request$valid)
    if(request$valid == TRUE) {
        if(print == TRUE) {
            if(!is.null(notification) & is.null(active)) 
                message("HITTypeNotification for ", hit.type, " Created")
            else if(!is.null(notification) & !is.null(active) && active == TRUE) 
                message("HITTypeNotification ", hit.type, " Created & Active")
            else if(!is.null(notification) & !is.null(active) && active == FALSE) 
                message("HITTypeNotification ", hit.type, " Created & Inactive")
            else if(is.null(notification) & !is.null(active) && active == TRUE) 
                message("HITTypeNotification ", hit.type, " Active")
            else if(is.null(notification) & !is.null(active) && active == FALSE) 
                message("HITTypeNotification ", hit.type, " Inactive")
        }
    }
    else if(request$valid == FALSE & print == TRUE)
        warning("Invalid Request")
    Notification$Valid <- factor(Notification$Valid, levels=c('TRUE','FALSE'))
    return(Notification)
}
