SetHITTypeNotification <-
setnotification <-
function (hit.type, notification = NULL, active = NULL, keypair = credentials(), 
    print = TRUE, browser = FALSE, log.requests = TRUE, sandbox = FALSE) 
{
    if (is.null(notification) & is.null(active)) 
        stop("Must specify either 'notification' and/or 'active'")
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "SetHITTypeNotification"
    GETparameters <- paste("&HITTypeId=", hit.type, sep = "")
    if (is.null(notification) & is.null(active)) 
        stop("Must specify 'notification' and/or 'active'")
    if (!is.null(notification)) 
        GETparameters <- paste(GETparameters, notification, sep = "")
    if (!is.null(active) && active == TRUE) 
        GETparameters <- paste(GETparameters, "&Active=true", 
            sep = "")
    if (!is.null(active) && active == FALSE) 
        GETparameters <- paste(GETparameters, "&Active=false", 
            sep = "")
    Notification <- data.frame(matrix(ncol = 4))
    names(Notification) <- c("HITTypeId", "Notification", "Active", 
        "Valid")
    auth <- authenticate(operation, secret)
    if (browser == TRUE) {
        request <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, browser = browser, 
            sandbox = sandbox)
    }
    else {
        request <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, log.requests = log.requests, 
            sandbox = sandbox)
        Notification[1, ] <- c(hit.type, notification, active, 
            request$valid)
        if (request$valid == TRUE) {
            if (print == TRUE) {
                if (!is.null(notification) & is.null(active)) 
                  message("HITTypeNotification for ", hit.type, " Created")
                else if (!is.null(notification) & !is.null(active) && active == TRUE) 
                  message("HITTypeNotification ", hit.type, " Created & Active")
                else if (!is.null(notification) & !is.null(active) && active == FALSE) 
                  message("HITTypeNotification ", hit.type, " Created & Inactive")
                else if (is.null(notification) & !is.null(active) && active == TRUE) 
                  message("HITTypeNotification ", hit.type, " Active")
                else if (is.null(notification) & !is.null(active) && active == FALSE) 
                  message("HITTypeNotification ", hit.type, " Inactive")
            }
        }
        else if (request$valid == FALSE) {
            if (print == TRUE) 
                warning("Invalid Request")
        }
    }
    if (print == TRUE) 
        return(Notification)
    else invisible(Notification)
}
