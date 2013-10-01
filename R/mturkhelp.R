mturkhelp <-
function (about, helptype = NULL, keypair = credentials(), print = getOption('MTurkR.print'), 
    browser = getOption('MTurkR.browser'), log.requests = getOption('MTurkR.log'),
    validation.test = getOption('MTurkR.test')) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "Help"
    if (about %in% ListOperations()) {
        if(is.null(helptype))
            helptype <- "Operation"
        GETparameters <- paste("&HelpType=",helptype, "&About=", about, sep = "")
    }
    else if (about %in% c("Minimal", "HITDetail", "HITQuestion", "HITAssignmentSummary",
                          "AssignmentFeedback", "Parameters", "Request")) {
        if(is.null(helptype))
            helptype <- "ResponseGroup"
        GETparameters <- paste("&HelpType=",helptype, "&About=", about, sep = "")
    }
    else
        stop("Operation or ResponseGroup not recognized")
    auth <- authenticate(operation, secret)
    if (browser == TRUE) {
        request <- request(keyid, auth$operation, auth$signature, 
             auth$timestamp, GETparameters, browser = browser, validation.test = validation.test)
        if(validation.test)
            invisible(request)
    }
    else {
        request <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, log.requests = log.requests,
			validation.test = validation.test)
		if(validation.test)
			invisible(request)
        if (request$valid == TRUE) {
            request$operation <- strsplit(strsplit(request$xml, 
                "<MessageText>")[[1]][2], "</MessageText>")[[1]][1]
            request$documentation <- strsplit(strsplit(request$xml, 
                "<Name>")[[1]][2], "</Name>")[[1]][1]
            message(request$operation, ": ", request$documentation)
            if (helptype == "Operation") 
                message("Or visit: http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_", 
                  about, "Operation.html")
            if (helptype == "ResponseGroup") 
                message("Or visit: http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_HITDataStructureArticle.html")
        }
        else if (request$valid == FALSE) {
            warning("Invalid Request")
        }
    }
}
