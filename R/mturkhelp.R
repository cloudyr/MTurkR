mturkhelp <-
function (about, helptype = NULL, keypair = credentials(), print = TRUE, 
    browser = FALSE, log.requests = TRUE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "Help"
    if (about %in% ListOperations()) {
        helptype <- "Operation"
        GETparameters <- paste("&HelpType=Operation", "&MessageText=", 
            about, sep = "")
    }
    else if (about %in% c("Minimal", "HITDetail", "HITQuestion", 
        "HITAssignmentSummary")) {
        helptype <- "ResponseGroup"
        GETparameters <- paste("&HelpType=ResponseGroup", "&MessageText=", 
            about, sep = "")
    }
    else stop("Operation or ResponseGroup not recognized")
    auth <- authenticate(operation, secret)
    if (browser == TRUE) {
        request <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, browser = browser)
    }
    else {
        request <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, log.requests = log.requests)
        if (request$valid == TRUE) {
            request$operation <- strsplit(strsplit(request$xml, 
                "<MessageText>")[[1]][2], "</MessageText>")[[1]][1]
            request$documentation <- strsplit(strsplit(request$xml, 
                "<Name>")[[1]][2], "</Name>")[[1]][1]
            cat(request$operation, ": ", request$documentation, 
                sep = "")
            if (helptype == "Operation") 
                cat("\nOr visit: http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_", 
                  about, "Operation.html\n", sep = "")
            if (helptype == "ResponseGroup") 
                cat("\nOr visit: http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_HITDataStructureArticle.html\n", 
                  sep = "")
        }
        else if (request$valid == FALSE) {
            cat("Invalid Request\n")
        }
    }
}
