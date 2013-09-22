RegisterHITType <-
hittype <-
function (title, description, reward, duration, keywords = NULL, 
    auto.approval.delay = NULL, qual.req = NULL, keypair = credentials(), 
    print = TRUE, browser = FALSE, log.requests = TRUE,
    sandbox = getOption('MTurkR.sandbox'), validation.test = FALSE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "RegisterHITType"
    if (nchar(curlEscape(title)) > 128) 
        stop("Title too long (128 char max)")
    if (nchar(curlEscape(description)) > 2000) 
        stop("Description too long (2000 char max)")
    if (as.numeric(duration) < 30 || as.numeric(duration) > 3153600) 
        stop("Duration must be between 30 (30 seconds) and 3153600 (365 days)")
    GETparameters <- paste("&Title=", curlEscape(title), "&Description=", 
        curlEscape(description), "&Reward.1.Amount=", reward, 
        "&Reward.1.CurrencyCode=USD", "&AssignmentDurationInSeconds=", 
        duration, sep = "")
    if (!is.null(keywords)) {
        if (nchar(curlEscape(keywords)) < 1000) 
            GETparameters <- paste(GETparameters, "&Keywords=", 
                curlEscape(keywords), sep = "")
        else
			stop("Keywords too long (1000 char max)")
    }
    if (!is.null(auto.approval.delay)) {
        if (as.numeric(auto.approval.delay) > 0 &
			as.numeric(auto.approval.delay) <= 2592000) 
            GETparameters <- paste(GETparameters, "&AutoApprovalDelayInSeconds=", 
                auto.approval.delay, sep = "")
        else
			warning("AutoApprovalDelayInSeconds must be between 0 (0 seconds) and 2592000 (30 days); defaults to 30 days")
    }
    if (!is.null(qual.req)) 
        GETparameters <- paste(GETparameters, qual.req, sep = "")
    auth <- authenticate(operation, secret)
    if (browser == TRUE) {
        request <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, browser = browser, 
            sandbox = sandbox, validation.test = validation.test)
		if(validation.test)
			invisible(request)
    }
    else {
        HITType <- data.frame(matrix(ncol = 2))
        names(HITType) <- c("HITTypeId", "Valid")
        request <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, log.requests = log.requests, 
            sandbox = sandbox, validation.test = validation.test)
		if(validation.test)
			invisible(request)
        if (request$valid == TRUE) {
            hit.type <- strsplit(strsplit(request$xml, "<HITTypeId>")[[1]][2], 
                "</HITTypeId>")[[1]][1]
            HITType[1, ] <- c(hit.type, request$valid)
            if (print == TRUE) 
                message("HITType Registered: ", HITType$HITTypeId[1])
        }
        else if (request$valid == FALSE) {
            HITType[1, ] <- c(NULL, request$valid)
            if (print == TRUE) 
                warning("Invalid Request")
        }
        if (print == TRUE) 
            return(HITType)
        else
			invisible(HITType)
    }
}
