SetHITAsReviewing <-
reviewing <-
function (hit = NULL, hit.type = NULL, revert = FALSE, keypair = credentials(), 
    print = getOption('MTurkR.print'), browser = getOption('MTurkR.browser'),
    log.requests = getOption('MTurkR.log'), sandbox = getOption('MTurkR.sandbox'),
    validation.test = getOption('MTurkR.test')) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "SetHITAsReviewing"
    if (revert == TRUE) 
        revert <- "true"
    else revert <- "false"
    if ((is.null(hit) & is.null(hit.type)) | (!is.null(hit) & 
        !is.null(hit.type))) 
        stop("Must provide 'hit' xor 'hit.type'")
    else if (!is.null(hit)) {
        hitlist <- hit
    }
    else if (!is.null(hit.type)) {
        hitsearch <- SearchHITs(keypair = keypair, print = FALSE, 
            log.requests = log.requests, sandbox = sandbox, return.qual.dataframe = FALSE)
        hitlist <- hitsearch$HITs[hitsearch$HITs$HITTypeId == 
            hit.type, ]$HITId
        if (length(hitlist) == 0) 
            stop("No HITs found for HITType")
    }
    GETparameters = paste("&Revert=", revert, sep = "")
    HITs <- data.frame(matrix(ncol = 3))
    names(HITs) <- c("HITId", "Status", "Valid")
    for (i in 1:length(hitlist)) {
        GETiteration <- paste(GETparameters, "&HITId=", hitlist[i], 
            sep = "")
        auth <- authenticate(operation, secret)
        if (browser == TRUE) {
            request <- request(keyid, auth$operation, auth$signature, 
                auth$timestamp, GETiteration, browser = browser, 
                sandbox = sandbox, validation.test = validation.test)
			if(validation.test)
				invisible(request)
        }
        else {
            request <- request(keyid, auth$operation, auth$signature, 
                auth$timestamp, GETiteration, log.requests = log.requests, 
                sandbox = sandbox, validation.test = validation.test)
			if(validation.test)
				invisible(request)
            if (revert == "false") 
                status <- "Reviewing"
            else if (revert == "true") 
                status <- "Reviewable"
            HITs[i, ] = c(hitlist[i], status, request$valid)
            if (request$valid == TRUE) {
                if (print == TRUE) {
                  if (revert == "false") 
                    message(i, ": HIT (", hitlist[i], ") set as Reviewing")
                  if (revert == "true") 
                    message(i, ": HIT (", hitlist[i], ") set as Reviewable")
                }
            }
            else if (request$valid == FALSE) {
                if (print == TRUE) 
                  warning(i, ": Invalid Request for HIT ", hitlist[i])
            }
        }
    }
    if (print == TRUE) 
        return(HITs)
    else
		invisible(HITs)
}
