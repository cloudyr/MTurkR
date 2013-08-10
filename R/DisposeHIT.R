DisposeHIT <-
disposehit <-
function (hit = NULL, hit.type = NULL, response.group = NULL, 
    keypair = credentials(), print = TRUE, browser = FALSE, log.requests = TRUE, 
    sandbox = FALSE, validation.test = FALSE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "DisposeHIT"
    if ((is.null(hit) & is.null(hit.type)) | (!is.null(hit) & 
        !is.null(hit.type))) 
        stop("Must provide 'hit' xor 'hit.type'")
    else if (!is.null(hit)) {
        hitlist <- hit
    }
    else if (!is.null(hit.type)) {
        hitsearch <- SearchHITs(keypair = keypair, print = FALSE, 
            log.requests = log.requests, sandbox = sandbox, return.qual.dataframe = FALSE)
        hitlist <- hitsearch$HITs[hitsearch$HITs$HITTypeId == hit.type, ]$HITId
        if (length(hitlist) == 0) 
            stop("No HITs found for HITType")
    }
    HITs <- data.frame(matrix(ncol = 2))
    names(HITs) <- c("HITId", "Valid")
    for (i in 1:length(hitlist)) {
        GETiteration <- paste("&HITId=", hitlist[i], sep = "")
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
			HITs[i, ] = c(hitlist[i], request$valid)
            if (request$valid == TRUE & print == TRUE) 
                message(i, ": HIT ", hitlist[i], " Disposed")
            else if (request$valid == FALSE & print == TRUE) {
                warning(i, ": Invalid Request for HIT ", hitlist[i])
            }
        }
    }
    if (print == TRUE) 
        return(HITs)
    else
		invisible(HITs)
}
