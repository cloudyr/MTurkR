ExtendHIT <-
extend <-
function (hit = NULL, hit.type = NULL, add.assignments = NULL, 
    add.seconds = NULL, unique.request.token = NULL, keypair = credentials(), 
    print = TRUE, browser = FALSE, log.requests = TRUE, sandbox = FALSE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "ExtendHIT"
    GETparameters <- ""
    if (is.null(add.assignments) & is.null(add.seconds)) 
        stop("Must specify more assignments or time (in seconds)")
    else if (!is.null(add.assignments)) {
        if (!is.numeric(add.assignments) & !is.numeric(as.numeric(add.assignments))) 
            stop("Assignment increment is non-numeric")
        else if (as.numeric(add.assignments) < 1 | as.numeric(add.assignments) > 
            1e+09) 
            stop("Assignment increment must be between 1 and 1000000000")
        else GETparameters <- paste(GETparameters, "&MaxAssignmentsIncrement=", 
            add.assignments, sep = "")
    }
    else if (!is.null(add.seconds)) {
        if (!is.numeric(add.seconds) & !is.numeric(as.numeric(add.seconds))) 
            stop("Expiration increment is non-numeric")
        else if (as.numeric(add.seconds) < 3600 | as.numeric(add.seconds) > 
            31536000) 
            stop("Expiration increment must be between 3600 and 31536000")
        else GETparameters <- paste(GETparameters, "&ExpirationIncrementInSeconds=", 
            add.seconds, sep = "")
    }
    if (is.null(add.assignments)) 
        add.assignments <- 0
    if (is.null(add.seconds)) 
        add.seconds <- 0
    if (!is.null(unique.request.token) && nchar(curlEscape(unique.request.token)) > 
        64) 
        stop("UniqueRequestToken must be <= 64 characters")
    else if (!is.null(unique.request.token)) 
        GETparameters <- paste(GETparameters, "&UniqueRequestToken=", 
            curlEscape(unique.request.token), sep = "")
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
        if (length(hitlist) == 0 || is.null(hitlist)) 
            stop("No HITs found for HITType")
    }
    HITs <- data.frame(matrix(ncol = 4))
    names(HITs) <- c("HITId", "AssignmentsIncrement", "ExpirationIncrement", 
        "Valid")
    for (i in 1:length(hitlist)) {
        GETiteration <- paste(GETparameters, "&HITId=", hitlist[i], 
            sep = "")
        auth <- authenticate(operation, secret)
        if (browser == TRUE) {
            request <- request(keyid, auth$operation, auth$signature, 
                auth$timestamp, GETiteration, browser = browser, 
                sandbox = sandbox)
        }
        else {
            request <- request(keyid, auth$operation, auth$signature, 
                auth$timestamp, GETiteration, log.requests = log.requests, 
                sandbox = sandbox)
            HITs[i, ] = c(hitlist[i], add.assignments, add.seconds, 
                request$valid)
            if (request$valid == TRUE & print == TRUE) {
                if (!is.null(add.assignments) & !is.null(add.seconds)) 
                  cat(i, ": HIT (", hitlist[i], ") Extended by ", 
                    add.assignments, " Assignments & ", add.seconds, 
                    " Seconds\n", sep = "")
                else if (!is.null(add.assignments)) 
                  cat(i, ": HIT (", hitlist[i], ") Extended by ", 
                    add.assignments, " Assignments\n", sep = "")
                else if (!is.null(add.seconds)) 
                  cat(i, ": HIT (", hitlist[i], ") Extended by ", 
                    add.seconds, " Seconds\n", sep = "")
            }
            else if (request$valid == FALSE & print == TRUE) {
                cat(i, ": Invalid Request for HIT ", hitlist[i], 
                  " \n", sep = "")
            }
        }
    }
    invisible(HITs)
}
