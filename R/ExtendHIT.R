ExtendHIT <-
extend <-
function (hit = NULL, hit.type = NULL, add.assignments = NULL, 
    add.seconds = NULL, unique.request.token = NULL,
    keypair = getOption('MTurkR.keypair'), 
    print = getOption('MTurkR.print'), browser = getOption('MTurkR.browser'),
    log.requests = getOption('MTurkR.log'), sandbox = getOption('MTurkR.sandbox'),
    validation.test = getOption('MTurkR.test')) {
    if(is.null(keypair))
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "ExtendHIT"
    GETparameters <- ""
    if(is.null(add.assignments) & is.null(add.seconds)) 
        stop("Must specify more assignments or time (in seconds)")
    else if(!is.null(add.assignments)) {
        if(!is.numeric(add.assignments) & !is.numeric(as.numeric(add.assignments))) 
            stop("Assignment increment is non-numeric")
        else if(as.numeric(add.assignments) < 1 | as.numeric(add.assignments) > 1e+09) 
            stop("Assignment increment must be between 1 and 1000000000")
        else GETparameters <- paste(GETparameters, "&MaxAssignmentsIncrement=", 
                                    add.assignments, sep = "")
    }
    else if(!is.null(add.seconds)) {
        if(!is.numeric(add.seconds) & !is.numeric(as.numeric(add.seconds))) 
            stop("Expiration increment is non-numeric")
        else if(as.numeric(add.seconds) < 3600 | as.numeric(add.seconds) > 31536000) 
            stop("Expiration increment must be between 3600 and 31536000")
        else
            GETparameters <- paste(GETparameters,"&ExpirationIncrementInSeconds=",
                                    add.seconds, sep = "")
    }
    if(is.null(add.assignments)) 
        add.assignments <- 0
    if(is.null(add.seconds)) 
        add.seconds <- 0
    if(!is.null(unique.request.token) && nchar(curlEscape(unique.request.token)) > 64) 
        stop("UniqueRequestToken must be <= 64 characters")
    else if(!is.null(unique.request.token)) 
        GETparameters <- paste(GETparameters, "&UniqueRequestToken=", 
            curlEscape(unique.request.token), sep = "")
    if((is.null(hit) & is.null(hit.type)) | (!is.null(hit) & !is.null(hit.type))) 
        stop("Must provide 'hit' xor 'hit.type'")
    else if(!is.null(hit)){
        if(is.factor(hit))
            hit <- as.character(hit)
        hitlist <- hit
    }
    else if(!is.null(hit.type)) {
        if(is.factor(hit.type))
            hit.type <- as.character(hit.type)
        hitsearch <- SearchHITs(keypair = keypair, print = FALSE, 
            log.requests = log.requests, sandbox = sandbox, return.qual.dataframe = FALSE)
        hitlist <- hitsearch$HITs$HITId[hitsearch$HITs$HITTypeId %in% hit.type]
        if(length(hitlist) == 0 || is.null(hitlist))
            stop("No HITs found for HITType")
    }
    HITs <- setNames(data.frame(matrix(ncol=4, nrow=length(hitlist))), 
                c("HITId", "AssignmentsIncrement", "ExpirationIncrement", "Valid"))
    for(i in 1:length(hitlist)) {
        GETiteration <- paste(GETparameters, "&HITId=", hitlist[i], sep = "")
        if(browser == TRUE) {
            request <- request(keypair[1], operation, secret=keypair[2],
                GETparameters = GETiteration, browser = browser, 
                sandbox = sandbox, validation.test = validation.test)
            if(validation.test)
                return(invisible(request))
        }
        else {
            request <- request(keypair[1], operation, secret=keypair[2],
            GETparameters = GETiteration, log.requests = log.requests, 
                sandbox = sandbox, validation.test = validation.test)
            if(validation.test)
                invisible(request)
            HITs[i, ] <- c(hitlist[i], add.assignments, add.seconds, request$valid)
            if(request$valid == TRUE & print == TRUE) {
                if(!is.null(add.assignments) & !is.null(add.seconds)) 
                    message(i, ": HIT (", hitlist[i], ") Extended by ", 
                            add.assignments, " Assignments & ", add.seconds, " Seconds")
                else if(!is.null(add.assignments)) 
                    message(i, ": HIT (", hitlist[i], ") Extended by ", 
                            add.assignments, " Assignments")
                else if(!is.null(add.seconds)) 
                    message(i, ": HIT (", hitlist[i], ") Extended by ", 
                            add.seconds, " Seconds")
            }
            else if(request$valid == FALSE & print == TRUE) {
                warning(i, ": Invalid Request for HIT ", hitlist[i])
            }
        }
    }
    return(HITs)
}
