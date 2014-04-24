DisableHIT <-
disable <-
function (hit = NULL, hit.type = NULL, response.group = NULL, 
    keypair = credentials(), print = getOption('MTurkR.print'),
    browser = getOption('MTurkR.browser'), log.requests = getOption('MTurkR.log'), 
    sandbox = getOption('MTurkR.sandbox'), validation.test = getOption('MTurkR.test')) {
    if(!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "DisableHIT"
    if(!is.null(response.group)) {
        if(!response.group %in% c("Minimal", "HITQuestion", 
            "HITDetail", "HITAssignmentSummary")) 
            stop("ResponseGroup must be in c(Minimal,HITQuestion,HITDetail,HITAssignmentSummary)")
    }
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
        if(length(hitlist) == 0) 
            stop("No HITs found for HITType")
    }
    HITs <- setNames(data.frame(matrix(ncol=2, nrow=length(hitlist))), c("HITId", "Valid"))
    for(i in 1:length(hitlist)) {
        GETiteration <- paste("&HITId=", hitlist[i], sep = "")
        if(!is.null(response.group)) {
            if(length(response.group) == 1) 
                GETiteration <- paste(    GETiteration, "&ResponseGroup=", 
                                        response.group, sep = "")
            else {
                for(i in 1:length(response.group)) {
                  GETiteration <- paste(GETiteration, "&ResponseGroup", i-1,
                                        "=", response.group[i], sep = "")
                }
            }
        }
        auth <- authenticate(operation, secret)
        if(browser == TRUE) {
            request <- request(keyid, auth$operation, auth$signature, 
                auth$timestamp, GETiteration, browser = browser, 
                sandbox = sandbox, validation.test = validation.test)
            if(validation.test)
                return(invisible(request))
        }
        else {
            request <- request(keyid, auth$operation, auth$signature, 
                auth$timestamp, GETiteration, log.requests = log.requests, 
                sandbox = sandbox, validation.test = validation.test)
            if(validation.test)
                return(invisible(request))
            if(request$valid == TRUE) {
                if(is.null(response.group)) 
                    request$ResponseGroup <- c("Minimal")
                else
                    request$ResponseGroup <- response.group
                HITs[i, ] <- c(hitlist[i], request$valid)
                if(print == TRUE) 
                    message(i, ": HIT ", hitlist[i], " Disabled")
            }
            else if(request$valid == FALSE & print == TRUE)
                warning(i, ": Invalid Request for HIT ", hitlist[i])
        }
    }
    HITs$Valid <- factor(HITs$Valid, levels=c('TRUE','FALSE'))
    return(HITs)
}
