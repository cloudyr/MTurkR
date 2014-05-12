SetHITAsReviewing <-
reviewing <-
function (hit = NULL, hit.type = NULL, revert = FALSE, 
    keypair = getOption('MTurkR.keypair'), 
    print = getOption('MTurkR.print'), 
    log.requests = getOption('MTurkR.log'), sandbox = getOption('MTurkR.sandbox'),
    validation.test = getOption('MTurkR.test')) {
    if(is.null(keypair))
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "SetHITAsReviewing"
    if(revert == TRUE) 
        revert <- "true"
    else
        revert <- "false"
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
    GETparameters <- paste("&Revert=", revert, sep = "")
    HITs <- data.frame(matrix(ncol = 3, nrow=length(hitlist)))
    names(HITs) <- c("HITId", "Status", "Valid")
    for(i in 1:length(hitlist)) {
        GETiteration <- paste(GETparameters, "&HITId=", hitlist[i], sep = "")        
        request <- request(keypair[1], operation, secret=keypair[2],
            GETparameters = GETiteration, log.requests = log.requests, 
            sandbox = sandbox, validation.test = validation.test)
        if(validation.test)
            return(invisible(request))
        if(revert == "false") 
            status <- "Reviewing"
        else if(revert == "true") 
            status <- "Reviewable"
        HITs[i, ] <- c(hitlist[i], status, request$valid)
        if(request$valid == TRUE & print == TRUE) {
            if(revert == "false") 
                message(i, ": HIT (", hitlist[i], ") set as Reviewing")
            if(revert == "true") 
                message(i, ": HIT (", hitlist[i], ") set as Reviewable")
        }
        else if(request$valid == FALSE & print == TRUE)
            warning(i, ": Invalid Request for HIT ", hitlist[i])
    }
    HITs$Valid <- factor(HITs$Valid, levels=c('TRUE','FALSE'))
    return(HITs)
}
