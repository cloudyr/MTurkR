ExpireHIT <-
expire <-
function (hit = NULL, hit.type = NULL, keypair = getOption('MTurkR.keypair'), 
    print = getOption('MTurkR.print'), 
    log.requests = getOption('MTurkR.log'), sandbox = getOption('MTurkR.sandbox'),
    validation.test = getOption('MTurkR.test')) {
    if(!is.null(keypair))
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "ForceExpireHIT"
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
        request <- request(keypair[1], operation, secret=keypair[2],
            GETparameters = GETiteration, log.requests = log.requests, 
            sandbox = sandbox, validation.test = validation.test)
        if(validation.test)
            return(invisible(request))
        HITs[i, ] <- c(hitlist[i], request$valid)
        if(request$valid == TRUE) {
            if(print == TRUE) 
                message(i, ": HIT ", hitlist[i], " Expired")
        }
        else if(request$valid == FALSE & print == TRUE) 
            warning(i, ": Invalid Request for HIT ", hitlist[i])
    }
    HITs$Valid <- factor(HITs$Valid, levels=c('TRUE','FALSE'))
    return(HITs)
}
