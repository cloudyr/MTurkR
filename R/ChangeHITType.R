ChangeHITType <-
changehittype <-
function (hit = NULL, old.hit.type = NULL, new.hit.type = NULL, 
    title = NULL, description = NULL, reward = NULL, duration = NULL, 
    keywords = NULL, auto.approval.delay = NULL, qual.req = NULL, 
    keypair = credentials(), print = getOption('MTurkR.print'),
    browser = getOption('MTurkR.browser'), log.requests = getOption('MTurkR.log'), 
    sandbox = getOption('MTurkR.sandbox'), validation.test = getOption('MTurkR.test')) {
    if(!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "ChangeHITTypeOfHIT"
    if((is.null(hit) & is.null(old.hit.type)) | (!is.null(hit) & !is.null(old.hit.type))) 
        stop("Must provide 'hit' xor 'old.hit.type'")
    if(is.factor(hit))
        hit <- as.character(hit)
    if(!is.null(new.hit.type)) {
        if(!is.null(new.hit.type) & (!is.null(title) || !is.null(description) || 
            !is.null(reward) || !is.null(duration))) 
            warning("HITType specified, HITType parameters (title, description, reward, duration) ignored")
    }
    else{
        if(is.null(title) || is.null(description) || is.null(reward) || is.null(duration)) 
            stop("Must specify new HITType xor new HITType parameters (title, description, reward, duration)")
        else{
            register <- RegisterHITType(keypair, title, description, 
                reward, duration, keywords = keywords, auto.approval.delay = auto.approval.delay, 
                qual.req = qual.req, print = print, log.requests = log.requests, 
                sandbox = sandbox, validation.test = validation.test)
			if(validation.test)
				invisible(request)
            if(register$valid == FALSE) 
                stop("Could not RegisterHITType(), check parameters")
            else
				new.hit.type <- register$HITTypeId
        }
    }
    if(!is.null(hit))
        hitlist <- hit
    else if(!is.null(old.hit.type)) {
        if(is.factor(old.hit.type))
            old.hit.type <- as.character(old.hit.type)
        hitsearch <- SearchHITs(keypair = keypair, print = FALSE,
								log.requests = log.requests, sandbox = sandbox,
								return.qual.dataframe = FALSE)
        hitlist <- hitsearch$HITs$HITId[hitsearch$HITs$HITTypeId %in% old.hit.type]
        if(length(hitlist) == 0) 
            stop("No HITs found for HITType")
    }
    HITs <- setNames(data.frame(matrix(ncol = 4, nrow=length(hitlist))),
                c("HITId", "oldHITTypeId", "newHITTypeId", "Valid"))
    for(i in 1:length(hitlist)) {
        GETparameters <- paste(	"&HITId=", hitlist[i],
								"&HITTypeId=", new.hit.type, sep = "")
        auth <- authenticate(operation, secret)
        if(browser == TRUE) {
            x <- request(keyid, auth$operation, auth$signature, 
                auth$timestamp, GETparameters, browser = browser, 
                sandbox = sandbox, validation.test = validation.test)
			if(validation.test)
				invisible(x)
        }
        else{
            x <- request(keyid, auth$operation, auth$signature, 
                auth$timestamp, GETparameters, log.requests = log.requests, 
                sandbox = sandbox, validation.test = validation.test)
			if(validation.test)
				invisible(x)
            if(is.null(old.hit.type)) 
                HITs[i, ] <- c(hitlist[i], NA, new.hit.type, x$valid)
            else
                HITs[i, ] <- c(hitlist[i], old.hit.type, new.hit.type, x$valid)
            if(print == TRUE) {
                if(x$valid == TRUE)
					message(i, ": HITType of HIT ", hitlist[i], " Changed to: ",new.hit.type)
                else if(x$valid == FALSE)
					warning(i,": Invalid Request for HIT ",hitlist[i])
            }
        }
    }
    if(print == TRUE) 
        return(HITs)
    else
		invisible(HITs)
}
