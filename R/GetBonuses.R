GetBonuses <-
bonuses <-
function (assignment = NULL, hit = NULL, hit.type = NULL, return.all = TRUE, 
    pagenumber = "1", pagesize = "100", keypair = credentials(), 
    print = getOption('MTurkR.print'), browser = getOption('MTurkR.browser'),
    log.requests = getOption('MTurkR.log'),
    sandbox = getOption('MTurkR.sandbox'),
    return.bonus.dataframe = TRUE,
    validation.test = getOption('MTurkR.test')) {
    if(!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else
        stop("No keypair provided or 'credentials' object not stored")
    operation <- "GetBonusPayments"
    if(is.null(hit) & is.null(hit.type) & is.null(assignment)) 
        stop("Specify HITId xor AssignmentId xor HITType")
    else if(!is.null(hit) & !is.null(hit.type) & !is.null(assignment)) 
        stop("Specify HITId xor AssignmentId xor HITType")
    if(return.all == TRUE) {
        pagenumber <- "1"
        pagesize <- "100"
    }
    if(as.numeric(pagesize) < 1 || as.numeric(pagesize) > 100) 
        stop("'pagesize' must be in range (1,100)")
    if(as.numeric(pagenumber) < 1) 
        stop("'pagenumber' must be > 1")
    if(xor(is.null(hit), is.null(assignment))) {
        if(!is.null(hit)) {
            if(is.factor(hit))
                hit <- as.character(hit)
            GETparameters <- paste("&HITId=", hit, "&PageNumber=", 
                pagenumber, "&PageSize=", pagesize, sep = "")
        }
        else{
			if(is.factor(assignment))
                assignment <- as.character(assignment)
            GETparameters <- paste(	"&AssignmentId=", assignment, 
									"&PageNumber=", pagenumber,
									"&PageSize=", pagesize, sep = "")
        }
        auth <- authenticate(operation, secret)
        request <- request(keyid, auth$operation, auth$signature, 
            auth$timestamp, GETparameters, browser=browser,
            log.requests = log.requests, sandbox = sandbox,
            validation.test = validation.test)
		if(validation.test)
			return(invisible(request))
        if(browser == TRUE)
            return(invisible(NULL))
        request$operation <- operation
        if(request$valid == TRUE) {
            request$total.bonuses <- strsplit(strsplit(request$xml, 
                "<NumResults>")[[1]][2], "</NumResults>")[[1]][1]
            if(print == TRUE)
                message(request$total.bonuses, " Bonuses Retrieved")
            if(return.bonus.dataframe == TRUE) {
                Bonuses <- BonusPaymentsToDataFrame(xml = request$xml)
                if(!is.null(hit)) 
                    Bonuses$HITId <- hit
                return(Bonuses)
            }
        }
        else if(request$valid == FALSE)
            warning("Invalid Request")
    }
    else if(!is.null(hit.type)) {
        hitsearch <- SearchHITs(keypair = keypair, print = FALSE, 
								log.requests = log.requests, sandbox = sandbox,
								return.qual.dataframe = FALSE)
        hitlist <- hitsearch$HITs$HITId[hitsearch$HITs$HITTypeId %in% hit.type]
        if(length(hitlist) == 0) 
            stop("No HITs found for HITType")
        z <- setNames(data.frame(matrix(ncol = 3, nrow = length(hitlist))),
                c("HITId", "Number", "Amount"))
        z$HITId <- hitlist
        for(i in 1:length(hitlist)) {
            GETparameters <- paste(	"&HITId=", hitlist[i],
									"&PageNumber=", pagenumber,
									"&PageSize=", pagesize, sep = "")
            auth <- authenticate(operation, secret)
            request <- request(keyid, auth$operation, auth$signature, 
                auth$timestamp, GETparameters, log.requests = log.requests, 
                sandbox = sandbox, validation.test = validation.test)
			if(validation.test)
				invisible(request)
            if(request$valid == TRUE) {
                request$bonuses <- BonusPaymentsToDataFrame(xml = request$xml)
                if(!is.null(request$bonuses)) {
                    z$Number[i] <- dim(request$bonuses)[1]
                    z$Amount[i] <- round(sum(as.numeric(request$bonuses$Amount)), 2)
                }
                else {
                    z$Number[i] <- 0
                    z$Amount[i] <- 0
                }
            }
            else {
                if(print == TRUE) 
                    warning("Invalid Request for HIT ", hitlist[i])
            }
        }
        if(print == TRUE) 
            message(sum(z$Number), " Bonuses Retrieved")
        if(return.bonus.dataframe == TRUE)
            return(z)
        else
            return(invisible(NULL))
    }
}
