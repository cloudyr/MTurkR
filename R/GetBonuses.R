GetBonuses <-
bonuses <-
function (assignment = NULL, hit = NULL, hit.type = NULL, return.all = TRUE, 
    pagenumber = "1", pagesize = "100", verbose = getOption('MTurkR.verbose'), ...) {
    # temporary check for `print` argument (remove after v1.0)
    if('print' %in% names(list(...)) && is.null(verbose))
        verbose <- list(...)$print
    operation <- "GetBonusPayments"
    if(is.null(hit) & is.null(hit.type) & is.null(assignment)) 
        stop("Specify HITId xor AssignmentId xor HITType")
    else if(!is.null(hit) & !is.null(hit.type) & !is.null(assignment)) 
        stop("Specify HITId xor AssignmentId xor HITType")
    if(as.numeric(pagesize) < 1 || as.numeric(pagesize) > 100) 
        stop("'pagesize' must be in range (1,100)")
    if(as.numeric(pagenumber) < 1) 
        stop("'pagenumber' must be > 1")
    batch <- function(type='assign', obj, page=1){
        if(type=='assign') {
            GETparameters <- paste( "&AssignmentId=", as.character(obj), 
                                    "&PageNumber=", page,
                                    "&PageSize=100", sep = "")
        } else if(type=='hit'){
            GETparameters <- paste("&HITId=", as.character(obj),
                                   "&PageNumber=", page, 
                                   "&PageSize=100", sep = "")
        }       
        out <- request(operation, GETparameters = GETparameters, ...)
        return(out)
    }
    
    if(!is.null(hit) || !is.null(assignment)){
        if(is.null(hit)){
            obj <- hit
            type <- 'hit'
        } else if(is.null(assignment)){
            obj <- assignment
            type <- 'assign'
        }
        request <- batch(type, obj, pagenumber)
        if(is.null(request$valid))
            return(request)
        if(request$valid == TRUE) {
            runningtotal <- strsplit(strsplit(request$xml, 
                    "<NumResults>")[[1]][2], "</NumResults>")[[1]][1]
            if(return.all){
                total <- strsplit(strsplit(request$xml, 
                    "<TotalNumResults>")[[1]][2], "</TotalNumResults>")[[1]][1]
                Bonuses <- list()
                Bonuses[[1]] <- as.data.frame.BonusPayments(xml.parsed = xmlParse(request$xml))
                pagenumber <- 2
                while(total > runningtotal){
                    nextbatch <- batch(type, obj, pagenumber)
                    Bonuses[[pagenumber]] <- as.data.frame.BonusPayments(xml.parsed = xmlParse(nextbatch$xml))
                    batch_total <- strsplit(strsplit(nextbatch$xml, 
                        "<NumResults>")[[1]][2], "</NumResults>")[[1]][1]
                    runningtotal <- runningtotal + batch_total
                    pagenumber <- pagenumber + 1
                }
                if(verbose)
                    message(runningtotal, " Bonuses Retrieved")
                return(do.call('rbind',Bonuses))
            } else {
                if(verbose)
                    message(runningtotal, " Bonuses Retrieved")
                Bonuses <- as.data.frame.BonusPayments(xml.parsed = xmlParse(request$xml))
                if(!is.null(hit)) 
                    Bonuses$HITId <- hit
                return(Bonuses)
            }
        } else if(request$valid == FALSE){
            warning("Invalid Request")
            return(request)
        }
    } else if(!is.null(hit.type)) {
        hitsearch <- SearchHITs(verbose = FALSE, return.qual.dataframe = FALSE, ...)
        hitlist <- hitsearch$HITs$HITId[hitsearch$HITs$HITTypeId %in% hit.type]
        if(length(hitlist) == 0) 
            stop("No HITs found for HITType")
        Bonuses <- list()
        for(i in 1:length(hitlist)) {
            b <- GetBonuses(hit = hitlist[i], return.all = return.all, ...)
            Bonuses[[i]] <- as.data.frame.BonusPayments(xml.parsed = xmlParse(b$xml))
        }
        out <- do.call('rbind',Bonuses)
        if(verbose) 
            message(nrow(Bonuses), " Bonuses Retrieved")
        return(out)
    }
}
