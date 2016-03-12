ChangeHITType <-
changehittype <-
function (hit = NULL, old.hit.type = NULL, new.hit.type = NULL, 
    title = NULL, description = NULL, reward = NULL, duration = NULL, 
    keywords = NULL, auto.approval.delay = NULL, qual.req = NULL, 
    old.annotation = NULL,
    verbose = getOption('MTurkR.verbose', TRUE), ...) {
    operation <- "ChangeHITTypeOfHIT"
    if ((is.null(hit) & is.null(old.hit.type) & is.null(old.annotation)) | 
       (!is.null(hit) & !is.null(old.hit.type) & !is.null(old.annotation))) {
        stop("Must provide 'hit' xor 'old.hit.type' xor 'old.annotation'")
    }
    if (is.factor(hit)) {
        hit <- as.character(hit)
    }
    if (!is.null(new.hit.type)) {
        if (!is.null(new.hit.type) & (!is.null(title) || !is.null(description) || 
            !is.null(reward) || !is.null(duration))) {
            warning("HITType specified, HITType parameters (title, description, reward, duration) ignored")
        }
    } else {
        if (is.null(title) || is.null(description) || is.null(reward) || is.null(duration))  {
            stop("Must specify new HITType xor new HITType parameters (title, description, reward, duration)")
        } else {
            register <- RegisterHITType(title, description, reward, duration,
                keywords = keywords, auto.approval.delay = auto.approval.delay, 
                qual.req = qual.req, ...)
            if (is.null(register$valid)) {
                return(register)
            }
            if (register$valid == FALSE) {
                stop("Could not RegisterHITType(), check parameters")
            } else {
                new.hit.type <- register$HITTypeId
            }
        }
    }
    if (!is.null(hit)) {
        hitlist <- hit
    } else if(!is.null(old.hit.type)) {
        if (is.factor(old.hit.type)) {
            old.hit.type <- as.character(old.hit.type)
        }
        hitsearch <- SearchHITs(verbose = FALSE, return.qual.dataframe = FALSE, ...)
        hitlist <- hitsearch$HITs$HITId[hitsearch$HITs$HITTypeId %in% old.hit.type]
    } else if (!is.null(old.annotation)) {
        if (is.factor(old.annotation)) {
            old.annotation <- as.character(old.annotation)
        }
        hitsearch <- SearchHITs(verbose = FALSE, return.qual.dataframe = FALSE, ...)
        hitlist <- hitsearch$HITs$HITId[hitsearch$HITs$RequesterAnnotation %in% old.annotation]
    } 
    if (length(hitlist) == 0) {
        stop("No HITs found for HITType")
    }
    HITs <- emptydf(length(hitlist), 4, c("HITId", "oldHITTypeId", "newHITTypeId", "Valid"))
    for (i in 1:length(hitlist)) {
        GETparameters <- paste("&HITId=", hitlist[i],
                               "&HITTypeId=", new.hit.type, sep = "")
        x <- request(operation, GETparameters = GETparameters, ...)
        if (is.null(x$valid)) {
            return(x)
        }
        if (is.null(old.hit.type)) {
            HITs[i, ] <- c(hitlist[i], NA, new.hit.type, x$valid)
        } else {
            HITs[i, ] <- c(hitlist[i], old.hit.type, new.hit.type, x$valid)
        }
        if (verbose) {
            if (x$valid == TRUE) {
                message(i, ": HITType of HIT ", hitlist[i], " Changed to: ",new.hit.type)
            } else if(x$valid == FALSE) {
                warning(i,": Invalid Request for HIT ",hitlist[i])
            }
        }
    }
    HITs$Valid <- factor(HITs$Valid, levels=c('TRUE','FALSE'))
    return(HITs)
}
