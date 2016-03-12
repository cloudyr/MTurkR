ExtendHIT <-
extend <-
function(hit = NULL, 
         hit.type = NULL, 
         annotation = NULL, 
         add.assignments = NULL, 
         add.seconds = NULL, 
         unique.request.token = NULL,
         verbose = getOption('MTurkR.verbose', TRUE), ...) {
    operation <- "ExtendHIT"
    GETparameters <- ""
    if (is.null(add.assignments) & is.null(add.seconds)) {
        stop("Must specify more assignments or time (in seconds)")
    }
    if (!is.null(add.assignments)) {
        if (!is.numeric(add.assignments) & !is.numeric(as.numeric(add.assignments))) {
            stop("Assignment increment is non-numeric")
        } else if (as.numeric(add.assignments) < 1 | as.numeric(add.assignments) > 1e+09) {
            stop("Assignment increment must be between 1 and 1000000000")
        } else {
            GETparameters <- paste(GETparameters, "&MaxAssignmentsIncrement=", 
                                    add.assignments, sep = "")
        }
    }
    if (!is.null(add.seconds)) {
        if (!is.numeric(add.seconds) & !is.numeric(as.numeric(add.seconds))) {
            stop("Expiration increment is non-numeric")
        } else if (as.numeric(add.seconds) < 3600 | as.numeric(add.seconds) > 31536000) {
            stop("Expiration increment must be between 3600 and 31536000")
        } else {
            GETparameters <- paste(GETparameters,"&ExpirationIncrementInSeconds=",
                                    add.seconds, sep = "")
        }
    }
    if (is.null(add.assignments)) {
        add.assignments <- 0
    }
    if (is.null(add.seconds)) {
        add.seconds <- 0
    }
    if (!is.null(unique.request.token) && nchar(curl_escape(unique.request.token)) > 64) {
        stop("UniqueRequestToken must be <= 64 characters")
    } else if (!is.null(unique.request.token)) {
        GETparameters <- paste(GETparameters, "&UniqueRequestToken=", 
            curl_escape(unique.request.token), sep = "")
    }
    if ((is.null(hit) & is.null(hit.type) & is.null(annotation)) | 
        (!is.null(hit) & !is.null(hit.type) & !is.null(annotation))) {
        stop("Must provide 'hit' xor 'hit.type' xor 'annotation'")
    } else if (!is.null(hit)) {
        if (is.factor(hit)) {
            hit <- as.character(hit)
        }
        hitlist <- hit
    } else if (!is.null(hit.type)) {
        if (is.factor(hit.type)) {
            hit.type <- as.character(hit.type)
        }
        hitsearch <- SearchHITs(verbose = FALSE, return.qual.dataframe = FALSE, ...)
        hitlist <- hitsearch$HITs$HITId[hitsearch$HITs$HITTypeId %in% hit.type]
    } else if (!is.null(annotation)) {
        if (is.factor(annotation)) {
            annotation <- as.character(annotation)
        }
        hitsearch <- SearchHITs(verbose = FALSE, return.qual.dataframe = FALSE, ...)
        hitlist <- hitsearch$HITs$HITId[hitsearch$HITs$RequesterAnnotation %in% annotation]
    }
    if (length(hitlist) == 0 || is.null(hitlist)) {
        stop("No HITs found for HITType")
    }
    HITs <- emptydf(length(hitlist), 4, c("HITId", "AssignmentsIncrement", "ExpirationIncrement", "Valid"))
    for (i in 1:length(hitlist)) {
        GETiteration <- paste(GETparameters, "&HITId=", hitlist[i], sep = "")
        request <- request(operation, GETparameters = GETiteration, ...)
        if (is.null(request$valid)) {
            return(request)
        }
        HITs[i, ] <- c(hitlist[i], add.assignments, add.seconds, request$valid)
        if (request$valid & verbose) {
            if (!is.null(add.assignments) & !is.null(add.seconds)) {
                message(i, ": HIT (", hitlist[i], ") Extended by ", 
                        add.assignments, " Assignments & ", add.seconds, " Seconds")
            } else if (!is.null(add.assignments)) {
                message(i, ": HIT (", hitlist[i], ") Extended by ", add.assignments, " Assignments")
            } else if(!is.null(add.seconds)) {
                message(i, ": HIT (", hitlist[i], ") Extended by ", add.seconds, " Seconds")
            }
        } else if(!request$valid & verbose) {
            warning(i, ": Invalid Request for HIT ", hitlist[i])
        }
    }
    return(HITs)
}
