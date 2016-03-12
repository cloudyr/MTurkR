DisableHIT <-
disable <-
function(hit = NULL, 
         hit.type = NULL, 
         annotation = NULL, 
         response.group = NULL, 
         verbose = getOption('MTurkR.verbose', TRUE), 
         ...) {
    operation <- "DisableHIT"
    if (!is.null(response.group)) {
        if (any(!response.group %in% 
           c("Minimal", "HITQuestion", "HITDetail", "HITAssignmentSummary"))) { 
            stop("ResponseGroup must be in c(Minimal,HITQuestion,HITDetail,HITAssignmentSummary)")
        }
    }
    if ((is.null(hit) & is.null(hit.type) & is.null(annotation)) | 
       (!is.null(hit) & !is.null(hit.type) & !is.null(annotation))) {
        stop("Must provide 'hit' xor 'hit.type' xor 'annotation'")
    } else if(!is.null(hit)) {
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
    if (length(hitlist) == 0) {
        stop("No HITs found for HITType")
    }
    HITs <- emptydf(length(hitlist), 2, c("HITId", "Valid"))
    for (i in 1:length(hitlist)) {
        GETiteration <- paste("&HITId=", hitlist[i], sep = "")
        if (!is.null(response.group)) {
            if (length(response.group) == 1) {
                GETiteration <- paste(GETiteration, "&ResponseGroup=", 
                                      response.group, sep = "")
            } else {
                for(i in 1:length(response.group)) {
                  GETiteration <- paste(GETiteration, "&ResponseGroup", i-1,
                                        "=", response.group[i], sep = "")
                }
            }
        }        
        request <- request(operation, GETparameters = GETiteration, ...)
        if (is.null(request$valid)) {
            return(request)
        }
        if (is.null(response.group)) {
            request$ResponseGroup <- c("Minimal")
        } else {
            request$ResponseGroup <- response.group
        }
        HITs[i, ] <- c(hitlist[i], request$valid)
        if (request$valid & verbose) {
            message(i, ": HIT ", hitlist[i], " Disabled")
        } else if (!request$valid & verbose) {
            warning(i, ": Invalid Request for HIT ", hitlist[i])
        }
    }
    HITs$Valid <- factor(HITs$Valid, levels=c('TRUE','FALSE'))
    return(HITs)
}
