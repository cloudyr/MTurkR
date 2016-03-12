SetHITAsReviewing <-
reviewing <-
function(hit = NULL, 
         hit.type = NULL, 
         annotation = NULL,
         revert = FALSE, 
         verbose = getOption('MTurkR.verbose', TRUE), ...){
    operation <- "SetHITAsReviewing"
    if (revert == TRUE) {
        revert <- "true"
    } else {
        revert <- "false"
    }
    if ((is.null(hit) & is.null(hit.type) & is.null(annotation)) | 
        (!is.null(hit) & !is.null(hit.type) & !is.null(annotation))) {
        stop("Must provide 'hit' xor 'hit.type' xor 'annotation'")
    } else if (!is.null(hit)){
        if (is.factor(hit)){
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
    GETparameters <- paste("&Revert=", revert, sep = "")
    HITs <- emptydf(length(hitlist), 3, c("HITId", "Status", "Valid"))
    for (i in 1:length(hitlist)) {
        GETiteration <- paste(GETparameters, "&HITId=", hitlist[i], sep = "")        
        request <- request(operation, GETparameters = GETiteration, ...)
        if (is.null(request$valid)) {
            return(request)
        }
        if (revert == "false") {
            status <- "Reviewing"
        } else if (revert == "true") {
            status <- "Reviewable"
        }
        HITs[i, ] <- c(hitlist[i], status, request$valid)
        if (request$valid & verbose) {
            if (revert == "false") {
                message(i, ": HIT (", hitlist[i], ") set as Reviewing")
            }
            if (revert == "true") {
                message(i, ": HIT (", hitlist[i], ") set as Reviewable")
            }
        } else if (!request$valid & verbose) {
            warning(i, ": Invalid Request for HIT ", hitlist[i])
        }
    }
    HITs$Valid <- factor(HITs$Valid, levels=c('TRUE','FALSE'))
    return(HITs)
}
