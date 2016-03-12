ExpireHIT <-
expire <-
function(hit = NULL, 
         hit.type = NULL, 
         annotation = NULL,
         verbose = getOption('MTurkR.verbose', TRUE), ...)
{
    operation <- "ForceExpireHIT"
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
    if (length(hitlist) == 0) {
        stop("No HITs found for HITType")
    }
    HITs <- emptydf(length(hitlist), 2, c("HITId", "Valid"))
    for (i in 1:length(hitlist)) {
        GETiteration <- paste("&HITId=", hitlist[i], sep = "")        
        request <- request(operation, GETparameters = GETiteration, ...)
        if (is.null(request$valid)) {
            return(request)
        }
        HITs[i, ] <- c(hitlist[i], request$valid)
        if (request$valid) {
            if (verbose) {
                message(i, ": HIT ", hitlist[i], " Expired")
            }
        } else if (!request$valid & verbose) {
            warning(i, ": Invalid Request for HIT ", hitlist[i])
        }
    }
    HITs$Valid <- factor(HITs$Valid, levels=c('TRUE','FALSE'))
    return(HITs)
}
