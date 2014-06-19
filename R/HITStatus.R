HITStatus <-
status <-
function (hit = NULL, hit.type = NULL, verbose = getOption('MTurkR.verbose'), ...){
    # temporary check for `print` argument (remove after v1.0)
    if('print' %in% names(list(...)) && is.null(verbose))
        verbose <- list(...)$print
    if((is.null(hit) & is.null(hit.type)) | (!is.null(hit) & !is.null(hit.type))) 
        stop("Must provide 'hit' xor 'hit.type'")
    hitsearch <- SearchHITs(verbose = TRUE, return.qual.dataframe = FALSE, ...)
    HITs <- hitsearch$HITs
    if(is.null(HITs))
        return(HITs) # return if NULL
    if(!is.null(hit)){
        if(is.factor(hit))
            hit <- as.character(hit)
        HITs <- HITs[grep(hit, HITs$HITId), ]
        toprint <- HITs[, c("HITId", "HITReviewStatus", "NumberOfAssignmentsPending", 
                            "NumberOfAssignmentsAvailable",
                            "NumberOfAssignmentsCompleted", "Expiration")]
        names(toprint) <- c("HITId", "Review Status", "Assignments Pending", 
                            "Assignments Available", "Assignments Completed",
                            "Expiration")
    } else if(!is.null(hit.type)) {
        if(is.factor(hit.type))
            hit <- as.character(hit.type)
        HITs <- HITs[HITs$HITTypeId %in% hit.type, ]
        if(dim(HITs)[1] == 0) {
            message("No HITs found for HITType")
            return(invisible(NULL))
        }
        toprint <- HITs[,c( "HITId","HITReviewStatus","NumberOfAssignmentsPending",
                                "NumberOfAssignmentsAvailable",
                                "NumberOfAssignmentsCompleted", "Expiration")]
        if(dim(HITs)[1] > 1) {
            totals <- data.frame(HITId = c( "------------------------------", "Totals"),
                                HITReviewStatus = c("---------------",""),
                                NumberOfAssignmentsPending = c("--------------------",
                                    sum(as.numeric(HITs$NumberOfAssignmentsPending))),
                                NumberOfAssignmentsAvailable = c("------------------",
                                    sum(as.numeric(HITs$NumberOfAssignmentsAvailable))),
                                NumberOfAssignmentsCompleted = c("--------------------",
                                    sum(as.numeric(HITs$NumberOfAssignmentsCompleted))),
                                Expiration = c("----------",""))
            toprint <- rbind(toprint,totals)
        }
    }
    print(toprint, row.names = FALSE)
    return(invisible(HITs))
}
