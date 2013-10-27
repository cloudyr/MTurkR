HITStatus <-
status <-
function (hit = NULL, hit.type = NULL, keypair = credentials(), 
    print = getOption('MTurkR.print'), log.requests = getOption('MTurkR.log'),
    sandbox = getOption('MTurkR.sandbox')){
    if((is.null(hit) & is.null(hit.type)) | (!is.null(hit) & !is.null(hit.type))) 
        stop("Must provide 'hit' xor 'hit.type'")
    hitsearch <- SearchHITs(keypair = keypair, print = TRUE, 
                            log.requests = log.requests, sandbox = sandbox,
                            return.qual.dataframe = FALSE)
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
    }
    else if(!is.null(hit.type)) {
        if(is.factor(hit.type))
            hit <- as.character(hit.type)
        HITs <- HITs[HITs$HITTypeId %in% hit.type, ]
        if(dim(HITs)[1] == 0) {
            message("No HITs found for HITType")
            invisible(HITs)
        }
        if(dim(HITs)[1] > 1) {
            toprint <- HITs[,c( "HITId","HITReviewStatus","NumberOfAssignmentsPending",
                                "NumberOfAssignmentsAvailable",
                                "NumberOfAssignmentsCompleted", "Expiration")]
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
    if(print == TRUE){
        print(toprint, row.names = FALSE)
        message()
    }
    invisible(HITs)
}
