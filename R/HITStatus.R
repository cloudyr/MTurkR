HITStatus <-
status <-
function (hit = NULL, hit.type = NULL, keypair = credentials(), 
    print = TRUE, log.requests = TRUE, sandbox = getOption('MTurkR.sandbox'))
{
    if ((is.null(hit) & is.null(hit.type)) | (!is.null(hit) & !is.null(hit.type))) 
        stop("Must provide 'hit' xor 'hit.type'")
    hitsearch <- SearchHITs(keypair = keypair, print = TRUE, 
                            log.requests = log.requests, sandbox = sandbox,
                            return.qual.dataframe = FALSE)
    HITs <- hitsearch$HITs
    if(is.null(HITs))
        return(HITs) # return if NULL
    if (!is.null(hit)){
        HITs <- HITs[grep(hit, HITs$HITId), ]
        toprint <- HITs[, c("HITId", "HITReviewStatus", "NumberofAssignmentsPending", 
                            "NumberofAssignmentsAvailable",
                            "NumberofAssignmentsCompleted", "Expiration")]
        names(toprint) <- c("HITId", "Review Status", "Assignments Pending", 
                            "Assignments Available", "Assignments Completed",
                            "Expiration")
    }
    else if (!is.null(hit.type)) {
        HITs <- HITs[HITs$HITTypeId == hit.type, ]
        if (dim(HITs)[1] == 0) {
            message("No HITs found for HITType")
            invisible(HITs)
        }
        if (dim(HITs)[1] > 1) {
            toprint <- HITs[,c( "HITId","HITReviewStatus","NumberofAssignmentsPending",
                                "NumberofAssignmentsAvailable",
                                "NumberofAssignmentsCompleted", "Expiration")]
            totals <- data.frame(HITId = c( "------------------------------",
                                            "Totals"),
                                HITReviewStatus = c("---------------",""),
                                NumberofAssignmentsPending = c("--------------------",
                                    sum(as.numeric(HITs$NumberofAssignmentsPending))),
                                NumberofAssignmentsAvailable = c("------------------",
                                    sum(as.numeric(HITs$NumberofAssignmentsAvailable))),
                                NumberofAssignmentsCompleted = c("--------------------",
                                    sum(as.numeric(HITs$NumberofAssignmentsCompleted))),
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
