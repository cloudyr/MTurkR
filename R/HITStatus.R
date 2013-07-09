HITStatus <-
status <-
function (hit = NULL, hit.type = NULL, keypair = credentials(), 
    print = TRUE, log.requests = TRUE, sandbox = FALSE) 
{
    if ((is.null(hit) & is.null(hit.type)) | (!is.null(hit) & !is.null(hit.type))) 
        stop("Must provide 'hit' xor 'hit.type'")
    hitsearch <- SearchHITs(keypair = keypair, print = TRUE, 
                            log.requests = log.requests, sandbox = sandbox,
                            return.qual.dataframe = FALSE)
    if(is.null(hitsearch$HITs))
        stop()
    HITs <- hitsearch$HITs
    if (!is.null(hit))
        HITs <- HITs[grep(hit, HITs$HITId), ]
    else if (!is.null(hit.type)) {
        HITs <- HITs[HITs$HITTypeId == hit.type, ]
        if (dim(HITs)[1] == 0) 
            stop("No HITs found for HITType")
        if (dim(HITs)[1] > 1) {
            i <- dim(HITs)[1]
            HITs$HITId[i + 1] <- "------------------------------"
            HITs$NumberofAssignmentsPending[i + 1] <- "--------------------"
            HITs$NumberofAssignmentsAvailable[i + 1] <- "------------------"
            HITs$NumberofAssignmentsCompleted[i + 1] <- "--------------------"
            HITs$HITId[i + 2] <- "Totals"
            HITs$NumberofAssignmentsPending[i + 2] <- sum(as.numeric(HITs$NumberofAssignmentsAvailable[1:i]))
            HITs$NumberofAssignmentsAvailable[i + 2] <- sum(as.numeric(HITs$NumberofAssignmentsPending[1:i]))
            HITs$NumberofAssignmentsCompleted[i + 2] <- sum(as.numeric(HITs$NumberofAssignmentsCompleted[1:i]))
        }
    }
    if (print == TRUE) {
        toprint <- HITs[, c("HITId", "HITReviewStatus", "NumberofAssignmentsPending", 
            "NumberofAssignmentsAvailable", "NumberofAssignmentsCompleted", "Expiration")]
        names(toprint) <- c("HITId", "Review Status", "Assignments Pending", 
            "Assignments Available", "Assignments Completed", "Expiration")
        print(toprint, row.names = FALSE)
        message()
    }
    invisible(HITs)
}
