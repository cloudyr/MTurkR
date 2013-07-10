approveall <-
ApproveAllAssignments <-
function (hit = NULL, hit.type = NULL, feedback = NULL, keypair = credentials(), 
    print = FALSE, log.requests = TRUE, sandbox = FALSE, validation.test = FALSE) 
{
    if (!is.null(feedback)) {
        if (length(feedback) > 0) 
            stop("Can only specify one feedback message; no assignments approved")
        else if (nchar(curlEscape(feedback)) > 1024) 
            stop(paste("Feedback is too long (1024 char max); no assignments approved", sep = ""))
    }
    if (!is.null(hit) & !is.null(hit.type)) 
        stop("Must specify 'hit' xor 'hit.type'")
    if (!is.null(hit)) {
        assignments <- GetAssignments(hit = hit, return.all = TRUE, 
            keypair = keypair, log.requests = log.requests, sandbox = sandbox)$AssignmentId
    }
    else if (!is.null(hit.type)) {
        hitsearch <- SearchHITs(keypair = keypair, print = print, 
                                sandbox = sandbox, return.qual.dataframe = FALSE)
        hitlist <- hitsearch$HITs[hitsearch$HITs$HITTypeId == hit.type, ]$HITId
        if (length(hitlist) == 0) 
            stop("No HITs found for HITType")
        assignments <- c()
        for (i in 1:length(hitlist)) {
            assignments <- c(assignments, GetAssignments(hit = hitlist[i], 
                return.all = TRUE, keypair = keypair, log.requests = log.requests, 
                sandbox = sandbox)$AssignmentId)
        }
    }
    if (is.null(feedback)) {
        request <- ApproveAssignments(keypair, assignments, print = print, 
                                      log.requests = log.requests, sandbox = sandbox,
                                      validation.test = validation.test)
            invisible(request)
    }
    else{
        request <- ApproveAssignments(keypair, assignments, feedback = feedback,
                                      print = print, log.requests = log.requests, 
                                      sandbox = sandbox, validation.test = validation.test)
        invisible(request)
    }
}
