approveall <-
ApproveAllAssignments <-
function (hit = NULL, hit.type = NULL, feedback = NULL, keypair = credentials(), 
    print = getOption('MTurkR.print'), log.requests = getOption('MTurkR.log'),
    sandbox = getOption('MTurkR.sandbox'), validation.test = getOption('MTurkR.test')){
    if(!is.null(feedback)) {
        if(length(feedback) > 0) 
            stop("Can only specify one feedback message; no assignments approved")
        else if(nchar(curlEscape(feedback)) > 1024) 
            stop(paste("Feedback is too long (1024 char max); no assignments approved", sep = ""))
    }
    if(!is.null(hit) & !is.null(hit.type)) 
        stop("Must specify 'hit' xor 'hit.type'")
    if(!is.null(hit)) {
        assignments <- GetAssignments(hit = hit, return.all = TRUE, 
            keypair = keypair, log.requests = log.requests, sandbox = sandbox)$AssignmentId
    }
    else if(!is.null(hit.type)) {
        if(is.factor(hit.type))
            hit.type <- as.character(hit.type)
        hitsearch <- SearchHITs(keypair = keypair, print = print, 
                                sandbox = sandbox, return.qual.dataframe = FALSE)
        hitlist <- hitsearch$HITs$HITId[hitsearch$HITs$HITTypeId %in% hit.type]
        if(length(hitlist) == 0) 
            stop("No HITs found for HITType")
        assignments <- sapply(hitlist, function(i){
                        GetAssignments(hit = i, return.all = TRUE, keypair = keypair,
                        log.requests = log.requests, sandbox = sandbox)$AssignmentId })
    }
    request <- ApproveAssignments(keypair, assignments, feedback = feedback,
                print = print, log.requests = log.requests, 
                sandbox = sandbox, validation.test = validation.test)
    invisible(request)
}
