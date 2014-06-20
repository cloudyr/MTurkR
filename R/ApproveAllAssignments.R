approveall <-
ApproveAllAssignments <-
function (hit = NULL, hit.type = NULL, feedback = NULL,
    verbose = getOption('MTurkR.verbose'), ...){
    # temporary check for `print` argument (remove after v1.0)
    if('print' %in% names(list(...)) && is.null(verbose))
        verbose <- list(...)$print
    if(!is.null(feedback)) {
        if(length(feedback) > 1) 
            stop("Can only specify one feedback message; no assignments approved")
        else if(nchar(curlEscape(feedback)) > 1024) 
            stop(paste("Feedback is too long (1024 char max); no assignments approved", sep = ""))
    }
    if(!is.null(hit) & !is.null(hit.type)) 
        stop("Must specify 'hit' xor 'hit.type'")
    if(!is.null(hit)) {
        assignments <- GetAssignments(hit = hit, 
                                      return.all = TRUE, 
                                      status = "Submitted", 
                                      verbose = verbose, ...)$AssignmentId
    } else if(!is.null(hit.type)) {
        if(is.factor(hit.type))
            hit.type <- as.character(hit.type)
        hitsearch <- SearchHITs(return.qual.dataframe = FALSE, ...)
        hitlist <- hitsearch$HITs$HITId[hitsearch$HITs$HITTypeId %in% hit.type]
        if(length(hitlist) == 0) 
            stop("No HITs found for HITType")
        assignments <- sapply(hitlist, function(i){
                           GetAssignments(hit = i, 
                                          return.all = TRUE, 
                                          status = "Submitted", 
                                          verbose = verbose,  ...)$AssignmentId
                       })
    }
    if(length(assignments)==0){
        return(setNames(data.frame(matrix(nrow=0, ncol=3)),
                        c("AssignmentId","Feedback","Valid")))
    } else {
        request <- ApproveAssignments(assignments,
                                      feedback = feedback, 
                                      verbose = verbose, ...)
        return(request)
    }
}
